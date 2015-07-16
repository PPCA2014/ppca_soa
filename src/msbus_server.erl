%%********************************************************************
%% @title Servidor HTTP
%% @version 1.0.0
%% @doc Módulo responsável pelo processamento das requisições HTTP.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_server).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_http_messages.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([start_listen/2, stop_listen/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-import(string, [tokens/2]).
-import(lists, [reverse/1, map/2, filter/2]).

% estado do servidor
-record(state, {listener=[]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================
 
start_listen(Port, From) ->
	gen_server:call(?SERVER, {start_listen, Port, From}).

stop_listen(Port, From) ->
	gen_server:call(?SERVER, {stop_listen, Port, From}).
	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	case do_start_listen(?CONF_PORT, #state{}) of
		{ok, NewState} -> {ok, NewState};
		{{error, _Reason}, NewState} -> {error, NewState}
	end.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({start_listen, Port, From}, State) ->
	{Reply, NewState} = do_start_listen(Port, State),
	From ! Reply, 
	{noreply, NewState};
    
handle_cast({stop_listen, Port, From}, State) ->
	{Reply, NewState} = do_stop_listen(Port, State),
	From ! Reply, 
	{noreply, NewState}.

handle_call({start_listen, Port}, _From, State) ->
	{Reply, NewState} = do_start_listen(Port, State),
	{reply, Reply, NewState}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

do_start_listen(Port, State) ->
	case start_server(Port) of
		{ok, Listen} ->
			NewState = State#state{listener=[{Listen, Port}|State#state.listener]},
			msbus_logger:info("Escutando na porta ~p~n", [Port]),
			Reply = {ok, NewState};
		{error, Reason} ->
			Reply = {{error, Reason}, State}
	end,
	Reply.

do_stop_listen(Port, State) ->
	case [ S || {S,P} <- State#state.listener, P == Port] of
		[Listen|_] ->
			stop_server(Listen),
			NewState = State#state{listener=lists:delete({Listen, Port}, State#state.listener)},
			msbus_logger:info("Parou de escutar na porta ~p~n", [Port]),
			Reply = {ok, NewState};
		_ -> 
			Reply = {{error, enolisten}, State}
	end,
	Reply.

start_server(Port) ->
	% Usando a operação gen_tcp:listen do OTP, informando a porta 
	% e mais alguns dados de configuração. Depois vamos otimizar as 
	% opções de configuração. 
	case gen_tcp:listen(Port, [binary, {packet, 0}, 
								{reuseaddr, true},
								{active, true}]) of
		{ok, Listen} ->
		    spawn(fun() -> aceita_conexoes(Listen) end),
			{ok, Listen};
		{error, Reason} -> 
			{error, Reason} 
	end.	

stop_server(Listen) ->
	gen_tcp:close(Listen).

aceita_conexoes(Listen) ->
 	case gen_tcp:accept(Listen) of
		{ok, Socket} -> processa_conexao(Listen, Socket);
		{error, closed} -> ok
	end.

processa_conexao(Listen, Socket) -> 
  spawn(fun() -> aceita_conexoes(Listen) end),
  inet:setopts(Socket, [{packet,0},binary, {nodelay,true},{active, true}]),
  case get_request(Socket, []) of
     {ok, HeaderDict, Payload} ->
		Response = trata_request(HeaderDict, Payload),
		gen_tcp:send(Socket, [Response]);
     tcp_closed -> ok
  end.	
	
get_request(Socket, L) ->
    receive
		{tcp, Socket, Bin} -> 
			L1 = L ++ binary_to_list(Bin),
			%% is_fim_header verifica quando o cabeçalho está completo
			case is_fim_header(L1, []) of
				more ->
					%% o cabeçalho está incompleto e precisa mais dados
					get_request(Socket, L1);
				{Header, Payload} ->
					%% cabeçalho completo
					HeaderDict = get_http_header(Header),
					case dict:find("Content-Length", HeaderDict) of
						{ok, 0} ->
							{ok, HeaderDict, ""};
						{ok, Content_Length} ->
							case get_request_payload(Socket, Content_Length, Payload) of
								{ok , Payload1} ->
									{ok, HeaderDict, Payload1};
								{error, Reason} ->
									{error, Reason}
							end;
						error -> 
							% Tudo ok, somente POST e PUT possuem payload
							{ok, HeaderDict, ""}
					end
			end;
		{tcp_closed, Socket} ->
		    tcp_closed;
		_Any  ->
		    get_request(Socket, L)
    end.

get_request_payload(Socket, Content_Length, L) when length(L) /= Content_Length ->
    receive
		{tcp, Socket, Bin} -> 
			L1 = L ++ binary_to_list(Bin),
			get_request_payload(Socket, Content_Length, L1);
		{tcp_closed, Socket} ->
		    [];
		_Any  ->
		    get_request_payload(Socket, Content_Length, L)
    end;
    
get_request_payload(_Socket, _Content_Length, L) ->    
	Payload = list_to_binary(L),
	{ok, Payload}.

-spec get_http_header(Header::list()) -> dict:dict().
get_http_header(Header) ->
	[Principal|Outros] = string:tokens(Header, "\r\n"),
	[Metodo|[Url|[Versao_HTTP|_]]] = string:tokens(Principal, " "),
	[Url2|QueryString] = string:tokens(Url, "?"),
	Url3 = remove_ult_backslash_url(Url2),
	Outros2 = get_http_header_adicionais(Outros),
	QueryString2 = parse_query_string(QueryString),
	dict:from_list([{"Metodo", Metodo}, 
					{"Url", Url3}, 
					{"HTTP-Version", Versao_HTTP},
					{"Query", QueryString2}]
					++ Outros2).

get_http_header_adicionais(Header) ->
	Header2 = map(fun(P) -> string:tokens(P, ":") end, Header),
	Header3 = [{P, format_header_value(P, V)} || [P|[V]] <- Header2],
	Header3.

format_header_value("Content-Length", Value) ->
	Value1 = string:strip(Value),
	Value2 = list_to_integer(Value1),
	case is_content_length_valido(Value2) of
		true -> Value2;
		false -> 0
	end;

format_header_value(_, Value) -> 
	string:strip(Value).

%% @doc Trata o request e retorna o response do resultado
trata_request_map(HeaderDict, PayloadMap) ->
	msbus_dispatcher:dispatch_request(self(), HeaderDict, PayloadMap),
	receive
		{ok, Result} ->
			Response = encode_response(<<"200">>, Result),
			{ok, Response};
		{ok, Result, MimeType} ->
			Response = encode_response(<<"200">>, Result, MimeType),
			{ok, Response};
		{error, servico_nao_encontrado, ErroInterno} ->
			Response = encode_response(<<"404">>, ?HTTP_ERROR_404),
			{error, Response, ErroInterno};
		{error, servico_falhou, ErroInterno} ->
			Response = encode_response(<<"502">>, ?HTTP_ERROR_502(ErroInterno)),
			{error, Response, ErroInterno};
		{error, servico_nao_disponivel, ErroInterno} ->
			Response = encode_response(<<"503">>, ?HTTP_ERROR_503),
			{error, Response, ErroInterno};
		{error, file_not_found, ErroInterno} ->
			Response = encode_response(<<"404">>, ?HTTP_ERROR_404_FILE_NOT_FOUND),
			{error, Response, ErroInterno}
	end.

%% @doc Trata o request e retorna o response do resultado
trata_request(HeaderDict, PayloadJSON) ->
	print_requisicao_debug(HeaderDict, PayloadJSON),
	case decode_payload(PayloadJSON) of
		{ok, PayloadMap} ->
			case trata_request_map(HeaderDict, PayloadMap) of
				{ok, Response} ->
					msbus_logger:info("Serviço atendido."),
					Response;
				{error, Response, ErroInterno} ->
					msbus_logger:error(ErroInterno),
					Response
			end;
		invalid_payload ->
			encode_response(<<"415">>, ?HTTP_ERROR_415)
	end.

%% @doc Decodifica o payload e transforma em um tipo Erlang
decode_payload([]) ->
	{ok, #{}};

%% @doc Decodifica o payload e transforma em um tipo Erlang
decode_payload(Payload) ->
	case msbus_util:json_decode_as_map(Payload) of
		{ok, PayloadJSON} -> {ok, PayloadJSON};
		{error, _Reason} -> invalid_payload
	end.

%% @doc Gera o response para enviar para o cliente
encode_response(<<Codigo/binary>>, <<Payload/binary>>, <<MimeType/binary>>) ->
	PayloadLength = list_to_binary(integer_to_list(size(Payload))),
	Response = [<<"HTTP/1.1 ">>, Codigo, <<" OK">>, <<"\n">>,
				<<"Server: ">>, ?SERVER_NAME, <<"\n">>,
				<<"Content-Type: ">>, MimeType, <<"\n">>,
				<<"Content-Length: ">>, PayloadLength, <<"\n">>,
				header_cache_control(MimeType),
				<<"\n\n">>, 
	            Payload],
	Response2 = iolist_to_binary(Response),
	Response2.

encode_response(Codigo, []) ->
	encode_response(Codigo, <<>>);
	
%% @doc Gera o response para dados binário
encode_response(<<Codigo/binary>>, <<Payload/binary>>) ->
	encode_response(Codigo, Payload, <<"application/json">>);

%% @doc Gera o response para dados Map (representação JSON em Erlang)
encode_response(Codigo, PayloadMap) when is_map(PayloadMap) ->
    Payload = msbus_util:json_encode(PayloadMap),
    encode_response(Codigo, Payload);

%% @doc Gera o response para dados list (representação JSON em Erlang)
encode_response(Codigo, [H|_] = PayloadList) when is_map(H) ->
    Payload = msbus_util:json_encode(PayloadList),
    encode_response(Codigo, Payload);

encode_response(Codigo, PayloadTuple) when is_tuple(PayloadTuple) ->
    Payload = msbus_util:json_encode(PayloadTuple),
    encode_response(Codigo, Payload);

%% @doc Gera o response para dados texto
encode_response(Codigo, Payload) ->
    Payload2 = msbus_util:json_encode(Payload),
    encode_response(Codigo, Payload2).

header_cache_control(<<"image/x-icon">>) ->
	<<"Cache-Control: max-age=290304000, public">>;

header_cache_control(<<_MimeType/binary>>) ->
	<<"Cache-Control: no-cache">>.

is_fim_header("\r\n\r\n" ++ T, L) -> {lists:reverse(L), T};
is_fim_header([H|T], L)           -> is_fim_header(T, [H|L]);
is_fim_header([], _)              -> more.

is_content_length_valido(N) when N < 0; N > ?HTTP_MAX_POST_SIZE -> false;
is_content_length_valido(_) -> true.

parse_query_string([]) ->
	[];
	
parse_query_string([Querystring]) ->
	Q1 = string:tokens(Querystring, "&"),
	Q2 = lists:map(fun(P) -> string:tokens(P, "=") end, Q1),
	Q3 = lists:map(fun([P|V]) -> {P, msbus_util:hd_or_empty(V)} end, Q2),
	Q3.

%% @doc Remove o último backslash da Url
remove_ult_backslash_url("/") -> "/";
remove_ult_backslash_url(Url) -> 
	case lists:suffix("/", Url) of
		true -> lists:droplast(Url);
		false -> Url
	end.

print_requisicao_debug(HeaderDict, Payload) ->
	msbus_logger:info("~s ~s", [dict:fetch("Metodo", HeaderDict), dict:fetch("Url", HeaderDict)]),    
	msbus_logger:info("Header:", []),
	[ msbus_logger:info("\t~s:  ~p", [P, dict:fetch(P, HeaderDict)]) || P <- dict:fetch_keys(HeaderDict)],
	print_requisicao_payload_debug(Payload).

print_requisicao_payload_debug([]) ->
	ok;

print_requisicao_payload_debug(Payload) ->
	msbus_logger:info("Payload: ~s", [Payload]).
	
	
	
