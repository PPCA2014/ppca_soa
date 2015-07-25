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
			msbus_logger:info("Escutando na porta ~p.", [Port]),
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
			msbus_logger:info("Parou de escutar na porta ~p.", [Port]),
			Reply = {ok, NewState};
		_ -> 
			Reply = {{error, enolisten}, State}
	end,
	Reply.

start_server(Port) ->
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
     {ok, Request} ->
		case trata_request(Request) of
			{ok, Response} ->
				gen_tcp:send(Socket, [Response]),
				log_status_requisicao(Request, "OK");
			{error, Response, ErroInterno} ->
				gen_tcp:send(Socket, [Response]),
				log_status_requisicao(Request, ErroInterno)
		end;
     {error, Request, Reason} -> 
		%% Foi possível ler o cabecalho mas o payload não é JSON
		Response = encode_response(<<"415">>, ?HTTP_ERROR_415),
		gen_tcp:send(Socket, [Response]),
		log_status_requisicao(Request, Reason);
     {error, Reason} -> 
		%% Requisição inválida.
		Response = encode_response(<<"400">>, ?HTTP_ERROR_400),
		gen_tcp:send(Socket, [Response]),
		logger:error("Ocorreu um erro que foi ingnorado sem traumas: ~s.", [Reason])
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
					RID = {now(), node()},
					{Metodo, Url, Versao_HTTP, Querystring, QuerystringMap, HeaderMap} = get_http_header(Header),
					Request = msbus_request:encode_request(RID, Metodo, Url, Versao_HTTP, Querystring, QuerystringMap, HeaderMap),
					case possui_payload(Request) of
						false ->
							% Requisições GET e DELETE ou POST e PUT sem payload (algum sentido nisso?!)
							{ok, Request};
						true ->
							% Requisições POST e PUT
							case get_request_payload(Socket, Request#request.content_length, Payload) of
								{ok , PayloadText, PayloadMap} ->
									Request1 = Request#request{payload = PayloadText, payload_map = PayloadMap},
									{ok, Request1};
								{error, Reason} -> {error, Request, Reason}
							end
					end
			end;
		{tcp_closed, Socket} ->
		    {error, invalid_request};
		_Any  ->
		    get_request(Socket, L)
    end.

%% @doc Retorna boolean indicando se possui conforme as regras RESTfull
possui_payload(Request) when Request#request.metodo =:= "GET"; 
							 Request#request.metodo =:= "DELETE" -> false;
possui_payload(Request) when Request#request.metodo =:= "POST"; 
							 Request#request.metodo =:= "PUT" -> 
	Request#request.content_length > 0.

get_request_payload(Socket, Content_Length, L) when length(L) /= Content_Length ->
    receive
		{tcp, Socket, Bin} -> 
			L1 = L ++ binary_to_list(Bin),
			get_request_payload(Socket, Content_Length, L1);
		{tcp_closed, Socket} ->
		    {error, tcp_closed};
		_Any  ->
		    get_request_payload(Socket, Content_Length, L)
    end;
    
get_request_payload(_Socket, _Content_Length, L) ->    
	Payload = list_to_binary(L),
	case decode_payload(Payload) of
		{ok, PayloadMap} -> {ok, L, PayloadMap};
		Error -> Error
	end. 

-spec get_http_header(Header::list()) -> maps:map().
get_http_header(Header) ->
	[Principal|Outros] = string:tokens(Header, "\r\n"),
	[Metodo|[Url|[Versao_HTTP|_]]] = string:tokens(Principal, " "),
	[Url2|QueryString] = string:tokens(Url, "?"),
	Url3 = msbus_util:remove_ult_backslash_url(Url2),
	Outros2 = get_http_header_adicionais(Outros),
	QueryStringMap = parse_querystring(QueryString),
    {Metodo, Url3, Versao_HTTP, QueryString, QueryStringMap, Outros2}.

get_http_header_adicionais(Header) ->
	Header1 = string:to_lower(Header),
	Header2 = map(fun(P) -> string:tokens(P, ":") end, Header1),
	Header3 = [{P, format_header_value(P, V)} || [P|[V]] <- Header2, is_valid_header(P)],
	maps:from_list(Header3).

%% @doc Trata o request e retorna o response do resultado
trata_request(Request) ->
	RID = Request#request.rid,	
	Url = Request#request.url,
	Metodo = Request#request.metodo,
	msbus_dispatcher:dispatch_request(Request, self()),
	msbus_health:collect(RID, request_submit, {Url, Metodo}),
	receive
		{ok, Result} ->
			msbus_health:collect(RID, request_success, {}),
			Response = encode_response(<<"200">>, Result),
			{ok, Response};
		{ok, Result, MimeType} ->
			msbus_health:collect(RID, request_success, MimeType),
			Response = encode_response(<<"200">>, Result, MimeType),
			{ok, Response};
		{error, servico_nao_encontrado, ErroInterno} ->
			msbus_health:collect(RID, request_error, {<<"404">>, servico_nao_encontrado}),
			Response = encode_response(<<"404">>, ?HTTP_ERROR_404),
			{error, Response, ErroInterno};
		{error, servico_falhou, ErroInterno} ->
			msbus_health:collect(RID, request_error, {<<"502">>, servico_falhou, ErroInterno}),
			Response = encode_response(<<"502">>, ?HTTP_ERROR_502(ErroInterno)),
			{error, Response, ErroInterno};
		{error, servico_nao_disponivel, ErroInterno} ->
			msbus_health:collect(RID, request_error, {<<"503">>, servico_nao_disponivel}),
			Response = encode_response(<<"503">>, ?HTTP_ERROR_503),
			{error, Response, ErroInterno};
		{error, file_not_found} ->
			msbus_health:collect(RID, request_error, {<<"404">>, file_not_found}),
			Response = encode_response(<<"404">>, ?HTTP_ERROR_404_FILE_NOT_FOUND),
			{error, Response, file_not_found}
	end.

%% @doc Decodifica o payload e transforma em um tipo Erlang
decode_payload([]) ->
	{ok, #{}};

%% @doc Decodifica o payload e transforma em um tipo Erlang
decode_payload(Payload) ->
	case msbus_util:json_decode_as_map(Payload) of
		{ok, PayloadMap} -> {ok, PayloadMap};
		{error, _Reason} -> {error, invalid_payload}
	end.

%% @doc Gera o response para enviar para o cliente
encode_response(<<Codigo/binary>>, <<Payload/binary>>, <<MimeType/binary>>) ->
	PayloadLength = list_to_binary(integer_to_list(size(Payload))),
	Response = [<<"HTTP/1.1 ">>, Codigo, <<" OK">>, <<"\n">>,
				<<"Server: ">>, ?SERVER_NAME, <<"\n">>,
				<<"Content-Type: ">>, MimeType, <<"\n">>,
				<<"Content-Length: ">>, PayloadLength, <<"\n">>,
				<<"Access-Control-Allow-Origin: *\n">>,
				<<"Access-Control-Allow-Methods: GET, PUT, POST, DELETE\n">>,
				<<"Access-Control-Allow-Headers: Content-Type, Content-Range, Content-Disposition, Content-Description\n">>,
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

%% @doc Verifica se o header é útil para erlangMS
is_valid_header("content-length") -> true;
is_valid_header("content-type") -> true;
is_valid_header("accept") -> true;
is_valid_header("accept-encoding") -> true;
%%is_valid_header("accept-language") -> true;
is_valid_header("user-agent") -> true;
%is_valid_header("cache-control") -> true;
is_valid_header(_) -> false.

%% @doc formata o valor do header (String, Integer)
format_header_value("content-length", Value) ->
	Value1 = string:strip(Value),
	Value2 = list_to_integer(Value1),
	case is_content_length_valido(Value2) of
		true -> Value2;
		false -> 0
	end;

format_header_value(_, Value) -> 
	string:strip(Value).

parse_querystring([]) -> #{};
parse_querystring([Querystring]) ->
	Q1 = string:tokens(Querystring, "&"),
	Q2 = lists:map(fun(P) -> string:tokens(P, "=") end, Q1),
	Q3 = lists:map(fun([P|V]) -> {iolist_to_binary(P), msbus_util:hd_or_empty(V)} end, Q2),
	maps:from_list(Q3).

log_status_requisicao(Request, Status) ->
	HeaderMap = Request#request.http_headers,
	Metodo = Request#request.metodo,
	Url = Request#request.url,
	HTTPVersion = Request#request.versao_http,
	Payload = Request#request.payload,
	case Payload of
		undefined ->
			Texto = [[io_lib:format("\t~s:  ~p\n", [P, maps:get(P, HeaderMap)]) || P <- maps:keys(HeaderMap)] | 
				[io_lib:format("\tstatus: ~s\n}", [Status])]];
		_ ->
			Texto = [[io_lib:format("\t~s:  ~p\n", [P, maps:get(P, HeaderMap)]) || P <- maps:keys(HeaderMap)] | 
				[io_lib:format("\tpayload: ~s\n\tstatus: ~s\n}", [Payload, Status])]]
	end,
	Texto1 = [io_lib:format("~s ~s ~s {\n", [Metodo, Url, HTTPVersion]) | Texto],
	msbus_logger:info(Texto1). 
	

	
