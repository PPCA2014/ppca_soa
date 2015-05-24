%% ---
%%  PPCA_SERVER
%%  Servidor HTTP para atender solicitações REST.
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Alunos: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%          Eliene do Carmo Vieira	 (elienev@gmail.com) 
%%          Celson Junior			 (celson.jr@gmail.com)
%%          Raphael Magalhães Hoed	 (raphael.hoed@gmail.com)
%%          Felipe Fonseca			 (fellipe.alves@gmail.com)
%%---
-module(ppca_server).

-include("../include/ppca_config.hrl").
-include("../include/http_messages.hrl").

-export([init/0, is_content_length_valido/1, remove_ult_backslash_url/1]).
-import(string, [tokens/2]).
-import(lists, [reverse/1, map/2, filter/2]).

% Record o estado do servidor
-record(state, {listener=[]}).

init() ->
	ppca_logger:info_msg("ppca_server iniciado."),
	loop(#state{}).

loop(State) ->
	receive
		{ From, { start_listen, Port } } ->
 			case start_server(Port) of
				{ok, Listen} ->
					From ! ok,
					State1 = State#state{listener=[{Listen, Port}|State#state.listener]},
					loop(State1);
				{error, Reason} ->
					From ! {error, Reason}
			end;
		{From, {stop_listen, Port}} ->				
			case [ S || {S,P} <- State#state.listener, P == Port] of
				[Listen|_] ->
					stop_server(Listen),
					State1 = State#state{listener=lists:delete({Listen, Port}, State#state.listener)},
					loop(State1),
					From ! ok;
				_ -> 
					From ! {error, enolisten}
			end
	end,
	loop(State).

start_server(Port) ->
	% Usando a operação gen_tcp:listen do OTP, informando a porta 
	% e mais alguns dados de configuração. Depois vamos otimizar as 
	% opções de configuração. 
	case gen_tcp:listen(Port, [binary, {packet, 0}, 
								{reuseaddr, true},
								{active, true}]) of
		{ok, Listen} ->
		    RequestHandler = spawn(ppca_request, init, []),
		    spawn(fun() -> aceita_conexoes(Listen, RequestHandler) end),
			{ok, Listen};
		{error, Reason} -> 
			{error, Reason} 
	end.	

stop_server(Listen) ->
	gen_tcp:close(Listen).

aceita_conexoes(Listen, RequestHandler) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} -> 
			spawn(fun() -> aceita_conexoes(Listen, RequestHandler) end),
			inet:setopts(Socket, [{packet,0},binary, {nodelay,true},{active, true}]),
			case get_request(Socket, []) of
				{ok, HeaderDict, Payload} ->
					Response = trata_request(RequestHandler, HeaderDict, Payload),
					gen_tcp:send(Socket, [Response]);
				tcp_closed ->
					ok
			end;
		{error, closed} -> 
			ok
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
trata_request_map(RequestHandler, HeaderDict, PayloadMap) ->
	RequestHandler ! {self(), {processa_request, {HeaderDict, PayloadMap}}},
	receive
		{ok, Resposta} ->
			Response = encode_response(<<"200">>, Resposta),
			{ok, Response};
		{error, servico_nao_encontrado, ErroInterno} ->
			Response = encode_response(<<"404">>, ?HTTP_ERROR_404),
			{error, Response, ErroInterno};
		{error, servico_falhou, ErroInterno} ->
			Response = encode_response(<<"502">>, ?HTTP_ERROR_502(ErroInterno)),
			{error, Response, ErroInterno};
		{error, servico_nao_disponivel, ErroInterno} ->
			Response = encode_response(<<"503">>, ?HTTP_ERROR_503),
			{error, Response, ErroInterno}
	end.

%% @doc Trata o request e retorna o response do resultado
trata_request(RequestHandler, HeaderDict, PayloadJSON) ->
	print_requisicao_debug(HeaderDict, PayloadJSON),
	case decode_payload(PayloadJSON) of
		{ok, PayloadMap} ->
			case trata_request_map(RequestHandler, HeaderDict, PayloadMap) of
				{ok, Response} ->
					ppca_logger:info_msg("Serviço atendido."),
					Response;
				{error, Response, ErroInterno} ->
					ppca_logger:error_msg(ErroInterno),
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
	case ppca_util:json_decode(Payload) of
		{ok, PayloadJSON} -> {ok, PayloadJSON};
		{error, _Reason} -> invalid_payload
	end.

%% @doc Gera o response para enviar para o cliente
encode_response(<<Codigo/binary>>, <<Payload/binary>>) ->
	PayloadLength = list_to_binary(integer_to_list(size(Payload))),
	Response = [<<"HTTP/1.1 ">>, Codigo, <<" OK">>, <<"\n">>,
				<<"Server: ">>, ?SERVER_NAME, <<"\n">>,
				<<"Content-Type: application/json">>, <<"\n">>,
				<<"Content-Length: ">>, PayloadLength, <<"\n\n">>, 
	            Payload],
	Response2 = iolist_to_binary(Response),
	Response2;

%% @doc Gera o response para enviar para o cliente
encode_response(Codigo, PayloadMap) when is_map(PayloadMap) ->
    Payload = ppca_util:json_encode(PayloadMap),
    encode_response(Codigo, Payload);

%% @doc Gera o response para enviar para o cliente
encode_response(Codigo, PayloadStr) ->
    PayloadBin = iolist_to_binary(PayloadStr),
    encode_response(Codigo, PayloadBin).

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
	Q3 = lists:map(fun([P|V]) -> {P, ppca_util:hd_or_empty(V)} end, Q2),
	Q3.

%% @doc Remove o último backslash da Url
remove_ult_backslash_url("/") -> "/";
remove_ult_backslash_url(Url) -> remove_ult_backslash_url2(lists:reverse(Url)).
remove_ult_backslash_url2("/" ++ T) -> lists:reverse(T);
remove_ult_backslash_url2(Url) -> lists:reverse(Url).


print_requisicao_debug(HeaderDict, Payload) ->
	ppca_logger:info_msg("~s ~s", [dict:fetch("Metodo", HeaderDict), dict:fetch("Url", HeaderDict)]),    
	ppca_logger:info_msg("Header:", []),
	[ ppca_logger:info_msg("\t~s:  ~p", [P, dict:fetch(P, HeaderDict)]) || P <- dict:fetch_keys(HeaderDict)],
	print_requisicao_payload_debug(Payload).

print_requisicao_payload_debug([]) ->
	ok;

print_requisicao_payload_debug(Payload) ->
	ppca_logger:info_msg("Payload: ~s", [Payload]).
	
	
	
