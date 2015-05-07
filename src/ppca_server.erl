%% ---
%%  PPCA_SOA
%%  Implementa um servidor web capaz de atender múltiplas requisições
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---
-module(ppca_server).

-include("../include/ppca_config.hrl").

-export([init/0, is_content_length_valido/1]).
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
				{error, _Reason} ->
					gen_tcp:send(Socket, [error_415_invalid_media_type()]);
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
	PayloadJson = list_to_binary(L),
	Result = ppca_util:json_decode(PayloadJson),
	Result.

-spec get_http_header(Header::list()) -> dict:dict().
get_http_header(Header) ->
	[Principal|Outros] = string:tokens(Header, "\r\n"),
	[Metodo|[Url|[Versao_HTTP|_]]] = string:tokens(Principal, " "),
	[Url2|QueryString] = string:tokens(Url, "?"),
	Outros2 = get_http_header_adicionais(Outros),
	QueryString2 = parse_query_string(QueryString),
	dict:from_list([{"Metodo", Metodo}, 
					{"Url", Url2}, 
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

trata_request(RequestHandler, HeaderDict, Payload) ->
	print_requisicao_debug(HeaderDict, Payload),
	{Codigo, Result} = processa_request(RequestHandler, HeaderDict, Payload),
	response(Codigo, Result).
	%case dict:fetch("Url", HeaderDict) of
	%	"faveicon.ico" -> response(200,"")		
	%end.
	

processa_request(RequestHandler, HeaderDict, Payload) ->
	RequestHandler ! {self(), { processa_request, {HeaderDict, Payload}}},
	receive
		{ ok, Response } -> { 200, Response };
		{ Erro, Reason } -> { Erro, Reason }
	end.

response(Codigo, Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.1 ~p OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [Codigo, size(B), B])).

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

error_415_invalid_media_type() ->
	Response = response(415, "{\"error\":\"415\",\"message\":\"HTTP ERROR 415 - Unsupported Media Type: Tipos dados suportado: JSON\"}"),
	Response.
	
print_requisicao_debug(HeaderDict, Payload) ->
	ppca_logger:info_msg("~s ~s", [dict:fetch("Metodo", HeaderDict), dict:fetch("Url", HeaderDict)]),    
	ppca_logger:info_msg("Header:", []),
	[ ppca_logger:info_msg("\t~s:  ~p", [P, dict:fetch(P, HeaderDict)]) || P <- dict:fetch_keys(HeaderDict)],
	print_requisicao_payload_debug(Payload).

print_requisicao_payload_debug([]) ->
	ok;

print_requisicao_payload_debug(Payload) ->
	ppca_logger:info_msg("Payload: ~s", [ppca_util:json_encode(Payload)]).
	
	
	
