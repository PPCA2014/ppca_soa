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
	ppca_logger:info_msg("ppca_server carregado."),
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
			{Header, Payload} = get_request(Socket, []),
			Response = trata_request(RequestHandler, Header, Payload),
			gen_tcp:send(Socket, [Response]);
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
							{HeaderDict, ""};
						{ok, Content_Length} when length(Payload) == Content_Length ->
							{HeaderDict, Payload};
						{ok, Content_Length} ->
							{HeaderDict, get_request_payload(Socket, Content_Length, Payload)};
						error -> 
							{HeaderDict, ""}
					end
			end;
		{tcp_closed, Socket} ->
		    void;
		_Any  ->
		    get_request(Socket, L)
    end.

get_request_payload(Socket, Length, L) ->
    receive
		{tcp, Socket, Bin} -> 
				L1 = L ++ binary_to_list(Bin),
				if 
					length(L1) == Length -> 
						L1;
					true -> 
						get_request_payload(Socket, Length, L1)
				end;
		{tcp_closed, Socket} ->
		    void;
		_Any  ->
		    get_request_payload(Socket, Length, L)
    end.

-spec get_http_header(Header::list()) -> dict:dict().
get_http_header(Header) ->
	[Principal|Outros] = string:tokens(Header, "\r\n"),
	[Metodo|[Url|[Versao_HTTP|_]]] = string:tokens(Principal, " "),
	[Url2|QueryString] = string:tokens(Url, "?"),
	Outros2 = get_http_header_adicionais(Outros),
	ppca_logger:info_msg(QueryString),
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

trata_request(RequestHandler, Header, Payload) ->
	print_requisicao_debug(Header, Payload),
	{Codigo, Corpo} = processa_request(RequestHandler, "Metodo", "Url", Payload),
	response(Codigo, Corpo).

print_requisicao_debug(Header, Payload) ->
	ppca_logger:info_msg("~s ~s", [dict:fetch("Metodo", Header), dict:fetch("Url", Header)]),    
	ppca_logger:info_msg("Header:", []),
	[ ppca_logger:info_msg("\t~s:  ~p", [P, dict:fetch(P, Header)]) || P <- dict:fetch_keys(Header)],
	ppca_logger:info_msg("Payload: ~p", [Payload]).

processa_request(RequestHandler, Metodo, Url, Payload) ->
	RequestHandler ! {self(), { processa_request, {Metodo, Url, Payload}}},
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
