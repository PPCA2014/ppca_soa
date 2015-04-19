%% ---
%%  PPCA_SOA
%%  Implementa um servidor web capaz de atender múltiplas requisições
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---
-module(ppca_server).


-export([init/0]).
-import(string, [tokens/2]).
-import(lists, [reverse/1, map/2]).


init() ->
	io:format("Módulo ppca_server carregado.~n"),
	loop().


loop() ->
	receive
		{ Client, { start_listen, Port } } ->
 			case start_server(Port) of
				ok ->
					Client ! {self(), ok},
					%ppca_util:sleep(infinity);
					loop();
				{error, Reason} ->
					Client ! {self(), {error, Reason}}
			end;
			
		{ Client, { stop_listen, _Port } } ->				
			Client ! {self(), ok}
	end,
	loop().



start_server(Port) ->
	case gen_tcp:listen(Port, [binary, {packet, 0}, 
								{reuseaddr, true},
								{active, true}]) of
		{ok, Listen} ->
		    RequestHandler = spawn(ppca_request, init, []),
		    spawn(fun() -> aceita_conexoes(Listen, RequestHandler) end),
			ok;
		{error, Reason} -> 
			{error, Reason} 
	end.	



aceita_conexoes(Listen, RequestHandler) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	io:format("Nova requisição em ~p.~n", [erlang:localtime()]),    
	spawn(fun() -> aceita_conexoes(Listen, RequestHandler) end),
	inet:setopts(Socket, [{packet,0},binary, {nodelay,true},{active, true}]),
	{Header, Payload} = get_request(Socket, []),
	Response = trata_request(RequestHandler, Header, Payload),
	gen_tcp:send(Socket, [Response]).
    

	
get_request(Socket, L) ->
    receive
		{tcp, Socket, Bin} -> 
			L1 = L ++ binary_to_list(Bin),
			%% split verifica quando o cabeçalho está completo
			case split(L1, []) of
				more ->
					%% o cabeçalho está incompleto e precisa mais dados
					get_request(Socket, L1);
				{Header, Payload} ->
					%% cabeçalho completo
					{Header, Payload}
			end;

		{tcp_closed, Socket} ->
		    void;

		_Any  ->
		    %% skip this
		    get_request(Socket, L)
    end.



split("\r\n\r\n" ++ T, L) -> {lists:reverse(L), T};
split([H|T], L)           -> split(T, [H|L]);
split([], _)              -> more.



trata_request(RequestHandler, Header, Payload) ->
	ListaHeader = string:tokens(Header, "\r\n"),
	io:format("Header: ~p~n", [ListaHeader]),
	io:format("Payload: ~p~n", [Payload]),
	[Metodo|[Url|_]]= string:tokens(hd(ListaHeader), " "),
	{Codigo, Corpo} = processa_request(RequestHandler, Metodo, Url, Payload),
	response(Codigo, Corpo).
   
 

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
         "HTTP/1.0 ~p OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [Codigo, size(B), B])).



