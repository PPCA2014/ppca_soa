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
				{ok, Reason} ->
					Client ! {self(), {ok, Reason}},
					ppca_util:sleep(infinity);
				{error, Reason} ->
					Client ! {self(), {error, Reason}}
			end
	end,
	loop().


start_server(Port) ->
	case gen_tcp:listen(Port, [binary, {packet, 0},
  						     		   {reuseaddr, true},
			    					   {active, true}]) of
		{ok, Listen} ->
		    RequestHandler = spawn(ppca_request, init, []),
		    spawn(fun() -> aceita_conexoes(Listen, RequestHandler) end),
			{ok, "Listener iniciado"};
		{error, Reason} -> 
			{error, Reason} 
	end.	


aceita_conexoes(Listen, RequestHandler) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	io:format("Nova requisição.~n", []),    
    spawn(fun() -> aceita_conexoes(Listen, RequestHandler) end),
    inet:setopts(Socket, [{packet,0},binary, {nodelay,true},{active, true}]),
    get_request(Socket, RequestHandler, []).



get_request(Socket, RequestHandler, L) ->
    receive
		{tcp, Socket, Bin} -> 
			io:format("get_request.~n"),
			L1 = L ++ binary_to_list(Bin),
			%% split checks if the header is complete
			case split(L1, []) of
				more ->
					%% the header is incomplete we need more data
					get_request(Socket, RequestHandler, L1);
				{Request, _Rest} ->
					%% header is complete
					got_request_from_client(Request, Socket, RequestHandler)
			end;

		{tcp_closed, Socket} ->
			io:format("Socket fechado.~n"),
		    void;

		_Any  ->
		    %% skip this
			io:format("Any data.~n"),
		    get_request(Socket, RequestHandler, L)
    end.


split("\r\n\r\n" ++ T, L) -> {lists:reverse(L), T};
split([H|T], L)           -> split(T, [H|L]);
split([], _)              -> more.



got_request_from_client(Request, Socket, RequestHandler) ->
    Cmds = string:tokens(Request, "\r\n"),
    Cmds1 = lists:map(fun(I) -> tokens(I, " ") end, Cmds),
	io:format("~p~n", [Cmds1]),
    gen_tcp:send(Socket, [response("Hello World!!!")]).



response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [size(B), B])).



