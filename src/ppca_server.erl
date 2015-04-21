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
						HeaderDict = get_http_header(Header),
						{ok, Content_Length} = dict:find("Content-Length", HeaderDict), 
						if length(Payload) == Content_Length -> 
								{HeaderDict, Payload};
							true -> 
								{HeaderDict, get_request_payload(Socket, Content_Length, Payload)}
						end
				end;

		{tcp_closed, Socket} ->
		    void;

		_Any  ->
		    %% skip this
		    get_request(Socket, L)
    end.



get_request_payload(Socket, Length, L) ->
    receive
		{tcp, Socket, Bin} -> 
				L1 = L ++ binary_to_list(Bin),
				if length(L1) == Length -> L1;
					true -> 
						get_request_payload(Socket, Length, L1)
				end;

		{tcp_closed, Socket} ->
		    void;

		_Any  ->
		    %% skip this
		    get_request_payload(Socket, Length, L)

    end.


get_http_header(Header) ->
	Headers = map(fun(P) -> string:tokens(P, ":") end, string:tokens(Header, "\r\n")),
	FmtParamValue = fun(ParamName, Value) -> if Value /= []  -> V1 = string:strip(hd(Value)); 
												true 		 -> V1 = ""
											 end,
											 if ParamName == "Content-Length" -> V2 = list_to_integer(V1);
												true		   				  -> V2 = V1
											 end,
											 V2
					end,
	
	dict:from_list([{Param, FmtParamValue(Param, Value)} || [Param|Value] <- Headers]).


split("\r\n\r\n" ++ T, L) -> {lists:reverse(L), T};
split([H|T], L)           -> split(T, [H|L]);
split([], _)              -> more.



trata_request(RequestHandler, Header, Payload) ->
	io:format("Header: ~n", []),
	io:format("\tContent-Length:  ~p~n", [dict:find("Content-Length", Header)]),
	io:format("\tAccept-Encoding:  ~p~n", [dict:find("Accept-Encoding", Header)]),
	io:format("\tHost:  ~p~n", [dict:find("Host", Header)]),
	io:format("\tAccept-Language:  ~p~n", [dict:find("Accept-Language", Header)]),
	io:format("\tConnection:  ~p~n", [dict:find("Connection", Header)]),
	io:format("\tPragma:  ~p~n", [dict:find("Pragma", Header)]),
	io:format("\tCache-Control:  ~p~n", [dict:find("Cache-Control", Header)]),
	io:format("\tAccept:  ~p~n", [dict:find("Accept", Header)]),
	io:format("\tUser-Agent:  ~p~n", [dict:find("User-Agent", Header)]),
	io:format("\tCookie:  ~p~n", [dict:find("Cookie", Header)]),
	io:format("Payload: ~p~n", [Payload]),
	%ListaHeader = string:tokens(Header, "\r\n"),
	%io:format("Header: ~p~n", [ListaHeader]),
	%io:format("Payload: ~p~n", [Payload]),
	%[Metodo|[Url|_]]= string:tokens(hd(ListaHeader), " "),
	{Codigo, Corpo} = processa_request(RequestHandler, "Metodo", "Url", Payload),
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



