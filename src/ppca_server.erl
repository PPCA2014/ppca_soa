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
	spawn(fun() -> aceita_conexoes(Listen, RequestHandler) end),
	inet:setopts(Socket, [{packet,0},binary, {nodelay,true},{active, true}]),
	{Header, Payload} = get_request(Socket, []),
	Response = trata_request(RequestHandler, Header, Payload),
	gen_tcp:send(Socket, [Response]).
    

	
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
						io:format("~p~n", [Header]),
						HeaderDict = get_http_header(Header),
						case dict:fetch("Metodo", HeaderDict) of
							"POST" -> 
								case dict:find("Content-Length", HeaderDict) of
									{ok, Content_Length} when Content_Length == 0 ->
										{HeaderDict, ""};
									{ok, Content_Length} ->
										if length(Payload) == Content_Length -> 
												{HeaderDict, Payload};
											true -> 
												{HeaderDict, get_request_payload(Socket, Content_Length, Payload)}
										end;
									error -> 
										{HeaderDict, ""}
								end;
							_ -> {HeaderDict, ""}
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



-spec get_http_header(Header::list()) -> dict:dict().
get_http_header(Header) ->
	[H|H1] = string:tokens(Header, "\r\n"),
	[Metodo|[Url|[Versao_HTTP|_]]] = string:tokens(H, " "),
	H2 = map(fun(P) -> string:tokens(P, ":") end, H1),
	FmtValue = fun(P, V) -> 
					if V /= [] -> V1 = string:strip(hd(V)); 
					   true ->  V1 = ""
					end,
					if P == "Content-Length" -> 
							V2 = list_to_integer(V1),
							case is_content_length_valido(V2) of
								true -> V2;
								false -> 0
							end;
					   true -> 
							V1
					end
				end,
	dict:from_list([{"Metodo", Metodo},  
					{"Url", Url},
					{"Versao_HTTP", Versao_HTTP}] ++ 
					[{P, FmtValue(P, V)} || [P|V] <- H2]).



trata_request(RequestHandler, Header, Payload) ->
	%% imprime cabecalho e payload para fins de depuração
	io:format("Nova requisição ~p ~p em ~p.~n", [dict:fetch("Metodo", Header), 
												 dict:fetch("Url", Header), 
												 erlang:localtime()]),    
	io:format("Header: ~n", []),
	[ io:format("\t~p:  ~p~n", [P, dict:fetch(P, Header)]) || P <- dict:fetch_keys(Header)],
	io:format("Payload: ~p~n", [Payload]),

	%% Processa o request e obtém o código de retorno e corpo do response
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
         "HTTP/1.1 ~p OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [Codigo, size(B), B])).


is_fim_header("\r\n\r\n" ++ T, L) -> {lists:reverse(L), T};
is_fim_header([H|T], L)           -> is_fim_header(T, [H|L]);
is_fim_header([], _)              -> more.


is_content_length_valido(N) when N < 0; N > ?HTTP_MAX_POST_SIZE -> false;
is_content_length_valido(_) -> true.


