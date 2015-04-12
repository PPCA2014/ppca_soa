%% ---
%%  PPCA_SOA
%%  Barramento SOA desenvolvido na disciplina de Construção de Software
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---

-module(ppca_soa).

-export([start/1, start_listen/2, stop_listen/2]).


start(Port) -> 
	io:format("PPCA_SOA - Barramento SOA da Turma PPCA 2014~n"),
	Server = spawn(ppca_server, init, []),
	start_listen(Server, Port),
	Server.



start_listen(Server, Port) ->
	Server ! { self(), {start_listen, Port}},
	receive
		{ Server, ok } -> io:format("Escutando na porta ~p.~n", [Port]);
		{ Server, {error, Reason} } -> io:format("Não foi possível escutar na porta ~p.~nMotivo: ~p.~n", [Port, Reason])
	end.


stop_listen(Server, Port) ->
	Server ! { self(), {stop_listen, Port}},
	receive
		{ Server, ok } -> io:format("Parou de escutar na porta ~p.~n", [Port]);
		{ Server, {error, Reason} } -> io:format("~p.~n", [Reason])
	end.
