%% ---
%%  PPCA_SOA
%%  Barramento SOA desenvolvido na disciplina de Construção de Software
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---
-module(ppca_soa).

-export([start/1, start_listen/2]).


start(Port) -> 
	io:format("PPCA_SOA - Barramento SOA da Turma PPCA 2014~n"),
	Server = spawn(ppca_server, init, []),
	start_listen(Server, Port).



start_listen(Server, Port) ->
	Server ! { self(), {start_listen, Port}},
	receive
		{ Server, {ok, Port} } -> io:format("Listen porta ~p.~n", [Port])
	end.
