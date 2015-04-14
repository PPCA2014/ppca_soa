%% ---
%%  PPCA_SOA
%%  Publish and subscribe message queue
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---

-module(ppca_event_mq).

-export([start/0, adiciona_evento/2, registra_interesse/2, lista_evento/1, lista_interesse/1, loop/2]).


start() ->
    ListaEvento = [],
    ListaInteresse = [],
	spawn(ppca_event_mq, loop, [ListaEvento, ListaInteresse]).
	

adiciona_evento(Pid, Evento) ->
	Pid ! {self(), {adiciona_evento, Evento}},
	receive
		{Pid, Msg} -> Msg
	end.
	

registra_interesse(Pid, {Evento, URI, Metodo}) ->
	Pid ! {self(), {registra_interesse, {Evento, URI, Metodo}}},
	receive
		{Pid, Msg} -> Msg
	end.


lista_evento(Pid) ->
	Pid ! {self(), lista_evento},
	receive
		{Pid, Msg} -> Msg
	end.


lista_interesse(Pid) ->
	Pid ! {self(), lista_interesse},
	receive
		{Pid, Msg} -> Msg
	end.


loop(ListaEvento, ListaInteresse) ->
	receive
		{From, {adiciona_evento, Evento}} ->
			case lists:member(Evento, ListaEvento) of
				true -> 
					From ! {self(), ok},
					loop(ListaEvento, ListaInteresse);
				false -> 
					From ! {self(), ok},
					loop([Evento|ListaEvento], ListaInteresse)
			end;
		{From, {registra_interesse, {Evento, URI, Metodo}}} ->
			case lists:member({Evento, URI, Metodo}, ListaInteresse) of
				true -> 
					From ! {self(), ok},
					loop(ListaEvento, ListaInteresse);
				false -> 
					From ! {self(), ok},
					loop(ListaEvento, [{Evento, URI, Metodo}|ListaInteresse])
			end;
		{From, lista_evento} ->
			From ! {self(), ListaEvento};
		{From, lista_interesse} ->
			From ! {self(), ListaInteresse}
	end.
	

