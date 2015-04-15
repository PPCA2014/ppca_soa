%% ---
%%  PPCA_SOA
%%  Publish and subscribe message queue
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---

-module(ppca_event_mq).

-export([start/0, 
		 adiciona_evento/1, 
		 registra_interesse/1, 
		 lista_evento/0, 
		 lista_interesse/0, 
		 notifica_evento/1, 
		 loop/2]).


start() ->
    ListaEvento = [],
    ListaInteresse = [],
	Pid = spawn(ppca_event_mq, loop, [ListaEvento, ListaInteresse]),
	register(ppca_event, Pid).
	

adiciona_evento(Evento) ->
	ppca_event ! {self(), {adiciona_evento, Evento}},
	receive
		Msg -> Msg
	end.
	

registra_interesse({Evento, Fun}) ->
	ppca_event ! {self(), {registra_interesse, {Evento, Fun}}},
	receive
		Msg -> Msg
	end.


lista_evento() ->
	ppca_event ! {self(), lista_evento},
	receive
		Msg -> Msg
	end.


lista_interesse() ->
	ppca_event ! {self(), lista_interesse},
	receive
		Msg -> Msg
	end.


notifica_evento(QualEvento) ->
	ppca_event ! {self(), {notifica_evento, QualEvento}},
	receive
		ok -> ok;
		{error, Reason} -> Reason
	end.
	

loop(ListaEvento, ListaInteresse) ->
	receive
		{From, {adiciona_evento, Evento}} ->
			case lists:member(Evento, ListaEvento) of
				true -> 
					From ! ok,
					loop(ListaEvento, ListaInteresse);
				false -> 
					From ! ok,
					loop([Evento|ListaEvento], ListaInteresse)
			end;
		{From, {notifica_evento, QualEvento}} ->
			case lists:member(QualEvento, ListaEvento) of
				true -> 
					[io:format("~p~n", [Interesse]) || Interesse <- ListaInteresse],
					From ! ok;
				false -> 
					From ! {error, enotsubscribe}
			end,
			loop(ListaEvento, ListaInteresse);
		{From, {registra_interesse, {Evento, Fun}}} ->
			case lists:member({Evento, Fun}, ListaInteresse) of
				true -> 
					From ! ok,
					loop(ListaEvento, ListaInteresse);
				false -> 
					From ! ok,
					loop(ListaEvento, [{Evento, Fun}|ListaInteresse])
			end;
		{From, lista_evento} ->
			From ! ListaEvento,
			loop(ListaEvento, ListaInteresse);
		{From, lista_interesse} ->
			From ! ListaInteresse,
			loop(ListaEvento, ListaInteresse)
	end.
	

