%% ---
%%  PPCA_SOA
%%  Barramento SOA desenvolvido na disciplina de Construção de Software
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---

-module(ppca_soa).

-export([start/0, start/1, start_listen/2, stop_listen/2]).

-include("../include/ppca_config.hrl").


-spec start(integer()) -> pid().
start(Port) -> 
	io:format("PPCA_SOA - Barramento SOA da Turma PPCA 2014~n"),
	Server = spawn(ppca_server, init, []),
	case start_listen(Server, Port) of
		ok -> io:format("Escutando na porta ~p.~n", [Port]);
		{error, Reason} -> io:format("Não foi possível escutar na porta ~p. Motivo: ~p.~n", [Port, Reason])
	end,
	Server.


-spec start() -> pid().
start() ->
	start(?CONF_PORT).


-spec start_listen(Server::pid(), Port::integer()) -> ok | {error, Reason::string()}.
start_listen(Server, Port) ->
	Server ! { self(), {start_listen, Port}},
	receive
		{ Server, ok } -> ok;
		{ Server, {error, Reason} } -> {error, Reason}
	end.


-spec stop_listen(Server::pid(), Port::integer()) -> ok | {error, Reason::string()}.
stop_listen(Server, Port) ->
	Server ! { self(), {stop_listen, Port}},
	receive
		{ Server, ok } -> ok;
		{ Server, {error, Reason} } -> {errpr, Reason}
	end.

