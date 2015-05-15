%% ---
%%  PPCA_SOA
%%  Barramento SOA desenvolvido na disciplina de Construção de Software
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Alunos: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%---

-module(ppca_soa).

-export([start/0, start/1, start_listen/1, stop_listen/1]).

-include("../include/ppca_config.hrl").


-spec start(pos_integer()) -> void.
start(Port) -> 
	io:format("PPCA_SOA - Barramento SOA da Turma PPCA 2014~n"),
	ppca_logger:start(),
	register(ppca_server, spawn(fun() -> ppca_server:init() end)),
	start_listen(Port).


-spec start() -> void.
start() ->
	start(?CONF_PORT).


-spec start_listen(Port::pos_integer()) -> ok | {error, Reason::string()}.
start_listen(Port) ->
	ppca_server ! { self(), {start_listen, Port}},
	receive
		ok -> ppca_logger:info_msg("Escutando na porta ~p", [Port]);
		{error, Reason} -> ppca_logger:erro_msg("Não foi possível escutar na porta ~p. Motivo: ~p.", [Port, Reason])
	end.


-spec stop_listen(Port::pos_integer()) -> ok | {error, Reason::string()}.
stop_listen(Port) ->
	ppca_server ! { self(), {stop_listen, Port}},
	receive
		ok -> ppca_logger:info_msg("Porta ~p fechada.~n", [Port]);
		{error, Reason} -> ppca_logger:erro_msg("Erro ao fechar porta ~p: Motivo: ~p.", [Port, Reason])
	end.
