%%********************************************************************
%% @title Módulo soa
%% @version 1.0.0
%% @doc Módulo responsável por iniciar o erlangMS.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************


-module(msbus_soa).

-export([start/0, start/1, start_listen/1, stop_listen/1]).

-include("../include/msbus_config.hrl").


-spec start(pos_integer()) -> void.
start(Port) -> 
	msbus_logger:start(),
	msbus_logger:info(?SERVER_NAME),
	msbus_server:start(),
	msbus_catalogo:start(),
	msbus_dispatcher:start(),
	msbus_info:start(),
	msbus_favicon:start(),
	msbus_static_file:start(),
	msbus_auth_user:start(),
	start_listen(Port),
	msbus_logger:info("Aguardando requisições...").

-spec start() -> void.
start() ->
	start(?CONF_PORT).


-spec start_listen(Port::pos_integer()) -> ok | {error, Reason::string()}.
start_listen(Port) ->
	msbus_server:start_listen(Port, self()),
	receive
		ok -> msbus_logger:info("Escutando na porta ~p.", [Port]);
		{error, Reason} -> msbus_logger:error("Não foi possível escutar na porta ~p. Motivo: ~p.", [Port, Reason])
	end.


-spec stop_listen(Port::pos_integer()) -> ok | {error, Reason::string()}.
stop_listen(Port) ->
	msbus_server:stop_listen(Port, self()),
	receive
		ok -> msbus_logger:info("Porta ~p fechada.", [Port]);
		{error, Reason} -> msbus_logger:error("Erro ao fechar porta ~p: Motivo: ~p.", [Port, Reason])
	end.
