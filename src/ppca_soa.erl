%%********************************************************************
%% @title Módulo soa
%% @version 1.0.0
%% @doc Módulo responsável por iniciar o erlangMS.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************


-module(ppca_soa).

-export([start/0, start/1, start_listen/1, stop_listen/1]).

-include("../include/ppca_config.hrl").


-spec start(pos_integer()) -> void.
start(Port) -> 
	ppca_logger:start(),
	ppca_logger:info_msg(?SERVER_NAME),
	ppca_server:start(),
	ppca_catalogo_service:start(),
	ppca_dispatcher:start(),
	ppca_info_service:start(),
	ppca_favicon_service:start(),
	static_file_service:start(),
	ppca_auth_user:start(),
	start_listen(Port),
	ppca_logger:info_msg("Aguardando requisições...").

-spec start() -> void.
start() ->
	start(?CONF_PORT).


-spec start_listen(Port::pos_integer()) -> ok | {error, Reason::string()}.
start_listen(Port) ->
	ppca_server:start_listen(Port, self()),
	receive
		ok -> ppca_logger:info_msg("Escutando na porta ~p.", [Port]);
		{error, Reason} -> ppca_logger:error_msg("Não foi possível escutar na porta ~p. Motivo: ~p.", [Port, Reason])
	end.


-spec stop_listen(Port::pos_integer()) -> ok | {error, Reason::string()}.
stop_listen(Port) ->
	ppca_server:stop_listen(Port, self()),
	receive
		ok -> ppca_logger:info_msg("Porta ~p fechada.", [Port]);
		{error, Reason} -> ppca_logger:error_msg("Erro ao fechar porta ~p: Motivo: ~p.", [Port, Reason])
	end.
