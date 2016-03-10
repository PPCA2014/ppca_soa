%%********************************************************************
%% @title Módulo msbus
%% @version 1.0.0
%% @doc Main Module of ErlangsMS
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus).

%%% API
-export([start/0, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	application:start(ranch),
    application:start(oauth2),
    application:start(crypto),
    application:start(odbc),
    application:start(msbus).
    

stop() ->
    application:stop(msbus),
    application:stop(crypto),
    application:stop(oauth2),
    application:stop(odbc),
    application:stop(ranch).
