%%********************************************************************
%% @title Module ems_bus
%% @version 1.0.0
%% @doc Main Module of ErlangsMS
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_bus).

%%% API
-export([start/0, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(oauth2),
    application:start(crypto),
	application:start(ssl),    
    application:start(odbc),
    application:start(ems_bus).
    

stop() ->
    application:stop(ems_bus),
    application:stop(ssl),
    application:stop(crypto),
    application:stop(oauth2),
    application:stop(odbc).
