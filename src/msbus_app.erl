-module(msbus_app).

-behaviour(application).

-include("../include/msbus_config.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
    io:format("~s~n", [?SERVER_NAME]),
    msbus_sup:start_link(StartArgs).

stop(_State) ->
    ok.
