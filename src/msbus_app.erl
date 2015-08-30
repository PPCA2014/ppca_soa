-module(msbus_app).

-behaviour(application).

-include("../include/msbus_config.hrl").

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).
    
start(_StartType, StartArgs) ->
    T1 = msbus_util:get_milliseconds(),
    io:format("~s~n", [?SERVER_NAME]),
    Ret = msbus_sup:start_link(StartArgs),
    Conf = msbus_config:getConfig(),
    io:format("Portal ErlangMS Api Management em http://127.0.0.1:~p/portal/index.html~n", [(msbus_config:getConfig())#config.tcp_port]),
    io:format("ErlangMS iniciado em ~pms.~n", [msbus_util:get_milliseconds() - T1]),
    Ret.

stop(_State) ->
    ok.
    
    
