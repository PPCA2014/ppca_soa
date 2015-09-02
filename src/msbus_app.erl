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
	case msbus_config:start() of
		{ok, _Pid} ->
			Config = msbus_config:getConfig(),
			msbus_logger:start(),
			T1 = msbus_util:get_milliseconds(),
			msbus_logger:info("~s", [?SERVER_NAME]),
			Ret = msbus_sup:start_link(StartArgs),
			msbus_logger:info("Portal ErlangMS Api Management em http://127.0.0.1:~p/portal/index.html", [Config#config.tcp_port]),
			msbus_logger:info("ErlangMS iniciado em ~pms.", [msbus_util:get_milliseconds() - T1]),
			Ret;
		{error, Error} ->
			io:format("Erro ao processar arquivo de configuração: ~p.", [Error]),
			{error, fim}
	end.

stop(_State) ->
    ok.
    
    
