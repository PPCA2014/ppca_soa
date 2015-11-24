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
			%% primeiro ip disponível informado na configuração
			IpPortal = inet:ntoa(hd(Config#config.tcp_listen_address)), 
			msbus_logger:info("Hostnames alias: ~p.", [Config#config.cat_host_alias]),
			msbus_logger:info("Portal ErlangMS Api Management: http://~s:~p/portal/index.html", [IpPortal, 
																								 Config#config.tcp_port]),
			msbus_logger:info("ESB ErlangMS iniciado em ~pms.", [msbus_util:get_milliseconds() - T1]),
			msbus_logger:sync(),
			registra_eventos(),
			Ret;
		{error, Error} ->
			io:format("Erro ao processar arquivo de configuração: ~p.", [Error]),
			{error, fim}
	end.

stop(_State) ->
    msbus_sup:stop(),
    msbus_logger:stop(),
	msbus_config:stop(),
    ok.
    
registra_eventos() ->
   	msbus_eventmgr:adiciona_evento(new_request),
	msbus_eventmgr:adiciona_evento(ok_request),
	msbus_eventmgr:adiciona_evento(erro_request),
	msbus_eventmgr:adiciona_evento(close_request),
	msbus_eventmgr:adiciona_evento(send_error_request),

    msbus_eventmgr:registra_interesse(ok_request, fun(_Q, R) -> 
														msbus_server_worker:cast(R) 
												  end),

    msbus_eventmgr:registra_interesse(erro_request, fun(_Q, R) -> 
														msbus_server_worker:cast(R) 
													end),

	msbus_eventmgr:registra_interesse(close_request, fun(_Q, R) -> 
														msbus_logger:log_request(R) 
													 end).
    
													 
    
