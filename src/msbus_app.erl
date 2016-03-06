-module(msbus_app).

-behaviour(application).

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").


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
			T1 = msbus_util:get_milliseconds(),
			Config = msbus_config:getConfig(),
			msbus_logger:start(),
			msbus_logger:info("~s", [?SERVER_NAME]),
			case msbus_catalogo:init_catalogo() of
				ok ->
					Ret = msbus_sup:start_link(StartArgs),

					%% first ip for the portal
					IpPortal = hd(Config#config.tcp_listen_address), 

					%% show config parameters 
					msbus_logger:info("config_file_dest: ~s", [Config#config.config_file_dest]),
					msbus_logger:info("cat_host_alias: ~p", [Config#config.cat_host_alias]),
					msbus_logger:info("cat_host_search: ~s", [msbus_util:join_binlist(Config#config.cat_host_search, ", ")]),
					msbus_logger:info("cat_node_search: ~s", [msbus_util:join_binlist(Config#config.cat_node_search, ", ")]),
					msbus_logger:info("log_file_dest: ~s", [Config#config.log_file_dest]),
					msbus_logger:info("log_file_checkpoint: ~pms", [Config#config.log_file_checkpoint]),
					msbus_logger:info("tcp_listen_address: ~p", [Config#config.tcp_listen_address]),
					msbus_logger:info("tcp_allowed_address: ~p", [Config#config.tcp_allowed_address]),
					msbus_logger:info("tcp_port: ~p", [Config#config.tcp_port]),
					msbus_logger:info("tcp_keepalive: ~p", [Config#config.tcp_keepalive]),
					msbus_logger:info("tcp_nodelay: ~p", [Config#config.tcp_nodelay]),
					msbus_logger:info("tcp_max_http_worker: ~p", [Config#config.tcp_max_http_worker]),
					msbus_logger:debug("modo_debug: ~p", [Config#config.modo_debug]),
					msbus_logger:info("Portal ErlangMS Api Management: http://~s:~p/portal/index.html", [IpPortal, Config#config.tcp_port]),
					msbus_logger:info("~s started in ~pms.", [node(), msbus_util:get_milliseconds() - T1]),
					msbus_logger:sync(),

					%% Start servers...
					msbus_http_server:start_listeners(Config#config.tcp_port, Config#config.tcp_listen_address_t),
					msbus_ldap_server:start_listeners(2389, Config#config.tcp_listen_address_t),

					%% Facilitates depuration on initialization
					msbus_util:sleep(2500), 

					register_events(),
					Ret;
				{error, Reason} -> 
					msbus_logger:sync(),
					{error, Reason}
			end;
		{error, Reason} ->
			io:format("Error processing configuration file: ~p.", [Reason]),
			{error, finish}
	end.

stop(_State) ->
    msbus_sup:stop(),
    msbus_logger:stop(),
	msbus_config:stop(),
    ok.
    
register_events() ->
   	msbus_eventmgr:adiciona_evento(new_request),
	msbus_eventmgr:adiciona_evento(ok_request),
	msbus_eventmgr:adiciona_evento(erro_request),
	msbus_eventmgr:adiciona_evento(close_request),
	msbus_eventmgr:adiciona_evento(send_error_request),

    msbus_eventmgr:registra_interesse(ok_request, fun(_Q, {_, #request{worker_send=Worker}, _} = R) -> 
														gen_server:cast(Worker, R)
												  end),

    msbus_eventmgr:registra_interesse(erro_request, fun(_Q, R) -> 
														msbus_http_worker:cast(R) 
													end),

	msbus_eventmgr:registra_interesse(close_request, fun(_Q, R) -> 
														msbus_logger:log_request(R) 
													 end).
    
													 
    
