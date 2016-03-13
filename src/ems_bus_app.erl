-module(ems_bus_app).

-behaviour(application).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").


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
	case ems_config:start() of
		{ok, _Pid} ->
			T1 = ems_util:get_milliseconds(),
			Config = ems_config:getConfig(),
			ems_logger:start(),
			ems_logger:info("~n~s", [?SERVER_NAME]),
			odbc:start(),

			case ems_catalog:init_catalogo() of
				ok ->
					Ret = ems_bus_sup:start_link(StartArgs),

					%% first ip for the portal
					IpPortal = hd(Config#config.tcp_listen_address), 

					%% show config parameters 
					ems_logger:info("Reading config parameters from ~s...", [Config#config.ems_file_dest]),
					ems_logger:info("cat_host_alias: ~p", [Config#config.cat_host_alias]),
					ems_logger:info("cat_host_search: ~s", [ems_util:join_binlist(Config#config.cat_host_search, ", ")]),
					ems_logger:info("cat_node_search: ~s", [ems_util:join_binlist(Config#config.cat_node_search, ", ")]),
					ems_logger:info("log_file_dest: ~s", [Config#config.log_file_dest]),
					ems_logger:info("log_file_checkpoint: ~pms", [Config#config.log_file_checkpoint]),
					ems_logger:info("tcp_listen_address: ~p", [Config#config.tcp_listen_address]),
					ems_logger:info("tcp_allowed_address: ~p", [Config#config.tcp_allowed_address]),
					ems_logger:info("tcp_port: ~p", [Config#config.tcp_port]),
					ems_logger:info("tcp_keepalive: ~p", [Config#config.tcp_keepalive]),
					ems_logger:info("tcp_nodelay: ~p", [Config#config.tcp_nodelay]),
					ems_logger:info("tcp_max_http_worker: ~p", [Config#config.tcp_max_http_worker]),

					ems_logger:debug("In debug mode: ~p~", [Config#config.ems_debug]),

					ems_logger:info("Portal Api Management: http://~s:~p/portal/index.html", [IpPortal, Config#config.tcp_port]),
					ems_logger:info("Node ~s started in ~pms.", [node(), ems_util:get_milliseconds() - T1]),
					ems_logger:sync(),

					%% Start servers...
					ems_http_server:start_listeners(Config#config.tcp_port, Config#config.tcp_listen_address_t),
					ems_ldap_server:start_listeners(2389, Config#config.tcp_listen_address_t),

					%% Facilitates depuration on initialization
					ems_util:sleep(2500), 

					register_events(),
					Ret;
				{error, Reason} -> 
					ems_logger:sync(),
					{error, Reason}
			end;
		{error, Reason} ->
			io:format("Error processing configuration file: ~p.", [Reason]),
			{error, finish}
	end.

stop(_State) ->
    ems_bus_sup:stop(),
    ems_logger:stop(),
	ems_config:stop(),
    ok.
    
register_events() ->
   	ems_eventmgr:adiciona_evento(new_request),
	ems_eventmgr:adiciona_evento(ok_request),
	ems_eventmgr:adiciona_evento(erro_request),
	ems_eventmgr:adiciona_evento(close_request),
	ems_eventmgr:adiciona_evento(send_error_request),

    ems_eventmgr:registra_interesse(ok_request, fun(_Q, {_, #request{worker_send=Worker}, _} = R) -> 
														gen_server:cast(Worker, R)
												  end),

    ems_eventmgr:registra_interesse(erro_request, fun(_Q, R) -> 
														ems_http_worker:cast(R) 
													end),

	ems_eventmgr:registra_interesse(close_request, fun(_Q, R) -> 
														ems_logger:log_request(R) 
													 end).
    
													 
    
