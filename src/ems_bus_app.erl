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
			case ems_catalog:init_catalog() of
				ok ->
					odbc:start(),
					ems_logger:start(),
					ems_logger:info("~n~s", [?SERVER_NAME]),
					Ret = ems_bus_sup:start_link(StartArgs),
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
					ems_logger:info("ldap_tcp_port: ~p", [Config#config.ldap_tcp_port]),
					ems_logger:info("ldap_datasource: ~s", [Config#config.ldap_datasource]),
					ems_logger:info("ldap_admin: ~s", [Config#config.ldap_admin]),
					ems_logger:debug("In debug mode: ~p~", [Config#config.ems_debug]),
					ems_logger:info("Server ~s started in ~pms.", [node(), ems_util:get_milliseconds() - T1]),
					ems_logger:sync(),
					ems_logger:set_level(error),
					Ret;
				Error-> 
					io:format("Error processing catalogs: ~p.", [Error]),
					{error, finish}
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
    
    
													 
    
