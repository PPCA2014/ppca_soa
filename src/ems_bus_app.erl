-module(ems_bus_app).

-behaviour(application).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").


%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

 
start(_StartType, StartArgs) ->
    io:format("Iniciando ErlangMS...\n"),
    application:start(oauth2),
    application:start(crypto),
	application:start(ssl),    
    application:start(odbc),
    application:start(ranch),
	application:start(cowlib),
	application:start(cowboy),
	case ems_config:start() of
		{ok, _Pid} ->
			T1 = ems_util:get_milliseconds(),
			Config = ems_config:getConfig(),
			ems_db:start(),
			case ems_catalog_loader:init_catalog() of
				ok ->
					odbc:start(),
					ems_logger:start(),
					ems_dispatcher:start(),
					ems_health:start(),
					ssl:start(),
					ems_logger:info("~n~s", [?SERVER_NAME]),
					Ret = ems_bus_sup:start_link(StartArgs),
					ems_logger:info("cat_host_alias: ~p", [Config#config.cat_host_alias]),
					ems_logger:info("cat_host_search: ~s", [ems_util:join_binlist(Config#config.cat_host_search, ", ")]),
					ems_logger:info("cat_node_search: ~s", [ems_util:join_binlist(Config#config.cat_node_search, ", ")]),
					ems_logger:info("log_file_dest: ~s", [Config#config.log_file_dest]),
					ems_logger:info("log_file_checkpoint: ~pms", [Config#config.log_file_checkpoint]),
					ems_logger:debug("In debug mode: ~p~", [Config#config.ems_debug]),
					ems_logger:info("Server ~s started in ~pms.", [node(), ems_util:get_milliseconds() - T1]),
					ems_logger:sync(),
					ems_logger:set_level(info),
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
    io:format("FinalizandoIniciando ErlangMS...\n"),
    ems_bus_sup:stop(),
    ems_logger:stop(),
	ems_config:stop(),
    application:stop(ssl),
    application:stop(crypto),
    application:stop(oauth2),
    application:stop(odbc),
	application:start(cowboy),
	application:start(cowlib),
	application:start(ranch),
    ok.
    
    
													 
    
