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
	case ems_config:start() of
		{ok, _Pid} ->
			T1 = ems_util:get_milliseconds(),
			ems_db:start(),
			case ems_catalog_loader:init_catalog() of
				ok ->
					ems_dispatcher:start(),
					ems_health:start(),
					oauth2ems_backend:start(),
					Ret = ems_bus_sup:start_link(StartArgs),
					Conf = ems_config:getConfig(),
					erlang:send_after(4500, spawn(fun() -> 
														ems_logger:info("Hosts in the cluster: ~p.", [ case net_adm:host_file() of 
																											{error, enoent} -> net_adm:localhost(); 
																											Hosts -> Hosts 
																									  end]),
														case Conf#config.oauth2_with_check_constraint of
															true -> ems_logger:info("Authorization mode: ~p <<with check constraint>>.", [Conf#config.authorization]);
															false -> ems_logger:info("Authorization mode: ~p.", [Conf#config.authorization])
														end,
														ems_logger:info("Server ~s started in ~pms.", [?SERVER_NAME, ems_util:get_milliseconds() - T1]),
														ems_logger:sync(),
														ems_logger:set_level(info)
												  end), set_level),
					Ret;
				Error-> 
					io:format("Error processing catalogs. Reason: ~p.", [Error]),
					{error, finish}
			end;
		{error, Reason} ->
			io:format("Error processing configuration file. Reason: ~p.", [Reason]),
			{error, finish}
	end.

stop(_State) ->
    ems_logger:info("Stopping server...\n"),
    ems_logger:sync(),
    ems_bus_sup:stop(),
    ems_logger:stop(),
	ems_config:stop(),
    ok.
    
    
													 
    
