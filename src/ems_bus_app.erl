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
	ems_db:start(),
	case ems_config:start() of
		{ok, _Pid} ->
			T1 = ems_util:get_milliseconds(),
			case ems_catalog_loader:start() of
				ok ->
					ems_dispatcher:start(),
					ems_oauth2_backend:start(),
					Ret = ems_bus_sup:start_link(StartArgs),
					erlang:send_after(5000, spawn(fun() -> 
							Conf = ems_config:getConfig(),
							ems_logger:info("Hosts in the cluster: ~p.", [ case net_adm:host_file() of 
																				{error, enoent} -> net_adm:localhost(); 
																				Hosts -> Hosts 
																		  end]),
							AuthorizationMode = case Conf#config.authorization of
													basic -> <<"basic, oauth2">>;
													oauth2 -> <<"oauth2">>;
													public -> <<"public">>
												end,
							case Conf#config.oauth2_with_check_constraint of
								true -> ems_logger:info("Default authorization mode: ~p <<with check constraint>>.", [AuthorizationMode]);
								false -> ems_logger:info("Default authorization mode: ~p.", [AuthorizationMode])
							end,
							ems_logger:info("Server ~s (PID ~s) started in ~pms.", [?SERVER_NAME, os:getpid(), ems_util:get_milliseconds() - T1]),
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
    
    
													 
    
