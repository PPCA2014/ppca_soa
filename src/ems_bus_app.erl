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
    io:format("aqui1\n"),
    application:start(oauth2),
    application:start(crypto),
	application:start(ssl),    
    application:start(odbc),
    application:start(ranch),
	application:start(cowlib),
	application:start(cowboy),
	application:start(erlydtl),
	case ems_config:start() of
		{ok, _Pid} ->
			io:format("aqui2\n"),
			T1 = ems_util:get_milliseconds(),
			ems_db:start(),
			io:format("aqui2.1\n"),
			case ems_catalog_loader:init_catalog() of
				ok ->
					io:format("aqui2.2\n"),
					ems_dispatcher:start(),
					ems_health:start(),
					io:format("aqui2.3\n"),
					Ret = ems_bus_sup:start_link(StartArgs),
					erlang:send_after(1500, spawn(fun() -> 
														ems_logger:info("Server ~s started in ~pms.", [?SERVER_NAME, ems_util:get_milliseconds() - T1]),
														ems_logger:sync(),
														ems_logger:set_level(info)
												  end), set_level),
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
    ems_logger:info("Stopping server...\n"),
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
    
    
													 
    
