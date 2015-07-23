-module(msbus_sup).

-behaviour(supervisor).


%% Supervisor callbacks
-export([start_link/1, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	msbus_database:start(),

	{ok, {{one_for_one, 3, 10},
			[
			
			%% Principais módulos de erlangMS
			{msbus_logger,
			  {msbus_logger, start, []},
			  permanent,
			  10000,
			  worker,
			  [msbus_logger]},
			 {msbus_catalogo,
			  {msbus_catalogo, start, []},
			  permanent,
			  10000,
			  worker,  
			  [msbus_catalogo]},
			 {msbus_info,
   			  {msbus_info, start, []},
			  permanent,
			  10000,
			  worker,
			  [msbus_info]},
			 {msbus_favicon,
			  {msbus_favicon, start, []},
			  permanent,
			  10000,
			  worker,
			  [msbus_favicon]},
			 {msbus_static_file,
   			  {msbus_static_file, start, []},
			   permanent,
			   10000,
			   worker,
			   [msbus_static_file]},
			 {msbus_dispatcher,
			  {msbus_dispatcher, start, []},
			   permanent,
			   10000,
			   worker,  
			   [msbus_dispatcher]},
			 {msbus_user,
			  {msbus_user, start, []},
			   permanent,
			   10000,
			   worker,  
			   [msbus_user]},
			 {msbus_health,
			  {msbus_health, start, []},
			   permanent,
			   10000,
			   worker,  
			   [msbus_health]},
			 {msbus_server,
			  {msbus_server, start, []},
			  permanent,
			  10000,
			  worker,
			  [msbus_server]},


			 %% Serviços REST
			 {msbus_user_service,
			  {msbus_user_service, start, []},
			   permanent,
			   10000,
			   worker,  
			   [msbus_user_service]},
			 {msbus_health_service,
			  {msbus_health_service, start, []},
			   permanent,
			   10000,
			   worker,  
			   [msbus_health_service]}
			   			  
			   
			   
			   
			   			  
 		    ]  
 		}
 	}.



