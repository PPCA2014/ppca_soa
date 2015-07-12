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
			[{tag1,
			  {msbus_logger, start, []},
			  permanent,
			  10000,
			  worker,
			  [msbus_logger]},
			 {tag2,
			  {msbus_server, start, []},
			  permanent,
			  10000,
			  worker,
			  [msbus_server]},
			 {tag3,
			  {msbus_catalogo, start, []},
			  permanent,
			  10000,
			  worker,  
			  [msbus_catalogo]},
			 {tag4,
   			  {msbus_info, start, []},
			  permanent,
			  10000,
			  worker,
			  [msbus_info]},
			 {tag5,
			  {msbus_favicon, start, []},
			  permanent,
			  10000,
			  worker,
			  [msbus_favicon]},
			 {tag6,
   			  {msbus_static_file, start, []},
			   permanent,
			   10000,
			   worker,
			   [msbus_static_file]},
			 {tag7,
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

			 %% Serviços REST
			 {msbus_user_service,
			  {msbus_user_service, start, []},
			   permanent,
			   10000,
			   worker,  
			   [msbus_user_service]}			  
			   			  
 		    ]  
 		}
 	}.



