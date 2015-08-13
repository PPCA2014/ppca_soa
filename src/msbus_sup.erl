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

	{ok, Pools} = application:get_env(msbus, pools),
    PoolSpecs = lists:map(
		fun
			({Name, [{size, SizePool}, _] = SizeArgs, WorkerArgs}) ->
				PoolName = atom_to_list(Name),
				WorkerName = string:substr(PoolName, 1, length(PoolName)-5),
				Worker = list_to_atom(WorkerName),
				case SizePool of
					1 -> 
						io:format("~s iniciado com 1 worker.\n", [WorkerName]),
						{Worker,
							{Worker, start, WorkerArgs},
							permanent, 10000, worker,  [Worker]
						};
					_ ->
						io:format("~s iniciado com ~p workers.\n", [WorkerName, SizePool]),
						PoolArgs = [{name, {local, Name}},
									{worker_module, Worker}] ++ SizeArgs,
						poolboy:child_spec(Name, PoolArgs, WorkerArgs)
				end
		end, Pools),
	
	{ok, {{one_for_one, 10, 10}, PoolSpecs}}.



