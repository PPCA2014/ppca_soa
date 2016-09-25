-module(ems_bus_sup).

-behaviour(supervisor).

-include("../include/ems_schema.hrl").

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
	KernelServices = ems_catalog_loader:list_kernel_catalog(),
    PoolSpecs = lists:map(
		fun(#service{name = WorkerName, 
					 pool_size = PoolSize, 
					 pool_max = PoolMax, 
					 properties = WorkerArgs, 
					 module = Worker}) ->
				case PoolSize == 1 andalso PoolMax == 1 of
					true -> 
						ems_logger:info("Start ~s with 1 worker", [WorkerName]),
						{Worker,
							{Worker, start, [WorkerArgs]},
							permanent, 10000, worker,  [Worker]
						};
					false ->
						ems_logger:info("Start ~s with ~p workers (Max ~p)", [WorkerName, PoolSize, PoolMax]),
						PoolArgs = [{strategy, fifo},
									{name, {local, Worker}},
									{worker_module, Worker},
									{size, PoolSize},
									{max_overflow, PoolMax - PoolSize}],
						ems_pool:child_spec(Worker, PoolArgs, WorkerArgs)
				end
		end, KernelServices),
	{ok, {{one_for_one, 10, 10}, PoolSpecs}}.
	

