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
		fun(#service{name = WorkerName, pool_size = PoolSize, properties = WorkerArgs, module = Worker}) ->
				WorkerPool = list_to_atom(binary_to_list(WorkerName) ++ "_pool"),
				case PoolSize of
					1 -> 
						ems_logger:info("Start ~s with 1 worker", [WorkerName]),
						{Worker,
							{Worker, start, [WorkerArgs]},
							permanent, 10000, worker,  [Worker]
						};
					_ ->
						ems_logger:info("Start ~s with ~p workers", [WorkerName, PoolSize]),
						PoolArgs = [{strategy, fifo},
									{name, {local, WorkerPool}},
									{worker_module, Worker},
									{size, PoolSize}],
						ems_pool:child_spec(atom_to_list(WorkerPool), PoolArgs, WorkerArgs)
				end
		end, KernelServices),
	{ok, {{one_for_one, 10, 10}, PoolSpecs}}.
	

