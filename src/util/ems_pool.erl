%%********************************************************************
%% @title Módulo ems_pool
%% @version 1.0.0
%% @doc Módulo para gerenciamento de pool de processos. Internamente usa poolboy.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(ems_pool).

-export([child_spec/3, transaction/2, call/2, cast/2, status/0, checkout/1, checkin/2]).

-spec child_spec(PoolId :: term(),
                 PoolArgs :: proplists:proplist(),
                 WorkerArgs :: proplists:proplist()) -> supervisor:child_spec().
child_spec(PoolId, PoolArgs, WorkerArgs) ->
	poolboy:child_spec(PoolId, PoolArgs, WorkerArgs).


-spec transaction(Pool :: poolboy:pool(), Fun :: fun((Worker :: pid()) -> any())) -> any().
transaction(Pool, Fun) ->
	Worker = poolboy:checkout(Pool),
	try
		Ret = Fun(Worker),
		erlang:yield(),
		erlang:yield(),
		erlang:yield(),
		Ret

	after
		poolboy:checkin(Pool, Worker)
	end.

checkout(Pool) -> poolboy:checkout(Pool).

checkin(Pool, Worker) -> poolboy:checkin(Pool, Worker).

cast(Pool, Args) ->
	Worker = poolboy:checkout(Pool),
	try
		gen_server:cast(Worker, Args),
		erlang:yield(),
		erlang:yield(),
		erlang:yield()
	after
		poolboy:checkin(Pool, Worker)
	end.


-spec call(Pool :: poolboy:pool(), Args :: list()) -> any().
call(Pool, Args) ->
	Worker = poolboy:checkout(Pool),
	Result = gen_server:call(Worker, Args),
	ok = poolboy:checkin(Pool, Worker),
	Result.


status() ->
	{ok, Pools} = application:get_env(ems_bus, pools),
    lists:foreach(
		fun
			({Name, [{size, SizePool}, _] = _SizeArgs, _WorkerArgs}) ->
				PoolName = atom_to_list(Name),
				case SizePool of
					1 -> 
						WorkerName = string:substr(PoolName, 1, length(PoolName)-5),
						Worker = list_to_atom(WorkerName),
						io:format("~p is ~p\n", [Name, whereis(Worker)]);
					_ -> io:format("~p is ~p\n", [Name, poolboy:status(list_to_atom(PoolName))])
				end
		end, Pools).

	
