%%********************************************************************
%% @title Módulo msbus_pool
%% @version 1.0.0
%% @doc Módulo para gerenciamento de pool de processos. Internamente usa poolboy.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_pool).

-export([child_spec/3, transaction/2, call/2, cast/2, status/0]).

-spec child_spec(PoolId :: term(),
                 PoolArgs :: proplists:proplist(),
                 WorkerArgs :: proplists:proplist()) -> supervisor:child_spec().
child_spec(PoolId, PoolArgs, WorkerArgs) ->
	poolboy:child_spec(PoolId, PoolArgs, WorkerArgs).


-spec transaction(Pool :: poolboy:pool(), Fun :: fun((Worker :: pid()) -> any())) -> any().
transaction(Pool, Fun) ->
	poolboy:transaction(Pool, Fun).

cast(Pool, Args) ->
	Worker = poolboy:checkout(Pool),
	gen_server:cast(Worker, Args),
	%true = msbus_util:sleep(1),
	ok = poolboy:checkin(Pool, Worker).


-spec call(Pool :: poolboy:pool(), Args :: list()) -> any().
call(Pool, Args) ->
	Worker = poolboy:checkout(Pool),
	Result = gen_server:call(Worker, Args),
	ok = poolboy:checkin(Pool, Worker),
	Result.


status() ->
	{ok, Pools} = application:get_env(msbus, pools),
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

	
