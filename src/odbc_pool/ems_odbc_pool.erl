%%********************************************************************
%% @title Module ems_odbc_pool
%% @version 1.0.0
%% @doc Provides a pool for ODBC connections
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_odbc_pool).

-behavior(gen_server). 

-include("../..//include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%% Server API
-export([start/1, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-export([get_connection/1, release_connection/1, connection_pool_size/1, param_query/4]).

-define(SERVER, ?MODULE).

%  State
-record(state, {}). 


%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
get_connection(Datasource = #service_datasource{rowid = Rowid}) ->
	case erlang:get(Rowid) of
		undefined ->
			case gen_server:call(?SERVER, {create_connection, Datasource}) of
				{ok, Datasource2} = Result ->
					erlang:put(Rowid, Datasource2),
					Result;
				Error -> Error
			end;
		DatasourceCache -> {ok, DatasourceCache}
	end.


release_connection(Datasource = #service_datasource{rowid = Rowid}) ->
	case erlang:erase(Rowid) of
		undefined -> ok;
		_ -> gen_server:call(?SERVER, {release_connection, Datasource})
	end.


connection_pool_size(Datasource) -> gen_server:call(?SERVER, {get_size, Datasource}).


param_query(#service_datasource{owner = Owner}, Sql, Params, Timeout) ->
	gen_server:call(Owner, {param_query, Sql, Params, Timeout}).

 
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State}.

handle_call({release_connection, Datasource}, _From, State) ->
	do_release_connection(Datasource),
	{reply, ok, State};

handle_call({create_connection, Datasource}, {Pid, _Tag}, State) ->
	Reply = do_create_connection(Datasource, Pid),
	{reply, Reply, State};

handle_call({get_size, Datasource}, _From, State) ->
	Reply = get_connection_pool_size(Datasource),
	{reply, Reply, State}.
	
handle_info(State) ->
   {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid2, _Reason}, State) ->
   case erlang:erase(Ref) of
		undefined -> 
			{noreply, State};
		Datasource ->
			do_release_connection(Datasource),
			{noreply, State}
	end.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

    
%%====================================================================
%% Internal functions
%%====================================================================
    
						
do_create_connection(Datasource = #service_datasource{connection = Connection}, PidModule) ->
	PoolName = "pool_" ++ Connection,
	Pool = find_pool(PoolName),
	case queue:len(Pool) of
		0 ->
			case ems_odbc_pool_worker:start_link(Datasource) of
				{ok, WorkerPid} ->
					ems_logger:info("New odbc connection: ~s.", [Connection]),
					PidModuleRef = erlang:monitor(process, PidModule),
					Datasource2 = Datasource#service_datasource{owner = WorkerPid, 
																pid_module = PidModule,
																pid_module_ref = PidModuleRef},
					erlang:put(PidModuleRef, Datasource2),
					{ok, Datasource2};
				_ -> 
					{error, enoent}
			end;
		_ -> 
			{{value, Datasource2}, Pool2} = queue:out(Pool),
			PidModuleRef = erlang:monitor(process, PidModule),
			Datasource3 = Datasource2#service_datasource{pid_module = PidModule,
														 pid_module_ref = PidModuleRef},
			erlang:put(PidModuleRef, Datasource3),
			erlang:put(PoolName, Pool2),
			{ok, Datasource3}
	end.

do_release_connection(Datasource = #service_datasource{connection = Connection, 
													   owner = Owner, 
													   pid_module_ref = PidModuleRef,
													   max_pool_size = MaxPoolSize}) ->
	erlang:demonitor(PidModuleRef),
	PoolName = "pool_" ++ Connection,
	Pool = find_pool(PoolName),
	case queue:len(Pool) < MaxPoolSize of
		true ->
			Pool2 = queue:in(Datasource#service_datasource{pid_module = undefined, 
														   pid_module_ref = undefined}, Pool),
			erlang:put(PoolName, Pool2),
			io:format("release odbc...\n");
		false -> 
			io:format("shutdown odbc...\n"),
			gen_server:call(Owner, shutdown)
	end.

find_pool(PoolName) ->
	case erlang:get(PoolName) of
		undefined -> queue:new();
		Pool -> Pool
	end.

get_connection_pool_size(#service_datasource{connection = Connection}) ->
	PoolName = "pool_" ++ Connection,
	Pool = find_pool(PoolName),
	queue:len(Pool).

