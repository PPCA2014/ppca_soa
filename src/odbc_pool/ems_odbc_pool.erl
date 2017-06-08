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

-export([get_connection/1, release_connection/1, connection_pool_size/1, param_query/2, param_query/3, param_query/4]).

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
 
 
-spec get_connection(#service_datasource{}) -> {ok, #service_datasource{}} | {error, eunavailable_odbc_connection}.
get_connection(Datasource) ->
	try
		case gen_server:call(?SERVER, {create_connection, Datasource}, 12500) of
			{ok, _Datasource2} = Result ->
				?DEBUG("ems_odbc_pool get connection ~p.", [_Datasource2]),
				Result;
			{error, Reason} -> 
				ems_logger:error("ems_odbc_pool get connection error ~p.", [Reason]),
				{error, eunavailable_odbc_connection}
		end
	catch
		_ : _ ->
			ems_logger:error("ems_odbc_pool catch timeout connection to datasource ~p.", [Datasource]),
			{error, eunavailable_odbc_connection}
	end.


release_connection(Datasource) ->
	gen_server:call(?SERVER, {release_connection, Datasource}).

connection_pool_size(Datasource) -> gen_server:call(?SERVER, {get_size, Datasource}).

param_query(#service_datasource{owner = Owner}, Sql) ->
	gen_server:call(Owner, {param_query, Sql, [], undefined}).

param_query(Datasource = #service_datasource{owner = Owner, 
											 timeout = Timeout}, Sql, Params) ->
	try
		gen_server:call(Owner, {param_query, Sql, Params}, Timeout)
	catch
		_ : _ ->
			ems_logger:error("ems_odbc_pool catch param query timeout to datasource ~p.", [Datasource]),
			{error, timeout}
	end.

param_query(Datasource = #service_datasource{owner = Owner}, Sql, Params, Timeout) ->
	try
		gen_server:call(Owner, {param_query, Sql, Params}, Timeout)
	catch
		_ : _ ->
			ems_logger:error("ems_odbc_pool catch param query timeout to datasource ~p.", [Datasource]),
			{error, timeout}
	end.

 
 
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
    
						
do_create_connection(Datasource = #service_datasource{id = Id,
													  table_name = TableName,
													  sql = Sql,
													  primary_key = PrimaryKey}, PidModule) ->
	Pool = find_pool(Id),
	case queue:len(Pool) of
		0 ->
			case ems_odbc_pool_worker:start_link(Datasource) of
				{ok, WorkerPid} ->
					?DEBUG("ems_odbc_pool start new worker for odbc connection ~p.", [Id]),
					PidModuleRef = erlang:monitor(process, PidModule),
					Datasource2 = Datasource#service_datasource{owner = WorkerPid, 
																pid_module = PidModule,
																pid_module_ref = PidModuleRef},
					erlang:put(PidModuleRef, Datasource2),
					{ok, Datasource2};
				_ -> 
					{error, eunavailable_odbc_connection}
			end;
		_ -> 
			{{value, Datasource2}, Pool2} = queue:out(Pool),
			PidModuleRef = erlang:monitor(process, PidModule),
			Datasource3 = Datasource2#service_datasource{pid_module = PidModule,
														 pid_module_ref = PidModuleRef,
														 table_name = TableName,
														 sql = Sql,
														 primary_key = PrimaryKey},
			erlang:put(PidModuleRef, Datasource3),
			erlang:put(Id, Pool2),
			{ok, Datasource3}
	end.

do_release_connection(Datasource = #service_datasource{id = Id,
													   owner = Owner, 
													   pid_module_ref = PidModuleRef,
													   max_pool_size = MaxPoolSize}) ->
	erlang:demonitor(PidModuleRef),
	Pool = find_pool(Id),
	PoolSize = queue:len(Pool),
	case PoolSize < MaxPoolSize of
		true ->
			Pool2 = queue:in(Datasource#service_datasource{pid_module = undefined, 
														   pid_module_ref = undefined}, Pool),
			erlang:put(Id, Pool2),
			?DEBUG("ems_odbc_pool with ~p entry for odbc connection pool ~p.", [PoolSize+1, Id]);
		false -> 
			?DEBUG("ems_odbc_pool shutdown odbc connection ~p.", [Id]),
			gen_server:call(Owner, shutdown)
	end.

find_pool(Id) ->
	case erlang:get(Id) of
		undefined -> queue:new();
		Pool -> Pool
	end.

get_connection_pool_size(#service_datasource{id = Id}) ->
	Pool = find_pool(Id),
	queue:len(Pool).

