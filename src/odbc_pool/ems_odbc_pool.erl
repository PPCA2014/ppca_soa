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
			io:format("process ~p pediu conexao\n", [self()]),
			case gen_server:call(?SERVER, {create_connection, Datasource}) of
				{ok, Datasource2} = Result ->
					erlang:put(Rowid,	Datasource2),
					Result;
				Error -> 
					io:format("erro grave ~p\n", [Error]),
					Error
			end;
		DatasourceCache -> 
			io:format("tava em cache para rowid ~p\n", [Rowid]),
			{ok, DatasourceCache}
	end.


release_connection(Datasource = #service_datasource{rowid = Rowid}) ->
	io:format("libera conexao manual?\n"),
	case erlang:erase(Rowid) of
		undefined -> ok;
		_ -> gen_server:cast(?SERVER, {release_connection, Datasource})
	end.


connection_pool_size(Datasource) -> gen_server:call(?SERVER, {get_size, Datasource}).


param_query(#service_datasource{owner = Owner}, Sql, Params, Timeout) ->
	io:format("chamando param quey para owner ~p\n", [Owner]),
	gen_server:call(Owner, {param_query, Sql, Params, Timeout}).

 
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({release_connection, Datasource}, State) ->
	do_release_connection(Datasource),
	{noreply, State}.

handle_call({create_connection, Datasource}, {Pid, _Tag}, State) ->
	Reply = do_create_connection(Datasource, Pid),
	{reply, Reply, State};

handle_call({get_size, Datasource}, _From, State) ->
	Reply = get_connection_pool_size(Datasource),
	{reply, Reply, State}.
	
handle_info(State) ->
   {noreply, State}.

handle_info({'DOWN', Ref, process, Pid2, Reason}, State) ->
   case erlang:erase(Ref) of
		undefined -> 
			io:format("NAO TAVA DICT!!! ~p\n", [Ref]),
			{noreply, State};
		Datasource ->
			io:format("morreu ~p porque ~p state ~p\n", [Pid2, Reason, State]),
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
	io:format("queye len: ~p\n", [queue:len(Pool)]),
	case queue:len(Pool) of
		0 ->
			case ems_odbc_pool_worker:start_link(Datasource) of
				{ok, WorkerPid} ->
					ems_logger:info("New odbc connection: ~s.", [Connection]),
					Datasource2 = ems_odbc_pool_worker:get_datasource(WorkerPid),
					MonitorRef = erlang:monitor(process, PidModule),
					erlang:put(MonitorRef, Datasource2),
					{ok, Datasource2};
				Error -> 
					ems_logger:info("NAO CONSEGUIU CRIAR WORKER ODBC\n"),
					Error
			end;
		_ -> 
			{{value, Datasource2}, Pool2} = queue:out(Pool),
			MonitorRef = erlang:monitor(process, PidModule),
			erlang:put(MonitorRef, Datasource2),
			erlang:put(PoolName, Pool2),
			
			io:format("reutiliza mas ta vivo: ~p\n", [erlang:is_pid(Datasource2#service_datasource.owner)]), 
			{ok, Datasource2}
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

do_release_connection(Datasource = #service_datasource{connection = Connection, 
													   owner = Owner, 
													   max_pool_size = MaxPoolSize}) ->
	case erlang:is_pid(Owner) of
		true -> 
			PoolName = "pool_" ++ Connection,
			Pool = find_pool(PoolName),
			case queue:len(Pool) < MaxPoolSize of
				true ->
					io:format("release\!!n"),
					Pool2 = queue:in(Datasource, Pool),
					erlang:put(PoolName, Pool2);
				false -> 
					io:format("mandando morrer\!!n"),
					gen_server:cast(Owner, shutdown)
			end;
		false ->
			io:format("tentou liberar uma conexao já liberada!!!\n\n")
	end.


