%%********************************************************************
%% @title Module ems_odbc_pool
%% @version 1.0.0
%% @doc Module ems_webservice
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_odbc_pool).

-behavior(gen_server). 

-include("../..//include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%% Server API
-export([start/1, start_link/1, stop/0, get_connection/1, sqconnection/0, connection_pool_size/1, release_connection/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  State
-record(state, {connection_by_pool = ?MAX_CONNECTION_BY_POOL}). 


%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
get_connection(Datasource) ->
	case erlang:get("my_connection") of
		undefined ->
			case gen_server:call(?SERVER, {create_connection, Datasource}) of
				{ok, Datasource2} = Result ->
					erlang:put("my_connection",	Datasource2),
					Result;
				Error -> Error
			end;
		DatasourceCache -> 
			{ok, DatasourceCache}
	end.


release_connection(Datasource) ->
	case erlang:erase("my_connection") of
		undefined -> {error, enoent};
		_ -> gen_server:call(?SERVER, {release_connection, Datasource})
	end.


connection_pool_size(Datasource) -> gen_server:call(?SERVER, {get_size, Datasource}).

sqconnection() ->
	#service_datasource{type = sqlite, connection = ?DATABASE_SQLITE_STRING_CONNECTION}.
	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.
    
handle_call({create_connection, Datasource}, {Pid, _Tag}, State) ->
	Reply = create_connection(Datasource, Pid),
	{reply, Reply, State};

handle_call({get_size, Datasource}, _From, State) ->
	Reply = get_connection_pool_size(Datasource),
	{reply, Reply, State};
	
handle_call({release_connection, Datasource}, _From, State) ->
	Reply = release_connection(Datasource, State),
	{reply, Reply, State}.

handle_info(State) ->
   {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid2, _Reason}, State) ->
   Datasource = erlang:erase(Ref),
   release_connection(Datasource, State),
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
						
create_connection(Datasource = #service_datasource{connection = Connection, 
												   timeout = _Timeout}, 
				  PidModule) ->
	PoolName = "pool_" ++ Connection,
	Pool = find_pool(PoolName),
	case queue:len(Pool) of
		0 ->
			Worker = ems_odbc_pool_worker:start(),
			case ems_odbc_pool_worker:connect(Worker, Connection, []) of
				{ok, Conn}	-> 
					Datasource2 = Datasource#service_datasource{conn_ref = Conn, pid_module = Worker},
					MonitorRef = erlang:monitor(process, PidModule),
					erlang:put(MonitorRef, Datasource2),
					{ok, Datasource2};
				Error -> Error
			end;
		_ -> 
			{{value, Datasource2}, Pool2} = queue:out(Pool),
			MonitorRef = erlang:monitor(process, PidModule),
			erlang:put(MonitorRef, Datasource2),
			erlang:put(PoolName, Pool2),
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

release_connection(Datasource = #service_datasource{connection = Connection, 
													conn_ref = Conn}, 
				   #state{connection_by_pool = ConnectionByPool}) ->
	PoolName = "pool_" ++ Connection,
	Pool = find_pool(PoolName),
	case queue:len(Pool) < ConnectionByPool of
		true ->
			io:format("volta pool\n"),
			Pool2 = queue:in(Datasource, Pool),
			erlang:put(PoolName, Pool2);
		false -> 
			io:format("release\n"),
			odbc:disconnect(Conn)
	end.
