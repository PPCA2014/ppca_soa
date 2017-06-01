%%********************************************************************
%% @title Module ems_odbc_pool_worker
%% @version 1.0.0
%% @doc Module ems_odbc_pool_worker
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_odbc_pool_worker).

-behavior(gen_server). 

-include("../..//include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%% Server API
-export([start/1, start_link/1, stop/0, get_datasource/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  State
-record(state, {datasource}). 


%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
get_datasource(Worker) -> gen_server:call(Worker, get_datasource).

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(Datasource) -> 
	case do_connect(Datasource) of
		{ok, Datasource2} -> {ok, #state{datasource = Datasource2}};
		_Error -> ignore
	end.
	
    
handle_cast(shutdown, State) ->
	do_disconnect(State),
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.
    
handle_call({param_query, Sql, Params}, _From, State) ->
	case do_param_query(Sql, Params, State) of
		{ok, Result, Datasource} -> 
			{reply, Result, #state{datasource = Datasource}};
		Error -> 
			{reply, Error, State}
	end;

handle_call(get_datasource, _From, State) ->
	{reply, State#state.datasource, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

    
do_connect(Datasource = #service_datasource{connection = Connection, type = sqlite, driver = <<"sqlite3">>}) -> 
	{ok, ConnRef} = esqlite3:open(Connection),
	Datasource2 = Datasource#service_datasource{owner = self(), conn_ref = ConnRef},
	{ok, Datasource2};
do_connect(Datasource = #service_datasource{connection = Connection}) -> 
	try
		case odbc:connect(Connection, [{scrollable_cursors, on}, {timeout, 12000}, {trace_driver, off}, {extended_errors, off}]) of
			{ok, ConnRef}	-> 
				Datasource2 = Datasource#service_datasource{owner = self(), conn_ref = ConnRef},
				{ok, Datasource2};
			{error, {PosixError, _}} -> 
				ems_logger:error("ems_odbc_pool_worker invalid posix odbc connection: ~s. Reason: ~p.", [Connection, ems_tcp_util:posix_error_description(PosixError)]),
				{error, PosixError};
			{error, Reason} -> 
				ems_logger:error("ems_odbc_pool_worker invalid odbc connection: ~s. Reason: ~p.", [Connection, Reason]),
				{error, Reason}
		end
	catch 
		_Exception:{PosixError2, _} -> 
			ems_logger:error("ems_odbc_pool_worker invalid posix odbc connection: ~s. Reason: ~p.", [Connection, ems_tcp_util:posix_error_description(PosixError2)]),
			{error, PosixError2}
	end.

do_disconnect(#state{datasource = #service_datasource{conn_ref = ConnRef, type = sqlite, driver = <<"sqlite3">>}}) -> 
	esqlite3:close(ConnRef);
do_disconnect(#state{datasource = #service_datasource{conn_ref = ConnRef}}) -> 
	odbc:disconnect(ConnRef).

do_param_query(Sql, Params, #state{datasource = Datasource = #service_datasource{conn_ref = ConnRef,
																			     type = sqlite,
																				 driver = <<"sqlite3">>}}) ->
	Params2 = [hd(V) || {_, V} <- Params],
	case esqlite3:prepare(Sql, ConnRef) of
        {ok, Statement} ->
            ok = esqlite3:bind(Statement, Params2),
            Records = esqlite3:fetchall(Statement),
			Fields = tuple_to_list(esqlite3:column_names(Statement)),
			Fields2 = [?UTF8_STRING(erlang:atom_to_binary(F, utf8)) || F <- Fields],
			%?DEBUG("Sqlite resultset query: ~p.", [Records]),
			{ok, {selected, Fields2, Records}, Datasource};
        Error -> Error
    end;
do_param_query(Sql, Params, #state{datasource = Datasource = #service_datasource{conn_ref = ConnRef,
																				 connection = Connection,
																				 timeout = Timeout}}) ->
	try
		case odbc:param_query(ConnRef, Sql, Params, Timeout) of
			{error, Reason} ->
				ems_logger:error("ems_odbc_pool_worker param_query error: \n\tSQL: ~s \n\tConnection: ~s \n\tReason: ~p.", [Sql, Connection, Reason]),
				{error, eodbc_connection_closed};
			{selected, Fields1, Result1} -> 
				%?DEBUG("Odbc resultset query: ~p.", [Result1]),
				{ok, {selected, [?UTF8_STRING(F) || F <- Fields1], Result1}, Datasource}
		end
	catch
		_:timeout -> 
			ems_logger:error("ems_odbc_pool_worker param_query connection timeout: \n\tSQL: ~s \n\tConnection: ~s.", [Sql, Connection]),
			{error, eodbc_connection_timeout};
		_:Reason6 -> 
			ems_logger:error("ems_odbc_pool_worker param_query catch exception: \n\tSQL: ~s \n\tConnection: ~s \n\tReason: ~p.", [Sql, Connection, Reason6]),
			{error, eodbc_invalid_connection}
	end.

    
						
