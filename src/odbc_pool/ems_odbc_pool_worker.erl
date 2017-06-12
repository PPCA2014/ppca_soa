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
-export([start/1, start_link/1, stop/0, get_datasource/1, last_error/1, notify_use/2, notify_return_pool/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  State
-record(state, {datasource, 
			    last_error, 
			    query_count = 0,
			    check_close_idle_ref,
			    close_idle_ref}). 


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

notify_use(Worker, Datasource) -> gen_server:call(Worker, {notify_use, Datasource}).

notify_return_pool(Worker) -> gen_server:call(Worker, notify_return_pool).

last_error(Worker) -> gen_server:call(Worker, last_error).


 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(Datasource) -> 
	case do_connect(Datasource) of
		{ok, Datasource2} -> 
			{ok, #state{datasource = Datasource2,
						last_error = undefined,
						query_count = 0,
						check_close_idle_ref = undefined,
						close_idle_ref = undefined}};
		_Error -> ignore
	end.
	
    
handle_cast(shutdown, State) ->
	do_disconnect(State),
    {stop, normal, State};


handle_cast(_Msg, State) ->
	{noreply, State}.


handle_call({param_query, Sql, Params}, _From, State = #state{query_count = QueryCount}) ->
	case do_param_query(Sql, Params, State) of
		{ok, Result, Datasource} -> 
			{reply, Result, State#state{datasource = Datasource, 
										last_error = undefined,
										query_count = QueryCount + 1}};
		Error -> 
			{reply, Error, State#state{last_error = Error,
									    query_count = QueryCount + 1}}
	end;


handle_call({notify_use, #service_datasource{pid_module = PidModule, 
											 pid_module_ref = PidModuleRef}}, 
			_From,  
		    State = #state{datasource = InternalDatasource,
						   check_close_idle_ref = CheckCloseIdleRef,
						   close_idle_ref = CloseIdleRef}) ->
	% verifica se a timer para verificar conexão ociosa
	case CheckCloseIdleRef of
		undefined -> 
			% verifica se um timer para fechar a conexão ociosa está em andamento
			case CloseIdleRef of
				undefined -> ok;
				_ -> erlang:cancel_timer(CloseIdleRef)
			end;
		_ -> erlang:cancel_timer(CheckCloseIdleRef)
	end,
	Datasource2 = InternalDatasource#service_datasource{pid_module = PidModule,
														pid_module_ref = PidModuleRef},
	{reply, ok, State#state{datasource = Datasource2,
							last_error = undefined,
							check_close_idle_ref = undefined,
							close_idle_ref = undefined}};

handle_call(notify_return_pool, _From, State = #state{datasource = InternalDatasource, 
													  query_count = QueryCount, 
													  last_error = LastError}) ->
	% volta ao pool somente se nenhum erro ocorreu
	case LastError of
		undefined ->
			CheckCloseIdleRef = erlang:send_after(60000 * 1, self(), {check_close_idle_connection, QueryCount}),
			{reply, ok, State#state{datasource = InternalDatasource#service_datasource{pid_module = undefined,
																					   pid_module_ref = undefined},
									last_error = undefined,
									check_close_idle_ref = CheckCloseIdleRef}};
		_ ->
			{reply, LastError, State}
	end;



handle_call(get_datasource, _From, State) ->
	{reply, State#state.datasource, State};

handle_call(last_error, _From, State) ->
	{reply, State#state.last_error, State}.

handle_info(State) ->
	{noreply, State}.

handle_info({check_close_idle_connection, QueryCount}, State = #state{datasource = #service_datasource{pid_module = undefined}, query_count = QueryCountNow}) ->
	case QueryCountNow > QueryCount of
		true -> 
			{noreply, State};
		false ->
			CloseIdleRef = erlang:send_after(60000 * 1, self(), close_idle_connection),
			{noreply, State#state{close_idle_ref = CloseIdleRef}}
	end;

handle_info({check_close_idle_connection, _QueryCount}, State) ->
   {noreply, State};

handle_info(close_idle_connection, State) ->
   {stop, normal, State};

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, State) ->
    do_disconnect(State),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

    
do_connect(Datasource = #service_datasource{connection = Connection, type = sqlite, driver = <<"sqlite3">>}) -> 
	{ok, ConnRef} = esqlite3:open(Connection),
	Datasource2 = Datasource#service_datasource{owner = self(), 
												conn_ref = ConnRef},
	{ok, Datasource2};
do_connect(Datasource = #service_datasource{connection = Connection}) -> 
	try
		case odbc:connect(Connection, [{scrollable_cursors, on}, {timeout, 12000}, {trace_driver, off}, {extended_errors, off}]) of
			{ok, ConnRef}	-> 
				Datasource2 = Datasource#service_datasource{owner = self(), 
															conn_ref = ConnRef},
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
			ems_logger:error("ems_odbc_pool_worker param_query catch connection timeout: \n\tSQL: ~s \n\tConnection: ~s.", [Sql, Connection]),
			{error, eodbc_connection_timeout};
		_:Reason6 -> 
			ems_logger:error("ems_odbc_pool_worker param_query catch exception: \n\tSQL: ~s \n\tConnection: ~s \n\tReason: ~p.", [Sql, Connection, Reason6]),
			{error, eodbc_invalid_connection}
	end.

    
						
