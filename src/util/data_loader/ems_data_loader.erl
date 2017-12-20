%%******************************************************************** 
%% @title Module ems_data_loader  
%% @version 1.0.0 %%
%% @doc Module responsible for load records from database
%% @author Everton de Vargas Agilar  <evertonagilar@gmail.com> 
%% @copyright ErlangMS Team 
%%********************************************************************

-module(ems_data_loader).

-behavior(gen_server). 

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3, 
		 last_update/1, is_empty/1, size_table/1, sync/1, sync_full/1, pause/1, resume/1]).

% estado do servidor
-record(state, {name,
			    datasource,
				update_checkpoint,
				timeout_on_error,
				last_update,
				last_update_param_name,
				sql_load,
				sql_update,
				sql_count,
				sql_codigos,
				middleware,
				fields,
				check_count_checkpoint_metric_name,
				check_remove_checkpoint_metric_name,
				sync_full_checkpoint_metric_name,
				load_checkpoint_metric_name,
				update_checkpoint_metric_name,
				error_checkpoint_metric_name,
				insert_metric_name,
				update_metric_name,
				update_miss_metric_name,
				error_metric_name,
				disable_metric_name,
				skip_metric_name,
				source_type,
				async
			}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Service = #service{name = Name}) -> 
   	ServerName = erlang:binary_to_atom(Name, utf8),
    gen_server:start_link({local, ServerName}, ?MODULE, Service, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
last_update(Server) -> gen_server:call(Server, last_update).
	
is_empty(Server) -> gen_server:call(Server, is_empty).

size_table(Server) -> gen_server:call(Server, size_table).

sync(Server) -> 
	gen_server:cast(Server, sync),
	ok.

sync_full(Server) -> 
	gen_server:cast(Server, sync_full),
	ok.

pause(Server) ->
	gen_server:cast(Server, pause),
	ok.

resume(Server) ->
	gen_server:cast(Server, resume),
	ok.

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{name = Name, 
			  datasource = Datasource, 
			  middleware = Middleware, 
			  start_timeout = StartTimeout,
			  properties = Props,
			  async = Async}) ->
	LastUpdateParamName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_last_update_param_name">>]), utf8),
	LastUpdate = ems_db:get_param(LastUpdateParamName),
	CheckRemoveRecords = ems_util:parse_bool(maps:get(<<"check_remove_records">>, Props, false)),
	CheckRemoveRecordsCheckpoint = maps:get(<<"check_remove_records_checkpoint">>, Props, ?DATA_LOADER_UPDATE_CHECKPOINT + 120000),
	UpdateCheckpoint = maps:get(<<"update_checkpoint">>, Props, ?DATA_LOADER_UPDATE_CHECKPOINT),
	SqlLoad = string:trim(binary_to_list(maps:get(<<"sql_load">>, Props, <<>>))),
	SqlUpdate = string:trim(binary_to_list(maps:get(<<"sql_update">>, Props, <<>>))),
	SqlCount = re:replace(SqlLoad, "select(.+)( from.+)( order by.+)","select count(1)\\2", [{return,list}]),
	SqlCodigos = re:replace(SqlLoad, "select ([^,]+),(.+)( from.+)( order by.+)", "select \\1 \\3", [{return,list}]),
	Fields = maps:get(<<"fields">>, Props, <<>>),
	SourceType = binary_to_atom(maps:get(<<"source_type">>, Props, <<"db">>), utf8),
	TimeoutOnError = maps:get(<<"timeout_on_error">>, Props, 60000 * 6),
	SyncFullCheckpointMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_full_checkpoint">>]), utf8),
	CheckCountCheckpointMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_check_count_checkpoint">>]), utf8),
	CheckRemoveCheckpointMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_check_remove_checkpoint">>]), utf8),
	LoadCheckpointMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_load_checkpoint">>]), utf8),
	UpdateCheckpointMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_update_checkpoint">>]), utf8),
	ErrorCheckpointMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_error_checkpoint">>]), utf8),
	InsertMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_inserts">>]), utf8),
	UpdateMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_updates">>]), utf8),
	UpdateMissMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_update_miss">>]), utf8),
	ErrorsMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_errors">>]), utf8),
	DisabledMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_disabled">>]), utf8),
	SkipMetricName = erlang:binary_to_atom(iolist_to_binary([Name, <<"_skip">>]), utf8),
	erlang:send_after(60000 * 60, self(), check_sync_full),
	case CheckRemoveRecords of
		true -> erlang:send_after(CheckRemoveRecordsCheckpoint, self(), check_count_records);
		false -> ok
	end,
	State = #state{name = binary_to_list(Name),
				   datasource = Datasource, 
				   update_checkpoint = UpdateCheckpoint,
				   last_update_param_name = LastUpdateParamName,
				   last_update = LastUpdate,
				   sql_load = SqlLoad,
				   sql_update = SqlUpdate,
				   sql_count = SqlCount,
				   sql_codigos = SqlCodigos,
				   middleware = Middleware,
				   fields = Fields,
				   timeout_on_error = TimeoutOnError,
   				   check_count_checkpoint_metric_name = CheckCountCheckpointMetricName,
   				   check_remove_checkpoint_metric_name = CheckRemoveCheckpointMetricName,
   				   sync_full_checkpoint_metric_name = SyncFullCheckpointMetricName,
   				   load_checkpoint_metric_name = LoadCheckpointMetricName,
				   update_checkpoint_metric_name = UpdateCheckpointMetricName,
				   error_checkpoint_metric_name = ErrorCheckpointMetricName,
				   insert_metric_name = InsertMetricName,
				   update_metric_name = UpdateMetricName,
				   update_miss_metric_name = UpdateMissMetricName,
				   error_metric_name = ErrorsMetricName,
				   disable_metric_name = DisabledMetricName,
				   skip_metric_name = SkipMetricName,
				   source_type = SourceType,
				   async = Async},
	{ok, State, StartTimeout}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(sync, State) -> handle_do_check_load_or_update_checkpoint(State);

handle_cast(sync_full, State = #state{sync_full_checkpoint_metric_name = SyncFullCheckpointMetricName}) -> 
	ems_db:inc_counter(SyncFullCheckpointMetricName),
	handle_do_check_load_or_update_checkpoint(State#state{last_update = undefined});

handle_cast(pause, State = #state{name = Name}) ->
	ems_logger:info("~s paused.", [Name]),
	{noreply, State};

handle_cast(resume, State = #state{name = Name,
								   update_checkpoint = UpdateCheckpoint}) ->
	ems_logger:info("~s resume.", [Name]),
	{noreply, State, UpdateCheckpoint};

handle_cast(_Msg, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{noreply, State, UpdateCheckpoint}.

handle_call(last_update, _From, State = #state{last_update_param_name = LastUpdateParamName}) ->
	Reply = {ok, ems_db:get_param(LastUpdateParamName)},
	{reply, Reply, State};

handle_call(is_empty, _From, State) ->
	Reply = {ok, do_is_empty(State)},
	{reply, Reply, State};

handle_call(size_table, _From, State) ->
	Reply = {ok, do_size_table(State)},
	{reply, Reply, State};

handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State = #state{update_checkpoint = UpdateCheckpoint}) ->
   {noreply, State, UpdateCheckpoint}.

handle_info(check_sync_full, State = #state{name = Name,
											update_checkpoint = UpdateCheckpoint,
											timeout_on_error = TimeoutOnError,
											sync_full_checkpoint_metric_name = SyncFullCheckpointMetricName,
											error_checkpoint_metric_name = ErrorCheckpointMetricName
										}) ->
		{{_, _, _}, {Hour, _, _}} = calendar:local_time(),
		case Hour >= 4 andalso Hour =< 6 of
			true ->
				case do_permission_to_execute(check_sync_full, Name, State) of
					true ->
						ems_db:inc_counter(SyncFullCheckpointMetricName),
						ems_logger:info("~s sync full checkpoint.", [Name]),
						State2 = State#state{last_update = undefined},
						case do_check_load_or_update_checkpoint(State2) of
							{ok, State3} ->
								erlang:send_after(86400 * 1000, self(), check_sync_full),
								ems_data_loader_ctl:notify_finish_work(check_sync_full, Name),
								{noreply, State3, UpdateCheckpoint};
							_Error -> 
								ems_db:inc_counter(ErrorCheckpointMetricName),
								erlang:send_after(86400 * 1000, self(), check_sync_full),
								ems_logger:warn("~s wait ~p minutes for next checkpoint while has database connection error.", [Name, trunc(TimeoutOnError / 60000)]),
								ems_data_loader_ctl:notify_finish_work(check_sync_full, Name),
								{noreply, State, TimeoutOnError}
						end;
					false ->
						erlang:send_after(30000, self(), check_sync_full), % aguarda 30 segundos e tente novamente
						{noreply, State, UpdateCheckpoint}
				end;
			_ -> 
				erlang:send_after(60000 * 60, self(), check_sync_full),
				{noreply, State, UpdateCheckpoint}
		end;

handle_info(timeout, State) -> 
	handle_do_check_load_or_update_checkpoint(State);

handle_info(check_count_records, State = #state{name = Name,
												update_checkpoint = UpdateCheckpoint,
											    timeout_on_error = TimeoutOnError}) ->
	case do_permission_to_execute(check_count_records, Name, State) of
		true ->
			case do_check_count_checkpoint(State) of
				ok -> 
					erlang:send_after(60000, self(), check_count_records),
					ems_data_loader_ctl:notify_finish_work(check_count_records, Name),
					{noreply, State, UpdateCheckpoint};
				_ -> 
					erlang:send_after(60000, self(), check_count_records),
					ems_data_loader_ctl:notify_finish_work(check_count_records, Name),
					{noreply, State, TimeoutOnError}
			end;
		false ->
			{noreply, State, 6000}
	end;

handle_info({_Pid, {error, _Reason}}, State = #state{timeout_on_error = TimeoutOnError}) ->
	{noreply, State, TimeoutOnError};
			
handle_info(_Msg, State = #state{timeout_on_error = TimeoutOnError}) ->
	{noreply, State, TimeoutOnError}.
		
terminate(Reason, #service{name = Name}) ->
    ems_logger:warn("~s was terminated. Reason: ~p.", [Name, Reason]),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_do_check_load_or_update_checkpoint(State = #state{name = Name,
														 update_checkpoint = UpdateCheckpoint,
														 timeout_on_error = TimeoutOnError,
														 error_checkpoint_metric_name = ErrorCheckpointMetricName}) ->
	case do_permission_to_execute(check_load_or_update_checkpoint, Name, State) of
		true ->
			case do_check_load_or_update_checkpoint(State) of
				{ok, State2} ->
					ems_data_loader_ctl:notify_finish_work(check_load_or_update_checkpoint, Name),
					{noreply, State2, UpdateCheckpoint};
				_Error -> 
					ems_db:inc_counter(ErrorCheckpointMetricName),
					ems_logger:warn("~s wait ~p minutes for next checkpoint while has database connection error.", [Name, trunc(TimeoutOnError / 60000)]),
					ems_data_loader_ctl:notify_finish_work(check_load_or_update_checkpoint, Name),
					{noreply, State, TimeoutOnError}
			end;
		false ->
			{noreply, State, 1000}	
	end.
	

%%====================================================================
%% Internal functions
%%====================================================================


do_check_count_checkpoint(State = #state{name = Name,
										 datasource = Datasource,
										 sql_count = SqlCount,
										 sql_codigos = SqlCodigos,
										 check_count_checkpoint_metric_name = CheckCountCheckpointMetricName,
										 check_remove_checkpoint_metric_name = CheckRemoveCheckpointMetricName}) ->
	try
		ems_db:inc_counter(CheckCountCheckpointMetricName),
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				Result = case ems_odbc_pool:param_query(Datasource2, SqlCount, []) of
					{_, _, [{Count}]} ->
						case Count < do_size_table(State) of
							true -> 
								ems_db:inc_counter(CheckRemoveCheckpointMetricName),
								case ems_odbc_pool:param_query(Datasource2, SqlCodigos, []) of
									{_, _, Result2} ->
										ems_odbc_pool:release_connection(Datasource2),
										Codigos = [N || {N} <- Result2],
										RemoveCount = do_check_remove_records(Codigos, State),
										case RemoveCount > 0 of
											true -> ems_logger:info("~s sync ~p deletes.", [Name, RemoveCount]);
											false -> ok
										end;
									Error3 -> 
										ems_odbc_pool:release_connection(Datasource2),
										Error3
								end;
							false -> 
								ems_odbc_pool:release_connection(Datasource2),
								ok
						end;
					Error4 -> 
						ems_odbc_pool:release_connection(Datasource2),
						Error4
				end,
				erlang:garbage_collect(),
				Result;
			Error5 -> 
				?DEBUG("~s has no connection to check counts.", [Name]),
				Error5
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("~s check count exception error: ~p.", [Name, Reason3]),
			{error, Reason3}
	end.


do_check_load_or_update_checkpoint(State = #state{name = Name,
									   last_update_param_name = LastUpdateParamName,
									   last_update = LastUpdate}) ->
	% garante que os dados serão atualizados mesmo que as datas não estejam sincronizadas
	NextUpdate = ems_util:date_dec_minute(calendar:local_time(), 6), 
	LastUpdateStr = ems_util:timestamp_str(),
	Conf = ems_config:getConfig(),
	case LastUpdate == undefined orelse do_is_empty(State) of
		true -> 
			?DEBUG("~s load checkpoint.", [Name]),
			case do_load(LastUpdateStr, Conf, State) of
				ok -> 
					ems_db:set_param(LastUpdateParamName, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				Error -> Error
			end;
		false ->
			?DEBUG("~s update checkpoint. last update: ~s.", [Name, ems_util:timestamp_str(LastUpdate)]),
			case do_update(LastUpdate, LastUpdateStr, Conf, State) of
				ok -> 
					ems_db:set_param(LastUpdateParamName, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				Error -> Error
			end
	end.


-spec do_load(tuple(), #config{}, #state{}) -> ok | {error, atom()}.
do_load(CtrlInsert, Conf, State = #state{datasource = Datasource,
										 name = Name,
										 middleware = Middleware,
										 sql_load = SqlLoad,
										 fields = Fields,
										 load_checkpoint_metric_name = LoadCheckpointMetricName,
										 insert_metric_name = InsertMetricName,
										 error_metric_name = ErrorsMetricName,
										 disable_metric_name = DisabledMetricName,
										 skip_metric_name = SkipMetricName,
										 source_type = SourceType}) -> 
	try
		ems_db:inc_counter(LoadCheckpointMetricName),
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				Result = case ems_odbc_pool:param_query(Datasource2, SqlLoad, []) of
					{_, _, Records} ->
						ems_odbc_pool:release_connection(Datasource2),
						case do_clear_table(State) of
							ok ->
								do_reset_sequence(State),
								{ok, InsertCount, _, ErrorCount, DisabledCount, SkipCount} = ems_data_pump:data_pump(Records, CtrlInsert, Conf, Name, Middleware, insert, 0, 0, 0, 0, 0, SourceType, Fields),
								ems_logger:info("~s sync ~p inserts, ~p disabled, ~p skips, ~p errors.", [Name, InsertCount, DisabledCount, SkipCount, ErrorCount]),
								ems_db:counter(InsertMetricName, InsertCount),
								ems_db:counter(ErrorsMetricName, ErrorCount),
								ems_db:counter(DisabledMetricName, DisabledCount),
								ems_db:counter(SkipMetricName, SkipCount),
								ok;
							Error ->
								ems_logger:error("~s could not clear table before load data.", [Name]),
								Error
						end;
					Error3 -> 
						ems_odbc_pool:release_connection(Datasource2),
						Error3
				end,
				erlang:garbage_collect(),
				Result;
			Error4 -> 
				?DEBUG("~s has no connection to load data from database.", [Name]),
				Error4
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("~s load exception error: ~p.", [Name, Reason3]),
			{error, Reason3}
	end.

-spec do_update(tuple(), tuple(), #config{}, #state{}) -> ok | {error, atom()}.
do_update(LastUpdate, CtrlUpdate, Conf, #state{datasource = Datasource,
											   name = Name,
											   middleware = Middleware,
											   sql_update = SqlUpdate,
											   fields = Fields,
											   update_checkpoint_metric_name = UpdateCheckpointMetricName,
											   insert_metric_name = InsertMetricName,
											   update_metric_name = UpdateMetricName,
											   update_miss_metric_name = UpdateMissMetricName,
											   error_metric_name = ErrorsMetricName,
											   disable_metric_name = DisabledMetricName,
											   skip_metric_name = SkipMetricName,
											   source_type = SourceType}) -> 
	try
		% do_update is optional
		case SqlUpdate =/= "" of
			true ->
				ems_db:inc_counter(UpdateCheckpointMetricName),
				case ems_odbc_pool:get_connection(Datasource) of
					{ok, Datasource2} -> 
						{{Year, Month, Day}, {Hour, Min, _}} = LastUpdate,
						DateInitial = {{Year, Month, Day}, {Hour, Min, 0}},
						Params = [{sql_timestamp, [DateInitial]},
								  {sql_timestamp, [DateInitial]},
								  {sql_timestamp, [DateInitial]},
								  {sql_timestamp, [DateInitial]},
								  {sql_timestamp, [DateInitial]},
								  {sql_timestamp, [DateInitial]},
								  {sql_timestamp, [DateInitial]},
								  {sql_timestamp, [DateInitial]},
								  {sql_timestamp, [DateInitial]}],
						Result = case ems_odbc_pool:param_query(Datasource2, SqlUpdate, Params) of
							{_,_,[]} -> 
								ems_odbc_pool:release_connection(Datasource2),
								ems_db:inc_counter(UpdateMissMetricName),
								ok;
							{_, _, Records} ->
								ems_odbc_pool:release_connection(Datasource2),
								{ok, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount} = ems_data_pump:data_pump(Records, CtrlUpdate, Conf, Name, Middleware, update, 0, 0, 0, 0, 0, SourceType, Fields),
								% Para não gerar muito log, apenas imprime no log se algum registro foi modificado
								case InsertCount > 0 orelse UpdateCount > 0 orelse ErrorCount > 0 orelse DisabledCount > 0 of
									true ->
										ems_db:counter(InsertMetricName, InsertCount),
										ems_db:counter(UpdateMetricName, UpdateCount),
										ems_db:counter(ErrorsMetricName, ErrorCount),
										ems_db:counter(DisabledMetricName, DisabledCount),
										LastUpdateStr = ems_util:timestamp_str(LastUpdate),
										ems_logger:info("~s sync ~p inserts, ~p updates, ~p disabled, ~p skips, ~p errors since ~s.", [Name, InsertCount, UpdateCount, DisabledCount, SkipCount, ErrorCount, LastUpdateStr]);
									false -> ok
								end,
								ems_db:counter(SkipMetricName, SkipCount),
								ok;
							Error -> 
								ems_odbc_pool:release_connection(Datasource2),
								Error
						end,
						erlang:garbage_collect(),
						Result;
					Error2 -> 
						?DEBUG("~s has no connection to update data from database.", [Name]),
						Error2
				end;
			_ -> ok
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("~s udpate exception error: ~p.", [Name, Reason3]),
			{error, Reason3}
	end.

-spec do_is_empty(#state{}) -> {ok, boolean()}.
do_is_empty(#state{middleware = Middleware, source_type = SourceType}) ->
	apply(Middleware, is_empty, [SourceType]).


-spec do_size_table(#state{}) -> {ok, non_neg_integer()}.
do_size_table(#state{middleware = Middleware, source_type = SourceType}) ->
	apply(Middleware, size_table, [SourceType]).


-spec do_clear_table(#state{}) -> ok | {error, efail_clear_ets_table}.
do_clear_table(#state{middleware = Middleware, source_type = SourceType}) ->
	apply(Middleware, clear_table, [SourceType]).


-spec do_reset_sequence(#state{}) -> ok.
do_reset_sequence(#state{middleware = Middleware, source_type = SourceType}) ->
	apply(Middleware, reset_sequence, [SourceType]).


-spec do_check_remove_records(list(), #state{}) -> non_neg_integer().
do_check_remove_records(Codigos, #state{middleware = Middleware, source_type = SourceType}) ->
	apply(Middleware, check_remove_records, [Codigos, SourceType]).

-spec do_permission_to_execute(atom(), atom(), #state{}) -> boolean().
do_permission_to_execute(_, _, #state{async = true}) -> 
	true;
do_permission_to_execute(What, ProcessName, _) -> 
	ems_data_loader_ctl:permission_to_execute(What, ProcessName).
		

