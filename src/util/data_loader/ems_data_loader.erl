%%******************************************************************** 
%% @title Module ems_data_loader  
%% @version 1.0.0 %%
%% @doc Module responsible for load records from database
%% @author Everton de Vargas Agilar  <evertonagilar@gmail.com> 
%% @copyright ErlangMS Team 
%%********************************************************************

-module(ems_data_loader).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3, 
		 last_update/1, is_empty/1, size_table/1, sync/1, sync_full/1, pause/1, resume/1]).

% estado do servidor
-record(state, {name,
			    datasource,
				update_checkpoint,
				last_update,
				last_update_param_name,
				sql_load,
				sql_update,
				middleware,
				fields
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
			  properties = Props}) ->
	LastUpdateParamName = erlang:binary_to_atom(maps:get(<<"last_update_param_name">>, Props, <<>>), utf8),
	LastUpdate = ems_db:get_param(LastUpdateParamName),
	UpdateCheckpoint = maps:get(<<"update_checkpoint">>, Props, ?USER_LOADER_UPDATE_CHECKPOINT),
	SqlLoad = binary_to_list(maps:get(<<"sql_load">>, Props, <<>>)),
	SqlUpdate = binary_to_list(maps:get(<<"sql_update">>, Props, <<>>)),
	Fields = maps:get(<<"fields">>, Props, <<>>),
	erlang:send_after(60000 * 60, self(), check_sync_full),
	State = #state{name = binary_to_list(Name),
				   datasource = Datasource, 
				   update_checkpoint = UpdateCheckpoint,
				   last_update_param_name = LastUpdateParamName,
				   last_update = LastUpdate,
				   sql_load = SqlLoad,
				   sql_update = SqlUpdate,
				   middleware = Middleware,
				   fields = Fields},
	{ok, State, StartTimeout}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(sync, State) -> handle_do_check_load_or_update(State);

handle_cast(sync_full, State) -> handle_do_check_load_or_update(State#state{last_update = undefined});

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
													  update_checkpoint = UpdateCheckpoint}) ->
	{{_, _, _}, {Hour, _, _}} = calendar:local_time(),
	case Hour >= 4 andalso Hour =< 6 of
		true ->
			ems_logger:info("~s force load checkpoint.", [Name]),
			State2 = State#state{last_update = undefined},
			case do_check_load_or_update(State2) of
				{ok, State3} ->
					erlang:send_after(86400 * 1000, self(), check_sync_full),
					{noreply, State3, UpdateCheckpoint};
				{error, _Reason} -> 
					erlang:send_after(86400 * 1000, self(), check_sync_full),
					{noreply, State, UpdateCheckpoint}
			end;
		_ -> 
			erlang:send_after(60000 * 60, self(), check_sync_full),
			{noreply, State, UpdateCheckpoint}
	end;

handle_info(timeout, State) -> handle_do_check_load_or_update(State);

handle_info({_Pid, {error, Reason}}, State = #state{name = Name,
													update_checkpoint = UpdateCheckpoint}) ->
	ems_logger:warn("~s is unable to load or update data. Reason: ~p.", [Name, Reason]),
	{noreply, State, UpdateCheckpoint};
			
handle_info(Msg, State = #state{name = Name, update_checkpoint = UpdateCheckpoint}) ->
	ems_logger:warn("~s unknown message: ~p.", [Name, Msg]),
	{noreply, State, UpdateCheckpoint}.
			
terminate(Reason, #service{name = Name}) ->
    ems_logger:warn("~s was terminated. Reason: ~p.", [Name, Reason]),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_do_check_load_or_update(State = #state{name = Name,
									 update_checkpoint = UpdateCheckpoint}) ->
	case do_check_load_or_update(State) of
		{ok, State2} ->	{noreply, State2, UpdateCheckpoint};
		{error, eunavailable_odbc_connection} -> 
			ems_logger:warn("~s wait 5 minutes for next checkpoint while has no database connection.", [Name]),
			{noreply, State};
		_Error -> {noreply, State, UpdateCheckpoint}
	end.
	

%%====================================================================
%% Internal functions
%%====================================================================


do_check_load_or_update(State = #state{name = Name,
									   datasource = Datasource,
									   last_update_param_name = LastUpdateParamName,
									   last_update = LastUpdate}) ->
	% garante que os dados serão atualizados mesmo que as datas não estejam sincronizadas
	NextUpdate = ems_util:date_dec_minute(calendar:local_time(), 6), 
	LastUpdateStr = ems_util:timestamp_str(),
	Conf = ems_config:getConfig(),
	case LastUpdate == undefined orelse do_is_empty(State) of
		true -> 
			?DEBUG("~s load checkpoint.", [Name]),
			case do_load(Datasource, LastUpdateStr, Conf, State) of
				ok -> 
					ems_db:set_param(LastUpdateParamName, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				Error -> Error
			end;
		false ->
			?DEBUG("~s update checkpoint. last update: ~s.", [Name, ems_util:timestamp_str(LastUpdate)]),
			case do_update(Datasource, LastUpdate, LastUpdateStr, Conf, State) of
				ok -> 
					ems_db:set_param(LastUpdateParamName, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				Error -> Error
			end
	end.


-spec do_load(#service_datasource{}, tuple(), #config{}, #state{}) -> ok | {error, atom()}.
do_load(Datasource, CtrlInsert, Conf, State = #state{name = Name,
													 middleware = Middleware,
													 sql_load = SqlLoad,
													 fields = Fields}) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				Result = case ems_odbc_pool:param_query(Datasource2, SqlLoad, []) of
					{_,_,[]} -> 
						ems_odbc_pool:release_connection(Datasource2),
						?DEBUG("~s did not load any record.", [Name]),
						ok;
					{_, _, Records} ->
						ems_odbc_pool:release_connection(Datasource2),
						case do_clear_table(State) of
							ok ->
								do_reset_sequence(State),
								{ok, InsertCount, _, ErrorCount, DisabledCount, SkipCount} = ems_data_pump:data_pump(Records, [], 1, CtrlInsert, Conf, Name, Middleware, insert, 0, 0, 0, 0, 0, db, Fields),
								ems_logger:info("~s sync ~p inserts, ~p disabled, ~p skips, ~p errors.", [Name, InsertCount, DisabledCount, SkipCount, ErrorCount]),
								ok;
							Error ->
								ems_logger:error("~s could not clear table before load data.", [Name]),
								Error
						end;
					{error, Reason2} = Error3 -> 
						ems_odbc_pool:release_connection(Datasource2),
						ems_logger:error("~s load data query error: ~p.", [Name, Reason2]),
						Error3
				end,
				Result;
			Error4 -> 
				ems_logger:warn("~s has no connection to load data from database.", [Name]),
				Error4
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("~s load exception error: ~p.", [Name, Reason3]),
			{error, Reason3}
	end.

-spec do_update(#service_datasource{}, tuple(), tuple(), #config{}, #state{}) -> ok | {error, atom()}.
do_update(Datasource, LastUpdate, CtrlUpdate, Conf, #state{name = Name,
														   middleware = Middleware,
														   sql_update = SqlUpdate,
														   fields = Fields}) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				{{Year, Month, Day}, {Hour, Min, _}} = LastUpdate,
				% Zera os segundos
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
						?DEBUG("~s did not load any record.", [Name]),
						ok;
					{_, _, Records} ->
						ems_odbc_pool:release_connection(Datasource2),
						{ok, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount} = ems_data_pump:data_pump(Records, [], 1, CtrlUpdate, Conf, Name, Middleware, update, 0, 0, 0, 0, 0, db, Fields),
						LastUpdateStr = ems_util:timestamp_str(LastUpdate),
						ems_logger:info("~s sync ~p inserts, ~p updates, ~p disabled, ~p skips, ~p errors since ~s.", [Name, InsertCount, UpdateCount, DisabledCount, SkipCount, ErrorCount, LastUpdateStr]),
						ok;
					{error, Reason} = Error -> 
						ems_odbc_pool:release_connection(Datasource2),
						ems_logger:error("~s update data error: ~p.", [Name, Reason]),
						Error
				end,
				Result;
			Error2 -> 
				ems_logger:warn("~s has no connection to update data from database.", [Name]),
				Error2
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("~s udpate exception error: ~p.", [Name, Reason3]),
			{error, Reason3}
	end.

-spec do_is_empty(#state{}) -> {ok, boolean()}.
do_is_empty(#state{middleware = Middleware}) ->
	apply(Middleware, is_empty, [db]).


-spec do_size_table(#state{}) -> {ok, non_neg_integer()}.
do_size_table(#state{middleware = Middleware}) ->
	apply(Middleware, size_table, [db]).


-spec do_clear_table(#state{}) -> ok | {error, efail_clear_ets_table}.
do_clear_table(#state{middleware = Middleware}) ->
	apply(Middleware, clear_table, [db]).


-spec do_reset_sequence(#state{}) -> ok.
do_reset_sequence(#state{middleware = Middleware}) ->
	apply(Middleware, reset_sequence, [db]).
