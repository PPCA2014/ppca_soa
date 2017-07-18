%%********************************************************************
%% @title Module ems_user_control_access_loader
%% @version 1.0.0
%% @doc ems_user_control_access_loader
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_control_access_loader).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3, 
		 last_update/0, is_empty/0, size_table/0, force_load_control_access/0, pause/0, resume/0]).

% estado do servidor
-record(state, {datasource,
				update_checkpoint,
				last_update}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Service) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, Service, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

last_update() -> ems_db:get_param(<<"ems_user_control_access_lastupdate">>).
	
is_empty() -> mnesia:table_info(user_control_access, size) == 0.

size_table() -> mnesia:table_info(user_control_access, size).

force_load_control_access() -> 
	gen_server:cast(?SERVER, force_load_control_access),
	ok.

pause() ->
	gen_server:cast(?SERVER, pause),
	ok.

resume() ->
	gen_server:cast(?SERVER, resume),
	ok.

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{datasource = Datasource, 
			  properties = Props}) ->
	LastUpdate = ems_db:get_param(<<"ems_user_control_access_lastupdate">>),
	UpdateCheckpoint = maps:get(<<"update_checkpoint">>, Props, ?CLIENT_LOADER_UPDATE_CHECKPOINT),
	set_force_load_control_access_checkpoint(),
	State = #state{datasource = Datasource, 
				   update_checkpoint = UpdateCheckpoint,
				   last_update = LastUpdate},
	{ok, State, 12000}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(force_load_control_access, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	State2 = State#state{last_update = undefined},
	update_or_load_control_access(State2),
	{noreply, State, UpdateCheckpoint};

handle_cast(pause, State) ->
	ems_logger:info("ems_user_control_access paused."),
	{noreply, State};

handle_cast(resume, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	ems_logger:info("ems_user_control_access resume."),
	{noreply, State, UpdateCheckpoint};

handle_cast(_Msg, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{noreply, State, UpdateCheckpoint}.

handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State = #state{update_checkpoint = UpdateCheckpoint}) ->
   {noreply, State, UpdateCheckpoint}.

handle_info(check_force_load_control_access, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{{_, _, _}, {Hour, _, _}} = calendar:local_time(),
	case Hour >= 4 andalso Hour =< 6 of
		true ->
			?DEBUG("ems_user_control_access force load access checkpoint."),
			State2 = State#state{last_update = undefined},
			case update_or_load_control_access(State2) of
				{ok, State3} ->
					erlang:send_after(86400 * 1000, self(), check_force_load_control_access),
					{noreply, State3, UpdateCheckpoint};
				{error, State3} -> 
					erlang:send_after(86400 * 1000, self(), check_force_load_control_access),
					{noreply, State3, UpdateCheckpoint}
			end;
		_ -> 
			set_force_load_control_access_checkpoint(),
			{noreply, State, UpdateCheckpoint}
	end;

handle_info(timeout, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{_, State2} = update_or_load_control_access(State),
	{noreply, State2, UpdateCheckpoint};
	
handle_info({_Pid, {error, Reason}}, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	ems_logger:warn("ems_user_control_access is unable to load or update access. Reason: ~p.", [Reason]),
	{noreply, State, UpdateCheckpoint};
			
handle_info(_, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{noreply, State, UpdateCheckpoint}.
			
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	

%%====================================================================
%% Internal functions
%%====================================================================

set_force_load_control_access_checkpoint() ->
	erlang:send_after(60000 * 60, self(), check_force_load_control_access).

update_or_load_control_access(State = #state{datasource = Datasource,
									  last_update = LastUpdate}) ->
	NextUpdate = ems_util:date_dec_minute(calendar:local_time(), 6), % garante que os dados serão atualizados mesmo que as datas não estejam sincronizadas
	TimestampStr = ems_util:timestamp_str(),
	case is_empty() orelse LastUpdate == undefined of
		true -> 
			?DEBUG("ems_user_control_access checkpoint. operation: access."),
			case load_control_access_from_datasource(Datasource, TimestampStr) of
				ok -> 
					ems_db:set_param(<<"ems_user_control_access_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				_ -> 
					{error, State}
			end;
		false ->
			?DEBUG("ems_user_control_access checkpoint. operation: update_control_access   last_update: ~s.", [ems_util:timestamp_str(LastUpdate)]),
			case update_control_access_from_datasource(Datasource, LastUpdate, TimestampStr) of
				ok -> 
					ems_db:set_param(<<"ems_user_control_access_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				_ -> 
					{error, State}
			end
	end.


load_control_access_from_datasource(Datasource, CtrlInsert) -> 
%%	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_user_control_access load access from database..."),
				Result = case ems_odbc_pool:param_query(Datasource2, 
														sql_load_control_access(), 
														[], 
														?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> 
						?DEBUG("ems_user_control_access did not load any access."),
						ok;
					{_, _, Records} ->
						case mnesia:clear_table(user_control_access) of
							{atomic, ok} ->
								ems_db:init_sequence(user_control_access, 0),
								F = fun() ->
									Count = insert_control_access(Records, 0, CtrlInsert),
									ems_logger:info("ems_user_control_access load ~p access.", [Count])
								end,
								mnesia:ets(F),
								mnesia:change_table_copy_type(user_control_access, node(), disc_copies),
								ok;
							_ ->
								ems_logger:error("ems_user_control_access could not clear access table before load access. Load access cancelled!"),
								{error, efail_load_access}
						end;
					{error, Reason} = Error -> 
						ems_logger:error("ems_user_control_access load access query error: ~p.", [Reason]),
						Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("ems_user_control_access has no connection to load access from database."),
				Error2
		end.
%%	catch
	%%	_Exception:Reason3 -> 
		%%	ems_logger:error("ems_user_control_access load access error: ~p.", [Reason3]),
		%%	{error, Reason3}
%%	end.

update_control_access_from_datasource(Datasource, LastUpdate, CtrlUpdate) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_user_control_access got a connection ~p to update access.", [Datasource#service_datasource.id]),
				Sql = sql_load_control_access(),
				%{{Year, Month, Day}, {Hour, Min, _}} = LastUpdate,
				% Zera os segundos para trazer todos os registros alterados no intervalor de 1 min
				%DateInitial = {{Year, Month, Day}, {Hour, Min, 0}},
				Params = [],
				Result = case ems_odbc_pool:param_query(Datasource2, Sql, Params, ?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> 
						?DEBUG("ems_user_control_access did not update any access."),
						ok;
					{_, _, Records} ->
						%?DEBUG("Update access ~p.", [Records]),
						F = fun() ->
							Count = update_control_access(Records, 0, CtrlUpdate),
							ems_logger:info("ems_user_control_access update ~p access since ~s.", [Count, ems_util:timestamp_str(LastUpdate)])
						end,
						mnesia:activity(transaction, F),
						ok;
					{error, Reason} = Error -> 
						ems_logger:error("ems_user_control_access update accesss error: ~p.", [Reason]),
						Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("ems_user_control_access has no connection to update access from database."),
				Error2
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("ems_user_control_access udpate access error: ~p.", [Reason3]),
			{error, Reason3}
	end.


insert_control_access([], Count, _CtrlInsert) -> Count;
insert_control_access([{Codigo, Name, Uri, UserId, SisId, Visualize }|T], Count, CtrlInsert) ->
	UserControl = #user_control_access{id = ems_db:sequence(user_control_access),
					 codigo = Codigo,
					 user_id = UserId,
					 sis_id = SisId,
					 visualize = Visualize,
				     name = ?UTF8_STRING(Name),
				     uri = ?UTF8_STRING(Uri),
				     ctrl_insert = CtrlInsert},
	mnesia:dirty_write(UserControl),
	insert_control_access(T, Count+1, CtrlInsert).


update_control_access([], Count, _CtrlUpdate) -> Count;
update_control_access([{Codigo, Name, Uri, UserId, SisId, Visualize}|T], Count, CtrlUpdate) ->
	case ems_user_control_access:find_by_codigo(Codigo) of
		{ok, Client} ->
			Client2 = Client#user_control_access{name = ?UTF8_STRING(Name),
									uri = ?UTF8_STRING(Uri),
									user_id = UserId,
									visualize = Visualize,
									ctrl_update = CtrlUpdate};
		{error, enoent} -> 
			Client2 = #user_control_access{id = ems_db:sequence(user_control_access),
						     codigo = Codigo,
						     user_id = UserId,
						     sis_id = SisId,
							 name = ?UTF8_STRING(Name),
							 uri = ?UTF8_STRING(Uri),
							 ctrl_insert = CtrlUpdate}
	end,
	mnesia:write(Client2),
	?DEBUG("ems_user_control_access update client: ~p.\n", [Client2]),
	update_control_access(T, Count+1, CtrlUpdate).


sql_load_control_access() ->	 
  "select distinct  t.TraId as codigo, t.TraNomeMenu as name ,t.TraNomeFrm as uri, 
		u.UsuPesIdPessoa as user_id,  s.SisId as sis_id, t.TraVisualizar as visualize
	    from BDAcesso.dbo.TB_Usuario u 
		inner join BDAcesso.dbo.TB_Acessos a 
				on u.UsuId = a.AceUsuId
		inner join BDAcesso.dbo.TB_Sistemas si
		on a.AceSisId = si.SisId
		inner join BDAcesso.dbo.TB_Acessos_Perfil up  
				on u.UsuId = up.APeUsuId 
		inner join BDAcesso.dbo.TB_Perfil p 
				on up.APePerId = p.PerId 
		inner join BDAcesso.dbo.TB_Perfil_Transacao pt 
				on p.PerId = pt.PTrPerId 
	    inner join BDAcesso.dbo.TB_Transacao t 
				on pt.PTrTraId = t.TraId 
		inner join BDAcesso.dbo.TB_Sistemas s 
				on s.SisId = t.TraSisId;".


