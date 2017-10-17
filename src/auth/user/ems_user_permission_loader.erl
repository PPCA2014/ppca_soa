%%********************************************************************
%% @title Module ems_user_permission_loader
%% @version 1.0.0
%% @doc ems_user_permission_loader
%% @author %% @author Renato Carauta Ribeiro 	<rcarauta6@gmail.com>
%%					  Everton de Vargas Agilar  <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_permission_loader).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3, 
		 last_update/0, is_empty/0, size_table/0, force_load_permissions/0, update_or_load_permissions/0]).

% estado do servidor
-record(state, {datasource,
				last_update,
				sql_load_permissions,
				sql_update_permissions}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Service) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, Service, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

last_update() -> ems_db:get_param(<<"ems_user_permission_loader_lastupdate">>).
	
is_empty() -> mnesia:table_info(user_permission, size) == 0.

size_table() -> mnesia:table_info(user_permission, size).

force_load_permissions() -> 
	gen_server:cast(?SERVER, force_load_permissions),
	ok.

update_or_load_permissions() -> 
	gen_server:cast(?SERVER, update_or_load_permissions),
	ok.

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{datasource = Datasource, properties = Props}) ->
	LastUpdate = ems_db:get_param(<<"ems_user_permission_loader_lastupdate">>),
	SqlLoadPermissions = binary_to_list(maps:get(<<"sql_load_permissions">>, Props, <<>>)),
	SqlUpdatePermissions = binary_to_list(maps:get(<<"sql_update_permissions">>, Props, <<>>)),
	State = #state{datasource = Datasource, 
				   last_update = LastUpdate,
				   sql_load_permissions = SqlLoadPermissions,
				   sql_update_permissions = SqlUpdatePermissions},
	{ok, State}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(update_or_load_permissions, State) ->
	{_Reason, State2} = update_or_load_permissions(State),
	{noreply, State2};

handle_cast(force_load_permissions, State) ->
	State2 = State#state{last_update = undefined},
	{_Reason, State3} = update_or_load_permissions(State2),
	{noreply, State3};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State) ->
   {noreply, State}.

handle_info({_Pid, {error, Reason}}, State) ->
	ems_logger:warn("ems_user_permission_loader is unable to load or update permissions. Reason: ~p.", [Reason]),
	{noreply, State};
			
handle_info(_, State) ->
	{noreply, State}.
			
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	

%%====================================================================
%% Internal functions
%%====================================================================


update_or_load_permissions(State = #state{datasource = Datasource, last_update = LastUpdate}) ->
	NextUpdate = ems_util:date_dec_minute(calendar:local_time(), 6), % garante que os dados serão atualizados mesmo que as datas não estejam sincronizadas
	TimestampStr = ems_util:timestamp_str(),
	case is_empty() orelse LastUpdate == undefined of
		true -> 
			?DEBUG("ems_user_permission_loader checkpoint. operation: load_permissions."),
			case load_permissions_from_datasource(Datasource, TimestampStr, State) of
				ok -> 
					ems_db:set_param(<<"ems_user_permission_loader_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				_ -> 
					{error, State}
			end;
		false ->
			?DEBUG("ems_user_permission_loader checkpoint. operation: update   last_update: ~s.", [ems_util:timestamp_str(LastUpdate)]),
			case update_from_datasource(Datasource, LastUpdate, TimestampStr, State) of
				ok -> 
					ems_db:set_param(<<"ems_user_permission_loader_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				_ -> 
					{error, State}
			end
	end.


load_permissions_from_datasource(Datasource, CtrlInsert, #state{sql_load_permissions = SqlLoadPermissions}) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_user_permission_loader load_permissions_from_datasource use datasource ~p.", [Datasource2#service_datasource.id]),
				Result = case ems_odbc_pool:param_query(Datasource2, SqlLoadPermissions, []) of
					{_,_,[]} -> 
						?DEBUG("ems_user_permission_loader did not load any user permissions."),
						ok;
					{_, _, Records} ->
						case mnesia:clear_table(user_permission) of
							{atomic, ok} ->
								ems_db:init_sequence(user_permission, 0),
								F = fun() ->
									Count = insert(Records, 0, CtrlInsert),
									ems_logger:info("ems_user_permission_loader load ~p user permissions.", [Count])
								end,
								mnesia:activity(transaction, F),
								ok;
							_ ->
								ems_logger:error("ems_user_permission_loader could not clear user_permission table before load user permissions. Load permissions cancelled!"),
								{error, efail_load_permissions}
						end;
					{error, Reason} = Error -> 
						ems_logger:error("ems_user_permission_loader load user permissions query error: ~p.", [Reason]),
						Error
				end,
				ems_odbc_pool:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("ems_user_permission_loader has no connection to load user permissions from database."),
				Error2
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("ems_user_permission_loader load user permissions error: ~p.", [Reason3]),
			{error, Reason3}
	end.

update_from_datasource(Datasource, LastUpdate, CtrlUpdate, #state{sql_update_permissions = SqlUpdatePermissions}) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_user_permission_loader update_from_datasource use datasource ~p.", [Datasource2#service_datasource.id]),
				{{Year, Month, Day}, {Hour, Min, _}} = LastUpdate,
				% Zera os segundos para trazer todos os registros alterados no intervalor de 1 min
				DateInitial = {{Year, Month, Day}, {Hour, Min, 0}},
				Params = [{sql_timestamp, [DateInitial]},
						  {sql_timestamp, [DateInitial]}],
				Result = case ems_odbc_pool:param_query(Datasource2, SqlUpdatePermissions, Params) of
					{_,_,[]} -> 
						?DEBUG("ems_user_permission_loader did not update any user permissions."),
						ok;
					{_, _, Records} ->
						%?DEBUG("Update permissions ~p.", [Records]),
						F = fun() ->
							Count = update(Records, 0, CtrlUpdate),
							ems_logger:info("ems_user_permission_loader update ~p user permissions since ~s.", [Count, ems_util:timestamp_str(LastUpdate)])
						end,
						mnesia:activity(transaction, F),
						ok;
					{error, Reason} = Error -> 
						ems_logger:error("ems_user_permission_loader update user permissions error: ~p.", [Reason]),
						Error
				end,
				ems_odbc_pool:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("ems_user_permission_loader has no connection to user update permissions from database."),
				Error2
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("ems_user_permission_loader udpate user permissions error: ~p.", [Reason3]),
			{error, Reason3}
	end.


insert([], Count, _CtrlInsert) -> Count;
insert([{UserId, GrantGet, GrantPost, GrantPut, GrantDelete, Url, Name, UserId, SisId, PerfilId}|T], Count, CtrlInsert) ->
	Rowid = ems_util:make_rowid(Url),
	Id = ems_db:sequence(user_permission),
	Hash = ems_user_permission:make_hash(Rowid, UserId),
	Hash2 = ems_user_permission:make_hash(Rowid, UserId + PerfilId),
	Permission = #user_permission {
					id = Id,
					hash = Hash,
					hash2 = Hash2,
					name = ?UTF8_STRING(Name),
					url = ?UTF8_STRING(Url),
					grant_get = GrantGet,
					grant_post = GrantPost,
					grant_put = GrantPut,
					user_id = UserId,
					sis_id = SisId,
					perfil_id = PerfilId,
					grant_delete = GrantDelete,
				    ctrl_insert = CtrlInsert
				  },
	mnesia:write(Permission),
	insert(T, Count+1, CtrlInsert).


update([], Count, _CtrlUpdate) -> Count;
update([{UserId, GrantGet, GrantPost, GrantPut, GrantDelete, Url, Name, UserId, SisId, PerfilId}|T], Count, CtrlUpdate) ->
	Rowid = ems_util:make_rowid(Url),
	Hash = ems_user_permission:make_hash(Rowid, UserId),
	Hash2 = ems_user_permission:make_hash(Rowid, UserId + PerfilId),
	case ems_user_permission:find_by_hash2(Hash) of
		{ok, Permission} ->
			Permission2 = Permission#user_permission {
							hash = Hash,
							hash2 = Hash2,
							name = ?UTF8_STRING(Name),
							url = ?UTF8_STRING(Url),
							grant_get = GrantGet,
							grant_post = GrantPost,
							grant_put = GrantPut,
							grant_delete = GrantDelete,
							user_id = UserId,
							sis_id = SisId,
							perfil_id = PerfilId,
							ctrl_update = CtrlUpdate
						};
		{error, enoent} -> 
			Permission2 = #user_permission {
							id = ems_db:sequence(user_permission),
							hash = Hash,
							hash2 = Hash2,
							name = ?UTF8_STRING(Name),
							url = ?UTF8_STRING(Url),
							grant_get = GrantGet,
							grant_post = GrantPost,
							grant_put = GrantPut,
							grant_delete = GrantDelete,
							user_id = UserId,
							sis_id = SisId,
							perfil_id = PerfilId,
							ctrl_insert = CtrlUpdate
						  }
	end,
	mnesia:write(Permission2),
	?DEBUG("ems_user_permission_loader update user permission: ~p.\n", [Permission2]),
	update(T, Count+1, CtrlUpdate).
