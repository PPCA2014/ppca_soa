%%********************************************************************
%% @title Module ems_user_perfil_loader
%% @version 1.0.0
%% @doc ems_user_perfil_loader
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_perfil_loader).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3, 
		 last_update/0, is_empty/0, size_table/0, force_load_perfil/0, update_or_load_perfil/0]).

% estado do servidor
-record(state, {datasource,
				last_update}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Service) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, Service, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

last_update() -> ems_db:get_param(<<"ems_user_perfil_loader_lastupdate">>).
	
is_empty() -> mnesia:table_info(user_perfil, size) == 0.

size_table() -> mnesia:table_info(user_perfil, size).

force_load_perfil() -> 
	gen_server:cast(?SERVER, force_load_perfil),
	ok.

update_or_load_perfil() -> 
	gen_server:cast(?SERVER, update_or_load_perfil),
	ok.

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{datasource = Datasource}) ->
	LastUpdate = ems_db:get_param(<<"ems_user_perfil_loader_lastupdate">>),
	State = #state{datasource = Datasource, 
				   last_update = LastUpdate},
	{ok, State}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(update_or_load_perfil, State) ->
	{_Reason, State2} = update_or_load_perfil(State),
	{noreply, State2};

handle_cast(force_load_perfil, State) ->
	State2 = State#state{last_update = undefined},
	{_Reason, State3} = update_or_load_perfil(State2),
	{noreply, State3};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State) ->
   {noreply, State}.

handle_info({_Pid, {error, Reason}}, State) ->
	ems_logger:warn("ems_user_perfil_loader is unable to load or update perfil. Reason: ~p.", [Reason]),
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


update_or_load_perfil(State = #state{datasource = Datasource,
										  last_update = LastUpdate}) ->
	NextUpdate = ems_util:date_dec_minute(calendar:local_time(), 6), % garante que os dados serão atualizados mesmo que as datas não estejam sincronizadas
	TimestampStr = ems_util:timestamp_str(),
	case is_empty() orelse LastUpdate == undefined of
		true -> 
			?DEBUG("ems_user_perfil_loader checkpoint. operation: load_perfil."),
			case load_perfil_from_datasource(Datasource, TimestampStr) of
				ok -> 
					ems_db:set_param(<<"ems_user_perfil_loader_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				_ -> 
					{error, State}
			end;
		false ->
			?DEBUG("ems_user_perfil_loader checkpoint. operation: update   last_update: ~s.", [ems_util:timestamp_str(LastUpdate)]),
			case update_from_datasource(Datasource, LastUpdate, TimestampStr) of
				ok -> 
					ems_db:set_param(<<"ems_user_perfil_loader_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				_ -> 
					{error, State}
			end
	end.


load_perfil_from_datasource(Datasource, CtrlInsert) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_user_perfil_loader load user perfil from database..."),
				Result = case ems_odbc_pool:param_query(Datasource2, 
														sql_load_perfil(), 
														[], 
														?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> 
						?DEBUG("ems_user_perfil_loader did not load any user perfil."),
						ok;
					{_, _, Records} ->
						case mnesia:clear_table(user_perfil) of
							{atomic, ok} ->
								ems_db:init_sequence(user_perfil, 0),
								F = fun() ->
									Count = insert(Records, 0, CtrlInsert),
									ems_logger:info("ems_user_perfil_loader load ~p user perfil.", [Count])
								end,
								mnesia:activity(transaction, F),
								ok;
							_ ->
								ems_logger:error("ems_user_perfil_loader could not clear user_perfil table before load user perfil. Load perfil cancelled!"),
								{error, efail_load_perfil}
						end;
					{error, Reason} = Error -> 
						ems_logger:error("ems_user_perfil_loader load user perfil query error: ~p.", [Reason]),
						Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("ems_user_perfil_loader has no connection to load user perfil from database."),
				Error2
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("ems_user_perfil_loader load user perfil error: ~p.", [Reason3]),
			{error, Reason3}
	end.

update_from_datasource(Datasource, LastUpdate, CtrlUpdate) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_user_perfil_loader got a connection ~p to update user perfil.", [Datasource#service_datasource.id]),
				Sql = sql_update_perfil(),
				{{Year, Month, Day}, {Hour, Min, _}} = LastUpdate,
				% Zera os segundos para trazer todos os registros alterados no intervalor de 1 min
				DateInitial = {{Year, Month, Day}, {Hour, Min, 0}},
				Params = [{sql_timestamp, [DateInitial]},
						  {sql_timestamp, [DateInitial]}],
				Result = case ems_odbc_pool:param_query(Datasource2, Sql, Params, ?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> 
						?DEBUG("ems_user_perfil_loader did not update any user perfil."),
						ok;
					{_, _, Records} ->
						%?DEBUG("Update perfil ~p.", [Records]),
						F = fun() ->
							Count = update(Records, 0, CtrlUpdate),
							ems_logger:info("ems_user_perfil_loader update ~p user perfil since ~s.", [Count, ems_util:timestamp_str(LastUpdate)])
						end,
						mnesia:activity(transaction, F),
						ok;
					{error, Reason} = Error -> 
						ems_logger:error("ems_user_perfil_loader update user perfil error: ~p.", [Reason]),
						Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("ems_user_perfil_loader has no connection to user update perfil from database."),
				Error2
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("ems_user_perfil_loader udpate user perfil error: ~p.", [Reason3]),
			{error, Reason3}
	end.


insert([], Count, _CtrlInsert) -> Count;
insert([{UserId, PerfilId, Name, Description}|T], Count, CtrlInsert) ->
	Perfil = #user_perfil {
					id = ems_db:sequence(user_perfil),
					user_id = UserId,
					perfil_id = PerfilId,
					name = ?UTF8_STRING(Name),
					description = ?UTF8_STRING(Description),
				    ctrl_insert = CtrlInsert
				  },
	mnesia:write(Perfil),
	insert(T, Count+1, CtrlInsert).


update([], Count, _CtrlUpdate) -> Count;
update([{UserId, PerfilId, Name, Description}|T], Count, CtrlUpdate) ->
	case ems_user_perfil:find(UserId, PerfilId) of
		{ok, Perfil} ->
			Perfil2 = Perfil#user_perfil {
							name = ?UTF8_STRING(Name),
							description = ?UTF8_STRING(Description),
							ctrl_update = CtrlUpdate
						};
		{error, enoent} -> 
			Perfil2 = #user_perfil {
							id = ems_db:sequence(user_perfil),
							user_id = UserId,
							perfil_id = PerfilId,
							name = ?UTF8_STRING(Name),
							description = ?UTF8_STRING(Description),
							ctrl_insert = CtrlUpdate
						  }
	end,
	mnesia:write(Perfil2),
	?DEBUG("ems_user_perfil_loader update user perfil: ~p.\n", [Perfil2]),
	update(T, Count+1, CtrlUpdate).


sql_load_perfil() ->	 
  "select u.UsuPesIdPessoa as UserId,
        p.PerId,
        p.PerNome,
        p.PerDescricao
	from BDAcesso.dbo.TB_Usuario u join BDAcesso.dbo.TB_Acessos_Perfil up  
							on u.UsuId = up.APeUsuId 
			inner join BDAcesso.dbo.TB_Perfil p 
							on up.APePerId = p.PerId
	group by u.UsuPesIdPessoa, p.PerId, p.PerNome, p.PerDescricao
	order by u.UsuPesIdPessoa
  ".

sql_update_perfil() ->	 
  "select u.UsuPesIdPessoa as UserId,
        p.PerId,
        p.PerNome,
        p.PerDescricao
	from BDAcesso.dbo.TB_Usuario u join BDAcesso.dbo.TB_Acessos_Perfil up  
							on u.UsuId = up.APeUsuId 
			inner join BDAcesso.dbo.TB_Perfil p 
							on up.APePerId = p.PerId
	where p.PerDataAlteracao >= ? or up.APeDataAlteracao >= ?
	group by u.UsuPesIdPessoa, p.PerId, p.PerNome, p.PerDescricao
	order by u.UsuPesIdPessoa
	".
