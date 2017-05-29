%%********************************************************************
%% @title Module ems_client_loader
%% @version 1.0.0
%% @doc ems_client_loader
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_client_loader).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3, 
		 last_update/0, is_empty/0, size_table/0, force_load_clients/0, pause/0, resume/0]).

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
 

last_update() -> ems_db:get_param(<<"ems_client_loader_lastupdate">>).
	
is_empty() -> mnesia:table_info(client, size) == 0.

size_table() -> mnesia:table_info(client, size).

force_load_clients() -> 
	gen_server:cast(?SERVER, force_load_clients),
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
	LastUpdate = ems_db:get_param(<<"ems_client_loader_lastupdate">>),
	UpdateCheckpoint = maps:get(<<"update_checkpoint">>, Props, ?CLIENT_LOADER_UPDATE_CHECKPOINT),
	set_force_load_clients_checkpoint(),
	State = #state{datasource = Datasource, 
				   update_checkpoint = UpdateCheckpoint,
				   last_update = LastUpdate},
	{ok, State, 2000}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(force_load_clients, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	State2 = State#state{last_update = undefined},
	update_or_load_clients(State2),
	{noreply, State, UpdateCheckpoint};

handle_cast(pause, State) ->
	ems_logger:info("ems_client_loader paused."),
	{noreply, State};

handle_cast(resume, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	ems_logger:info("ems_client_loader resume."),
	{noreply, State, UpdateCheckpoint};

handle_cast(_Msg, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{noreply, State, UpdateCheckpoint}.

handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State = #state{update_checkpoint = UpdateCheckpoint}) ->
   {noreply, State, UpdateCheckpoint}.

handle_info(check_force_load_clients, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{{_, _, _}, {Hour, _, _}} = calendar:local_time(),
	case Hour >= 4 andalso Hour =< 6 of
		true ->
			?DEBUG("ems_client_loader force load clients checkpoint."),
			State2 = State#state{last_update = undefined},
			case update_or_load_clients(State2) of
				{ok, State3} ->
					erlang:send_after(86400 * 1000, self(), check_force_load_clients),
					{noreply, State3, UpdateCheckpoint};
				{error, State3} -> 
					erlang:send_after(60000 * 5, self(), check_force_load_clients),
					{noreply, State3, UpdateCheckpoint}
			end;
		_ -> 
			set_force_load_clients_checkpoint(),
			{noreply, State, UpdateCheckpoint}
	end;

handle_info(timeout, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{_, State2} = update_or_load_clients(State),
	{noreply, State2, UpdateCheckpoint};
	
handle_info({_Pid, {error, Reason}}, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	ems_logger:warn("ems_client_loader is unable to load or update clients. Reason: ~p.", [Reason]),
	{noreply, State, UpdateCheckpoint}.
			
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	

%%====================================================================
%% Internal functions
%%====================================================================

set_force_load_clients_checkpoint() ->
	erlang:send_after(60000 * 60, self(), check_force_load_clients).

update_or_load_clients(State = #state{datasource = Datasource,
									  last_update = LastUpdate}) ->
	NextUpdate = ems_util:date_dec_minute(calendar:local_time(), 6), % garante que os dados serão atualizados mesmo que as datas não estejam sincronizadas
	TimestampStr = ems_util:timestamp_str(),
	case is_empty() orelse LastUpdate == undefined of
		true -> 
			?DEBUG("ems_client_loader checkpoint. operation: load_clients."),
			case load_clients_from_datasource(Datasource, TimestampStr) of
				ok -> 
					ems_db:set_param(<<"ems_client_loader_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				_ -> 
					{error, State}
			end;
		false ->
			?DEBUG("ems_client_loader checkpoint. operation: update_clients   last_update: ~s.", [ems_util:timestamp_str(LastUpdate)]),
			case update_clients_from_datasource(Datasource, LastUpdate, TimestampStr) of
				ok -> 
					ems_db:set_param(<<"ems_client_loader_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					{ok, State2};
				_ -> 
					{error, State}
			end
	end.


load_clients_from_datasource(Datasource, CtrlInsert) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_client_loader load clients from database..."),
				Result = case ems_odbc_pool:param_query(Datasource2, 
														sql_load_clients(), 
														[], 
														?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> 
						?DEBUG("ems_client_loader did not load any clients."),
						ok;
					{_, _, Records} ->
						case mnesia:clear_table(client) of
							{atomic, ok} ->
								ems_db:init_sequence(client, 0),
								F = fun() ->
									Count = insert_clients(Records, 0, CtrlInsert),
									ems_logger:info("ems_client_loader load ~p clients.", [Count])
								end,
								mnesia:ets(F),
								mnesia:change_table_copy_type(client, node(), disc_copies),
								ok;
							_ ->
								ems_logger:error("ems_client_loader could not clear client table before load clients. Load clients cancelled!"),
								{error, efail_load_clients}
						end;
					{error, Reason} = Error -> 
						ems_logger:error("ems_client_loader load clients query error: ~p.", [Reason]),
						Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("ems_client_loader has no connection to load clients from database."),
				Error2
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("ems_client_loader load clients error: ~p.", [Reason3]),
			{error, Reason3}
	end.

update_clients_from_datasource(Datasource, LastUpdate, CtrlUpdate) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_client_loader got a connection ~p to update clients.", [Datasource#service_datasource.id]),
				Sql = sql_load_clients(),
				%{{Year, Month, Day}, {Hour, Min, _}} = LastUpdate,
				% Zera os segundos para trazer todos os registros alterados no intervalor de 1 min
				%DateInitial = {{Year, Month, Day}, {Hour, Min, 0}},
				Params = [],
				Result = case ems_odbc_pool:param_query(Datasource2, Sql, Params, ?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> 
						?DEBUG("ems_client_loader did not update any clients."),
						ok;
					{_, _, Records} ->
						%?DEBUG("Update clients ~p.", [Records]),
						F = fun() ->
							Count = update_clients(Records, 0, CtrlUpdate),
							ems_logger:info("ems_client_loader update ~p clients since ~s.", [Count, ems_util:timestamp_str(LastUpdate)])
						end,
						mnesia:activity(transaction, F),
						ok;
					{error, Reason} = Error -> 
						ems_logger:error("ems_client_loader update clients error: ~p.", [Reason]),
						Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("ems_client_loader has no connection to update clients from database."),
				Error2
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("ems_client_loader udpate clients error: ~p.", [Reason3]),
			{error, Reason3}
	end.


insert_clients([], Count, _CtrlInsert) -> Count;
insert_clients([{Codigo, Name, Secret, RedirectUri, Description, Active}|T], Count, CtrlInsert) ->
	Client = #client{id = ems_db:sequence(client),
				     codigo = erlang:integer_to_binary(Codigo),
				     name = ?UTF8_STRING(Name),
				     secret = ?UTF8_STRING(Secret),
				     redirect_uri = ?UTF8_STRING(RedirectUri),
				     description = ?UTF8_STRING(Description),
				     scope = <<>>,
				     active = Active,
				     ctrl_insert = CtrlInsert},
	mnesia:dirty_write(Client),
	insert_clients(T, Count+1, CtrlInsert).


update_clients([], Count, _CtrlUpdate) -> Count;
update_clients([{Codigo, Name, Secret, RedirectUri, Description, Active}|T], Count, CtrlUpdate) ->
	case ems_client:find_by_codigo(Codigo) of
		{ok, Client} ->
			Client2 = Client#client{name = ?UTF8_STRING(Name),
									secret = ?UTF8_STRING(Secret),
									redirect_uri = ?UTF8_STRING(RedirectUri),
									description = ?UTF8_STRING(Description),
									scope = <<>>,
									active = Active,
									ctrl_update = CtrlUpdate};
		{error, enoent} -> 
			Client2 = #client{id = ems_db:sequence(client),
							 codigo = ?UTF8_STRING(Codigo),
							 name = ?UTF8_STRING(Name),
							 secret = ?UTF8_STRING(Secret),
							 redirect_uri = ?UTF8_STRING(RedirectUri),
							 description = ?UTF8_STRING(Description),
							 scope = <<>>,
							 active = Active,
							 ctrl_insert = CtrlUpdate}
	end,
	mnesia:write(Client2),
	?DEBUG("ems_client_loader update client: ~p.\n", [Client2]),
	update_clients(T, Count+1, CtrlUpdate).


sql_load_clients() ->	 
	"select s.SisId as Codigo,
       s.SisSistema as Name,
       s.SisOrgao as Secret,
       s.SisUrl as RedirectUri,
       s.SisDescricao as Description,
       s.SisSituacao as Active
	from BDAcesso.dbo.TB_Sistemas s
	where s.SisUrl is not null
	".

