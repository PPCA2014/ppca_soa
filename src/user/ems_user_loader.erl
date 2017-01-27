%%********************************************************************
%% @title Module ems_user_loader
%% @version 1.0.0
%% @doc ems_user_loader
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_loader).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {datasource,
				operation,
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
 


 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{datasource = Datasource, 
			  properties = Props}) ->
	LastUpdate = ems_db:get_param(<<"ems_user_loader_lastupdate">>),
	UpdateCheckpoint = maps:get(<<"update_checkpoint">>, Props, ?USER_LOADER_UPDATE_CHECKPOINT),
	State = #state{datasource = Datasource, 
				   operation = load_or_update_operation(),
				   update_checkpoint = UpdateCheckpoint,
				   last_update = LastUpdate},
	{ok, State, 5000}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State) ->
   {noreply, State, 1000}.

handle_info(timeout, State = #state{datasource = Datasource,
									operation = update_users,
									update_checkpoint = UpdateCheckpoint,
									last_update = LastUpdate}) ->
	NextUpdate = calendar:local_time(),
	case update_users_from_datasource(Datasource, LastUpdate) of
		ok -> 
			ems_db:set_param(<<"ems_user_loader_lastupdate">>, NextUpdate),
			State2 = State#state{last_update = NextUpdate, 
								  operation = load_or_update_operation()},
			{noreply, State2, UpdateCheckpoint};
		_ -> 
			{noreply, State, UpdateCheckpoint}
	end;
		

handle_info(timeout, State = #state{datasource = Datasource,
									operation = load_users,
									update_checkpoint = UpdateCheckpoint}) ->
	NextUpdate = calendar:local_time(),
	case load_users_from_datasource(Datasource) of
		ok -> 
			ems_db:set_param(<<"ems_user_loader_lastupdate">>, NextUpdate),
			State2 = State#state{operation = load_or_update_operation(),
								 last_update = NextUpdate},
			{noreply, State2, UpdateCheckpoint};
		_ -> 
			{noreply, State, UpdateCheckpoint}
	end;
	
handle_info({_Pid, {error, Reason}}, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	ems_logger:warn("~p não está conseguindo atualizar users. Reason: ~p", [?SERVER, Reason]),
	{noreply, State, UpdateCheckpoint}.
			
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

load_or_update_operation() ->
	case mnesia:table_info(user, size) of
		 0 -> load_users;
		 _ -> update_users
	end.

load_users_from_datasource(Datasource) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				Result = case ems_odbc_pool:param_query(Datasource2, 
														sql_load_users(), 
														[], 
														?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> ok;
					{_, _, Records} ->
						F = fun() ->
							io:format("aqui1\n"),
							Count = insert_users(Records, 0),
							ems_logger:info("~p load ~p users from database", [?SERVER, Count])
						end,
						mnesia:ets(F),
						mnesia:change_table_copy_type(user, node(), disc_copies),
						ok;
					{error, Reason} = Error -> 
						ems_logger:error("~p fail in load users from database. Error: ~p.", [?SERVER, Reason]),
						Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("~p has no connection to load users from database.", [?SERVER]),
				Error2
		end
	catch
		_Exception:Reason3 -> {error, Reason3}
	end.

update_users_from_datasource(Datasource, LastUpdate) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				case LastUpdate of 
					undefined -> 
						Sql = sql_load_users(),
						Params = [];
					_ -> 
						Sql = sql_update_users(),
						{{Year, Month, Day}, {Hour, Min, _}} = LastUpdate,
						% Zera os segundos para trazer todos os registros alterados no intervalor de 1 min
						Params = [{sql_timestamp, [{{Year, Month, Day}, {Hour, Min, 0}}]}]
				end, 
				Result = case ems_odbc_pool:param_query(Datasource2, Sql, Params, ?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> ok;
					{_, _, Records} ->
						%?DEBUG("Update users ~p.", [Records]),
						F = fun() ->
							Count = update_users(Records, 0),
							ems_logger:info("~p update ~p users from database.", [?SERVER, Count])
						end,
						mnesia:activity(transaction, F),
						ok;
					{error, Reason} = Error -> 
						ems_logger:error("~p fail in update users from database. Error: ~p.", [?SERVER, Reason]),
						Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("~p has no connection to update users from database.", [?SERVER]),
				Error2
		end
	catch
		_Exception:Reason3 -> {error, Reason3}
	end.


insert_users([], Count) -> Count;
insert_users([{Codigo, Login, Name, Cpf, Email, Password}|T], Count) ->
	User = #user{id = ems_db:sequence(user),
				 codigo = Codigo,
				 login = ?UTF8_STRING(Login),
				 name = ?UTF8_STRING(Name),
				 cpf = ?UTF8_STRING(Cpf),
				 email = ?UTF8_STRING(Email),
				 password = list_to_binary(Password)},
	mnesia:dirty_write(User),
	insert_users(T, Count+1).


update_users([], Count) -> Count;
update_users([{Codigo, Login, Name, Cpf, Email, Password, Situacao}|T], Count) ->
	case ems_user:find_by_login(Login) of
		{ok, User = #user{id = Id}} ->
			case Situacao of
				1 -> % active
					User2 = User#user{codigo = Codigo,
									  login = ?UTF8_STRING(Login),
									  name = ?UTF8_STRING(Name),
									  cpf = ?UTF8_STRING(Cpf),
									  email = ?UTF8_STRING(Email),
									  password = list_to_binary(Password)},
					mnesia:write(User2);
				_ -> 
					% if inative then delete
					mnesia:delete(Id)
			end;
		{error, enoent} -> 
			User = #user{id = ems_db:sequence(user),
				 codigo = Codigo,
				 login = ?UTF8_STRING(Login),
				 name = ?UTF8_STRING(Name),
				 cpf = ?UTF8_STRING(Cpf),
				 email = ?UTF8_STRING(Email),
				 password = list_to_binary(Password)},
			mnesia:write(User)
	end,
	update_users(T, Count+1).

sql_load_users() ->	 
	"select p.PesCodigoPessoa, rtrim(u.UsuLogin), rtrim(p.PesNome), p.PesCpf, rtrim(p.PesEmail), u.UsuSenha
	 from BDPessoa.dbo.TB_Pessoa p join BDAcesso.dbo.TB_Usuario u on p.PesCodigoPessoa = u.UsuPesIdPessoa
	 where u.UsuSituacao = 1".

sql_update_users() ->	 
	"select p.PesCodigoPessoa, rtrim(u.UsuLogin), rtrim(p.PesNome), p.PesCpf, rtrim(p.PesEmail), u.UsuSenha, u.UsuSituacao
	 from BDPessoa.dbo.TB_Pessoa p join BDAcesso.dbo.TB_Usuario u on p.PesCodigoPessoa = u.UsuPesIdPessoa
	 where u.UsuDataAlteracao >= ?".
