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

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3, 
		 last_update/0, is_empty/0, size_table/0, force_load_users/0]).

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
 

last_update() -> ems_db:get_param(<<"ems_user_loader_lastupdate">>).
	
is_empty() -> mnesia:table_info(user, size) == 0.

size_table() -> mnesia:table_info(user, size).

force_load_users() -> 
	gen_server:cast(?SERVER, force_load_users),
	ok.

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{datasource = Datasource, 
			  properties = Props}) ->
	LastUpdate = ems_db:get_param(<<"ems_user_loader_lastupdate">>),
	UpdateCheckpoint = maps:get(<<"update_checkpoint">>, Props, ?USER_LOADER_UPDATE_CHECKPOINT),
	set_force_load_users_checkpoint(),
	State = #state{datasource = Datasource, 
				   update_checkpoint = UpdateCheckpoint,
				   last_update = LastUpdate},
	{ok, State, 2000}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(force_load_users, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	State2 = State#state{last_update = undefined},
	update_or_load_users(State2),
	{noreply, State, UpdateCheckpoint};

handle_cast(_Msg, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{noreply, State, UpdateCheckpoint}.

handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State = #state{update_checkpoint = UpdateCheckpoint}) ->
   {noreply, State, UpdateCheckpoint}.

handle_info(check_force_load_users, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{{_, _, _}, {Hour, _, _}} = calendar:local_time(),
	case Hour >= 4 andalso Hour =< 6 of
		true ->
			ems_logger:info("ems_user_loader force load users checkpoint."),
			State2 = State#state{last_update = undefined},
			case update_or_load_users(State2) of
				{ok, State3} ->
					erlang:send_after(86400 * 1000, self(), check_force_load_users),
					{noreply, State3, UpdateCheckpoint};
				{error, State3} -> 
					erlang:send_after(60000 * 5, self(), check_force_load_users),
					{noreply, State3, UpdateCheckpoint}
			end;
		_ -> 
			set_force_load_users_checkpoint(),
			{noreply, State, UpdateCheckpoint}
	end;

handle_info(timeout, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	{_, State2} = update_or_load_users(State),
	{noreply, State2, UpdateCheckpoint};
	
handle_info({_Pid, {error, Reason}}, State = #state{update_checkpoint = UpdateCheckpoint}) ->
	ems_logger:warn("ems_user_loader is unable to load or update users. Reason: ~p.", [Reason]),
	{noreply, State, UpdateCheckpoint}.
			
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	

%%====================================================================
%% Internal functions
%%====================================================================

set_force_load_users_checkpoint() ->
	erlang:send_after(60000 * 60, self(), check_force_load_users).

update_or_load_users(State = #state{datasource = Datasource,
									last_update = LastUpdate}) ->
	NextUpdate = calendar:local_time(),
	TimestampStr = ems_util:timestamp_str(),
	case is_empty() orelse LastUpdate == undefined of
		true -> 
			ems_logger:info("ems_user_loader checkpoint. operation: load_users."),
			case load_users_from_datasource(Datasource, TimestampStr) of
				ok -> 
					ems_db:set_param(<<"ems_user_loader_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					ems_user_permission_loader:force_load_permissions(),
					{ok, State2};
				_ -> 
					{error, State}
			end;
		false ->
			?DEBUG("ems_user_loader checkpoint. operation: update_users   last_update: ~s.", [ems_util:timestamp_str(LastUpdate)]),
			case update_users_from_datasource(Datasource, LastUpdate, TimestampStr) of
				ok -> 
					ems_db:set_param(<<"ems_user_loader_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					ems_user_permission_loader:update_or_load_permissions(),
					{ok, State2};
				_ -> 
					ems_user_permission_loader:update_or_load_permissions(),
					{error, State}
			end
	end.


load_users_from_datasource(Datasource, CtrlInsert) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				ems_logger:info("ems_user_loader load users from database..."),
				Result = case ems_odbc_pool:param_query(Datasource2, 
														sql_load_users(), 
														[], 
														?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> 
						?DEBUG("ems_user_loader did not load any users."),
						ok;
					{_, _, Records} ->
						case mnesia:clear_table(user) of
							{atomic, ok} ->
								F = fun() ->
									Count = insert_users(Records, 0, CtrlInsert),
									ems_logger:info("ems_user_loader load ~p users.", [Count])
								end,
								mnesia:ets(F),
								mnesia:change_table_copy_type(user, node(), disc_copies),
								ok;
							_ ->
								ems_logger:error("Could not clear user table before load users. Load users cancelled!"),
								{error, efail_load_users}
						end;
					{error, Reason} = Error -> 
						ems_logger:error("ems_user_loader load users query error: ~p.", [Reason]),
						Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("ems_user_loader has no connection to load users from database."),
				Error2
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("ems_user_loader load users error: ~p.", [Reason3]),
			{error, Reason3}
	end.

update_users_from_datasource(Datasource, LastUpdate, CtrlUpdate) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_user_loader got a connection ~p to update users.", [Datasource#service_datasource.id]),
				Sql = sql_update_users(),
				{{Year, Month, Day}, {Hour, Min, _}} = LastUpdate,
				% Zera os segundos para trazer todos os registros alterados no intervalor de 1 min
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
				Result = case ems_odbc_pool:param_query(Datasource2, Sql, Params, ?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> 
						?DEBUG("ems_user_loader did not update any users."),
						ok;
					{_, _, Records} ->
						%?DEBUG("Update users ~p.", [Records]),
						F = fun() ->
							Count = update_users(Records, 0, CtrlUpdate),
							ems_logger:info("ems_user_loader update ~p users since ~s.", [Count, ems_util:timestamp_str(LastUpdate)])
						end,
						mnesia:activity(transaction, F),
						ok;
					{error, Reason} = Error -> 
						ems_logger:error("ems_user_loader update users error: ~p.", [Reason]),
						Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error2 -> 
				ems_logger:warn("ems_user_loader has no connection to update users from database."),
				Error2
		end
	catch
		_Exception:Reason3 -> 
			ems_logger:error("ems_user_loader udpate users error: ~p.", [Reason3]),
			{error, Reason3}
	end.


insert_users([], Count, _CtrlInsert) -> Count;
insert_users([{Codigo, Login, Name, Cpf, Email, Password, Type, PasswdCrypto, TypeEmail, Situacao}|T], Count, CtrlInsert) ->
	PasswdCryptoBin = ?UTF8_STRING(PasswdCrypto),
	User = #user{id = ems_db:sequence(user),
				 codigo = Codigo,
				 login = ?UTF8_STRING(Login),
				 name = ?UTF8_STRING(Name),
				 cpf = ?UTF8_STRING(Cpf),
				 email = ?UTF8_STRING(Email),
				 password = case PasswdCryptoBin of
								<<"SHA1">> -> ?UTF8_STRING(Password);
								_ -> ems_util:criptografia_sha1(Password)
							end,
				 type = Type,
				 passwd_crypto = PasswdCrypto,
				 type_email = TypeEmail,
				 active = Situacao == 1,
				 ctrl_insert = CtrlInsert},
	%?DEBUG("User  ~p\n", [User]),
	mnesia:dirty_write(User),
	insert_users(T, Count+1, CtrlInsert).


update_users([], Count, _CtrlUpdate) -> Count;
update_users([{Codigo, Login, Name, Cpf, Email, Password, Type, PasswdCrypto, TypeEmail, Situacao}|T], Count, CtrlUpdate) ->
	PasswdCryptoBin = ?UTF8_STRING(PasswdCrypto),
	case ems_user:find_by_codigo(Codigo) of
		{ok, User} ->
			PasswdCryptoBin = ?UTF8_STRING(PasswdCrypto),
			User2 = User#user{codigo = Codigo,
							  login = ?UTF8_STRING(Login),
							  name = ?UTF8_STRING(Name),
							  cpf = ?UTF8_STRING(Cpf),
							  email = ?UTF8_STRING(Email),
							  password = case PasswdCryptoBin of
											<<"SHA1">> -> ?UTF8_STRING(Password);
											_ -> ems_util:criptografia_sha1(Password)
										 end,
							  type = Type,
							  passwd_crypto = <<"SHA1">>,
							  type_email = TypeEmail,
							  active = Situacao == 1,
							  ctrl_update = CtrlUpdate};
		{error, enoent} -> 
			User2 = #user{id = ems_db:sequence(user),
						  codigo = Codigo,
						  login = ?UTF8_STRING(Login),
						  name = ?UTF8_STRING(Name),
						  cpf = ?UTF8_STRING(Cpf),
						  email = ?UTF8_STRING(Email),
						  password = case PasswdCryptoBin of
										<<"SHA1">> -> ?UTF8_STRING(Password);
										_ -> ems_util:criptografia_sha1(Password)
									 end,
						  type = Type,
						  passwd_crypto = <<"SHA1">>,
						  type_email = TypeEmail,
						  active = Situacao == 1,
						  ctrl_insert = CtrlUpdate}
	end,
	mnesia:write(User2),
	?DEBUG("ems_user_loader update user: ~p.\n", [User2]),
	update_users(T, Count+1, CtrlUpdate).

sql_load_users() ->	 
	"select top 100000  CodigoPessoa, 
					lower(rtrim(LoginPessoa)) as LoginPessoa, 
					rtrim(NomePessoa) as NomePessoa, 
					rtrim(CpfCnpjPessoa) as CpfCnpjPessoa, 
					lower(rtrim(EmailPessoa)) as EmailPessoa, 
					rtrim(SenhaPessoa) as SenhaPessoa,
					TipoPessoa,
					PasswdCryptoPessoa,
					TipoEmailPessoa,
					1 as SituacaoPessoa
	from (
			-- Busca dados de pessoa física em BDPessoa
			select p.PesCodigoPessoa as CodigoPessoa, 
				   u.UsuLogin as LoginPessoa,
				   p.PesNome as NomePessoa, 
				   cast(p.PesCpf as varchar(14)) as CpfCnpjPessoa, 
				   cast(coalesce(em.EmaEmail, p.PesEmail) as varchar(60)) as EmailPessoa, 
				   cast(u.UsuSenha as varchar(60)) as SenhaPessoa,
				   0 as TipoPessoa,  -- Pessoa física,
				   'SHA1' as PasswdCryptoPessoa,
				   em.EmaTipo as TipoEmailPessoa
			from BDAcesso.dbo.TB_Usuario u join BDPessoa.dbo.TB_Pessoa p
						 on u.UsuPesIdPessoa = p.PesCodigoPessoa
				 left join BDPessoa.dbo.TB_PessoaFisicaEmail pfe
						 on p.PesCodigoPessoa = pfe.PFmPesCodigoPessoa             
				 join BDPessoa.dbo.TB_Email em
						 on pfe.PFmEmaCodigo = em.EmaCodigo
			
			union all
			
			-- Busca dados de alunos em BDSiac com AluRA
			select p.PesCodigoPessoa as CodigoPessoa, 
				   cast(al.AluRA as varchar(100)) as LoginPessoa,
				   p.PesNome as NomePessoa, 
				   cast(coalesce(p.PesCpf, cast(al.AluCPF as varchar(11))) as varchar(14)) as CpfCnpjPessoa, 
				   cast(coalesce(em.EmaEmail, al.AluEmail) as varchar(60)) as EmailPessoa, 
				   cast(al.AluSenha as varchar(60)) as SenhaPessoa,
				   2 as TipoPessoa,  -- Aluno
				   null as PasswdCryptoPessoa,
				   em.EmaTipo as TipoEmailPessoa
			from BDSiac.dbo.TB_Aluno al join BDPessoa.dbo.TB_Pessoa p
						 on al.AluPesCodigoPessoa = p.PesCodigoPessoa
				 left join BDPessoa.dbo.TB_PessoaFisicaEmail pfe
						 on p.PesCodigoPessoa = pfe.PFmPesCodigoPessoa             
				 join BDPessoa.dbo.TB_Email em
						 on pfe.PFmEmaCodigo = em.EmaCodigo 

			union all
			
			-- Busca dados de alunos em BDSiac com AluMatricula
			select p.PesCodigoPessoa as CodigoPessoa, 
				   cast(al.AluMatricula as varchar(100)) as LoginPessoa,
				   p.PesNome as NomePessoa, 
				   cast(coalesce(p.PesCpf, cast(al.AluCPF as varchar(11))) as varchar(14)) as CpfCnpjPessoa, 
				   cast(coalesce(em.EmaEmail, al.AluEmail) as varchar(60)) as EmailPessoa, 
				   cast(al.AluSenha as varchar(60)) as SenhaPessoa,
				   3 as TipoPessoa,  -- Aluno
				   null as PasswdCryptoPessoa,
				   em.EmaTipo as TipoEmailPessoa
			from BDSiac.dbo.TB_Aluno al join BDPessoa.dbo.TB_Pessoa p
						 on al.AluPesCodigoPessoa = p.PesCodigoPessoa
				 left join BDPessoa.dbo.TB_PessoaFisicaEmail pfe
						 on p.PesCodigoPessoa = pfe.PFmPesCodigoPessoa             
				 join BDPessoa.dbo.TB_Email em
						 on pfe.PFmEmaCodigo = em.EmaCodigo
	) as t_users
	order by t_users.TipoPessoa, t_users.TipoEmailPessoa
	".

sql_update_users() ->	 
	"select top 100000 CodigoPessoa, 
					lower(rtrim(LoginPessoa)) as LoginPessoa, 
					rtrim(NomePessoa) as NomePessoa, 
					rtrim(CpfCnpjPessoa) as CpfCnpjPessoa, 
					lower(rtrim(EmailPessoa)) as EmailPessoa, 
					rtrim(SenhaPessoa) as SenhaPessoa,
					TipoPessoa,
					PasswdCryptoPessoa,
					TipoEmailPessoa,
					1 as SituacaoPessoa
	from (
			-- Busca dados de pessoa física em BDPessoa
			select p.PesCodigoPessoa as CodigoPessoa, 
				   u.UsuLogin as LoginPessoa,
				   p.PesNome as NomePessoa, 
				   cast(p.PesCpf as varchar(14)) as CpfCnpjPessoa, 
				   cast(coalesce(em.EmaEmail, p.PesEmail) as varchar(60)) as EmailPessoa, 
				   cast(u.UsuSenha as varchar(60)) as SenhaPessoa,
				   0 as TipoPessoa,  -- Pessoa física,
				   'SHA1' as PasswdCryptoPessoa,
				   em.EmaTipo as TipoEmailPessoa
			from BDAcesso.dbo.TB_Usuario u join BDPessoa.dbo.TB_Pessoa p
						 on u.UsuPesIdPessoa = p.PesCodigoPessoa
				 left join BDPessoa.dbo.TB_PessoaFisicaEmail pfe
						 on p.PesCodigoPessoa = pfe.PFmPesCodigoPessoa             
				 join BDPessoa.dbo.TB_Email em
						 on pfe.PFmEmaCodigo = em.EmaCodigo
			where u.UsuDataAlteracao >= ? or p.PesDataAlteracao >= ? or em.EmaDataAlteracao >= ?
				
			union all
			
			-- Busca dados de alunos em BDSiac com AluRA
			select p.PesCodigoPessoa as CodigoPessoa, 
				   cast(al.AluRA as varchar(100)) as LoginPessoa,
				   p.PesNome as NomePessoa, 
				   cast(coalesce(p.PesCpf, cast(al.AluCPF as varchar(11))) as varchar(14)) as CpfCnpjPessoa, 
				   cast(coalesce(em.EmaEmail, al.AluEmail) as varchar(60)) as EmailPessoa, 
				   cast(al.AluSenha as varchar(60)) as SenhaPessoa,
				   2 as TipoPessoa,  -- Aluno
				   null as PasswdCryptoPessoa,
				   em.EmaTipo as TipoEmailPessoa
			from BDSiac.dbo.TB_Aluno al join BDPessoa.dbo.TB_Pessoa p
						 on al.AluPesCodigoPessoa = p.PesCodigoPessoa
				 left join BDPessoa.dbo.TB_PessoaFisicaEmail pfe
						 on p.PesCodigoPessoa = pfe.PFmPesCodigoPessoa             
				 join BDPessoa.dbo.TB_Email em
						 on pfe.PFmEmaCodigo = em.EmaCodigo 
			where al.AluDataAlteracao >= ? or p.PesDataAlteracao >= ? or em.EmaDataAlteracao >= ?

			union all
			
			-- Busca dados de alunos em BDSiac com AluMatricula
			select p.PesCodigoPessoa as CodigoPessoa, 
				   cast(al.AluMatricula as varchar(100)) as LoginPessoa,
				   p.PesNome as NomePessoa, 
				   cast(coalesce(p.PesCpf, cast(al.AluCPF as varchar(11))) as varchar(14)) as CpfCnpjPessoa, 
				   cast(coalesce(em.EmaEmail, al.AluEmail) as varchar(60)) as EmailPessoa, 
				   cast(al.AluSenha as varchar(60)) as SenhaPessoa,
				   3 as TipoPessoa,  -- Aluno
				   null as PasswdCryptoPessoa,
				   em.EmaTipo as TipoEmailPessoa
			from BDSiac.dbo.TB_Aluno al join BDPessoa.dbo.TB_Pessoa p
						 on al.AluPesCodigoPessoa = p.PesCodigoPessoa
				 left join BDPessoa.dbo.TB_PessoaFisicaEmail pfe
						 on p.PesCodigoPessoa = pfe.PFmPesCodigoPessoa             
				 join BDPessoa.dbo.TB_Email em
						 on pfe.PFmEmaCodigo = em.EmaCodigo 
			where al.AluDataAlteracao >= ? or p.PesDataAlteracao >= ? or em.EmaDataAlteracao >= ?
			
	) as t_users
	order by t_users.TipoPessoa, t_users.TipoEmailPessoa
	".
