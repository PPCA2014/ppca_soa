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
				last_update,
				allow_load_aluno}).

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
	AllowLoadAluno = maps:get(<<"allow_load_aluno">>, Props, false),
	set_force_load_users_checkpoint(),
	State = #state{datasource = Datasource, 
				   update_checkpoint = UpdateCheckpoint,
				   last_update = LastUpdate,
				   allow_load_aluno = AllowLoadAluno},
	{ok, State, 2500}.
    
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
	NextUpdate = ems_util:date_dec_minute(calendar:local_time(), 6), % garante que os dados serão atualizados mesmo que as datas não estejam sincronizadas
	TimestampStr = ems_util:timestamp_str(),
	case is_empty() orelse LastUpdate == undefined of
		true -> 
			?DEBUG("ems_user_loader checkpoint. operation: load_users."),
			case load_users_from_datasource(Datasource, TimestampStr, State) of
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
			case update_users_from_datasource(Datasource, LastUpdate, TimestampStr, State) of
				ok -> 
					ems_db:set_param(<<"ems_user_loader_lastupdate">>, NextUpdate),
					State2 = State#state{last_update = NextUpdate},
					ems_user_permission_loader:update_or_load_permissions(),
					{ok, State2};
				_ -> 
					{error, State}
			end
	end.


load_users_from_datasource(Datasource, CtrlInsert, #state{allow_load_aluno = AllowLoadAluno}) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_user_loader load users from database..."),
				Result = case ems_odbc_pool:param_query(Datasource2, 
														sql_load_users_tipo_pessoa(), 
														[]) of
					{_,_,[]} -> 
						?DEBUG("ems_user_loader did not load any users tipo pessoa."),
						ok;
					{_, _, Records} ->
						case mnesia:clear_table(user) of
							{atomic, ok} ->
								ems_db:init_sequence(user, 0),
								InsertUserPessoaFunc = fun() ->
									Count = insert_users(Records, 0, CtrlInsert),
									ems_logger:info("ems_user_loader load ~p users tipo pessoa.", [Count])
								end,
								mnesia:activity(transaction, InsertUserPessoaFunc),
								case AllowLoadAluno of
									true ->
										case ems_odbc_pool:param_query(Datasource2, 
																		sql_load_users_tipo_aluno(), 
																		[]) of
											{_,_,[]} -> 
												?DEBUG("ems_user_loader did not load any users tipo aluno."),
												ok;
											{_, _, Records} ->
												InsertUserAlunoFunc = fun() ->
													Count = insert_users(Records, 0, CtrlInsert),
													ems_logger:info("ems_user_loader load ~p users tipo aluno.", [Count])
												end,
												mnesia:activity(transaction, InsertUserAlunoFunc),
												ok;
											{error, Reason} = Error -> 
												ems_logger:error("ems_user_loader load users query error: ~p.", [Reason]),
												Error
										end;
									false -> ok
								end,
								erlang:garbage_collect(),
								ok;
							_ ->
								ems_logger:error("ems_user_loader could not clear user table before load users. Load users cancelled!"),
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

update_users_from_datasource(Datasource, LastUpdate, CtrlUpdate, #state{allow_load_aluno = AllowLoadAluno}) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				?DEBUG("ems_user_loader got a connection ~p to update users.", [Datasource#service_datasource.id]),
				{{Year, Month, Day}, {Hour, Min, _}} = LastUpdate,
				% Zera os segundos
				DateInitial = {{Year, Month, Day}, {Hour, Min, 0}},
				Params = [{sql_timestamp, [DateInitial]},
						  {sql_timestamp, [DateInitial]},
						  {sql_timestamp, [DateInitial]}],
				Result = case ems_odbc_pool:param_query(Datasource2, sql_update_users_tipo_pessoa(), Params) of
					{_,_,[]} -> 
						?DEBUG("ems_user_loader did not update any users tipo pessoa."),
						ok;
					{_, _, Records} ->
						%?DEBUG("Update users ~p.", [Records]),
						UpdatePessoaFunc = fun() ->
							Count = update_users(Records, 0, CtrlUpdate),
							ems_logger:info("ems_user_loader update ~p users tipo pessoa since ~s.", [Count, ems_util:timestamp_str(LastUpdate)])
						end,
						mnesia:activity(transaction, UpdatePessoaFunc),
						case AllowLoadAluno of
							true ->
								case ems_odbc_pool:param_query(Datasource2, sql_update_users_tipo_aluno(), Params) of
									{_,_,[]} -> 
										?DEBUG("ems_user_loader did not update any users tipo aluno."),
										ok;
									{_, _, Records} ->
										%?DEBUG("Update users ~p.", [Records]),
										UpdateAlunoFunc = fun() ->
											Count = update_users(Records, 0, CtrlUpdate),
											ems_logger:info("ems_user_loader update ~p users tipo aluno since ~s.", [Count, ems_util:timestamp_str(LastUpdate)])
										end,
										mnesia:activity(transaction, UpdateAlunoFunc);
									{error, Reason} = Error -> 
										ems_logger:error("ems_user_loader update users tipo aluno error: ~p.", [Reason]),
										Error
								end;		
							false -> ok
						end;
					{error, Reason} = Error -> 
						ems_logger:error("ems_user_loader update users tipo pessoa error: ~p.", [Reason]),
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
insert_users([{Codigo, Login, Name, Cpf, Email, Password, Type, PasswdCrypto, 
			   TypeEmail, Active, Endereco, ComplementoEndereco, Bairro, 
			   Cidade, Uf, Cep, Rg, DataNascimento, Sexo, 
			   Telefone, Celular, DDD, Matricula, Lotacao, LotacaoSigla,
			   LotacaoCentro, LotacaoCodigoFuncao, LotacaoFuncao,
			   LotacaoOrgao, LotacaoCodigoCargo, LotacaoCargo}|T], Count, CtrlInsert) ->
	User = #user{id = ems_db:sequence(user),
				 codigo = Codigo,
				 login = ?UTF8_STRING(Login),
				 name = ?UTF8_STRING(Name),
				 cpf = ?UTF8_STRING(Cpf),
				 email = ?UTF8_STRING(Email),
				 password = case PasswdCrypto of
								"SHA1" -> ?UTF8_STRING(Password);
								_ -> ems_util:criptografia_sha1(Password)
							end,
				 type = Type,
				 passwd_crypto = <<"SHA1">>,
				 type_email = TypeEmail,
				 endereco = ?UTF8_STRING(Endereco),
				 complemento_endereco = ?UTF8_STRING(ComplementoEndereco),
				 bairro = ?UTF8_STRING(Bairro),
				 cidade = ?UTF8_STRING(Cidade),
				 uf = ?UTF8_STRING(Uf),
				 cep = ?UTF8_STRING(Cep),
				 rg = ?UTF8_STRING(Rg),
				 data_nascimento = ems_util:date_to_binary(DataNascimento),
				 sexo = Sexo,
				 telefone = ?UTF8_STRING(Telefone),
				 celular = ?UTF8_STRING(Celular),
				 ddd = ?UTF8_STRING(DDD),
				 matricula = Matricula,
				 lotacao = ?UTF8_STRING(Lotacao),
				 lotacao_sigla = ?UTF8_STRING(LotacaoSigla),
				 lotacao_centro = ?UTF8_STRING(LotacaoCentro),
				 lotacao_codigo_funcao = LotacaoCodigoFuncao,
				 lotacao_funcao = ?UTF8_STRING(LotacaoFuncao),
				 lotacao_orgao = ?UTF8_STRING(LotacaoOrgao),
				 lotacao_codigo_cargo = LotacaoCodigoCargo,
				 lotacao_cargo = ?UTF8_STRING(LotacaoCargo),
				 active = Active == 1,
				 ctrl_insert = CtrlInsert},
	%?DEBUG("User  ~p\n", [User]),
	mnesia:write(User),
	insert_users(T, Count+1, CtrlInsert).


update_users([], Count, _CtrlUpdate) -> Count;
update_users([{Codigo, Login, Name, Cpf, Email, Password, Type, PasswdCrypto, 
			   TypeEmail, Active, Endereco, ComplementoEndereco, Bairro, 
			   Cidade, Uf, Cep, Rg, DataNascimento, Sexo, 
			   Telefone, Celular, DDD, Matricula, Lotacao, LotacaoSigla,
			   LotacaoCentro, LotacaoCodigoFuncao, LotacaoFuncao,
			   LotacaoOrgao, LotacaoCodigoCargo, LotacaoCargo}|T], Count, CtrlUpdate) ->
	case ems_user:find_by_codigo(Codigo) of
		{ok, User} ->
			User2 = User#user{codigo = Codigo,
							  login = ?UTF8_STRING(Login),
							  name = ?UTF8_STRING(Name),
							  cpf = ?UTF8_STRING(Cpf),
							  email = ?UTF8_STRING(Email),
							  password = case PasswdCrypto of
											"SHA1" -> ?UTF8_STRING(Password);
											_ -> ems_util:criptografia_sha1(Password)
										 end,
							  type = Type,
							  passwd_crypto = <<"SHA1">>,
							  type_email = TypeEmail,
							  endereco = ?UTF8_STRING(Endereco),
							  complemento_endereco = ?UTF8_STRING(ComplementoEndereco),
							  bairro = ?UTF8_STRING(Bairro),
							  cidade = ?UTF8_STRING(Cidade),
							  uf = ?UTF8_STRING(Uf),
							  cep = ?UTF8_STRING(Cep),
							  rg = ?UTF8_STRING(Rg),
							  data_nascimento = ems_util:date_to_binary(DataNascimento),
							  sexo = Sexo,
							  telefone = ?UTF8_STRING(Telefone),
							  celular = ?UTF8_STRING(Celular),
							  ddd = ?UTF8_STRING(DDD),
							  matricula = Matricula,
							  lotacao = ?UTF8_STRING(Lotacao),
							  lotacao_sigla = ?UTF8_STRING(LotacaoSigla),
							  lotacao_centro = ?UTF8_STRING(LotacaoCentro),
							  lotacao_codigo_funcao = LotacaoCodigoFuncao,
							  lotacao_funcao = ?UTF8_STRING(LotacaoFuncao),
							  lotacao_orgao = ?UTF8_STRING(LotacaoOrgao),
							  lotacao_codigo_cargo = LotacaoCodigoCargo,
							  lotacao_cargo = ?UTF8_STRING(LotacaoCargo),
							  active = Active == 1,
							  ctrl_update = CtrlUpdate};
		{error, enoent} -> 
			User2 = #user{id = ems_db:sequence(user),
						  codigo = Codigo,
						  login = ?UTF8_STRING(Login),
						  name = ?UTF8_STRING(Name),
						  cpf = ?UTF8_STRING(Cpf),
						  email = ?UTF8_STRING(Email),
						  password = case PasswdCrypto of
										"SHA1" -> ?UTF8_STRING(Password);
										_ -> ems_util:criptografia_sha1(Password)
									 end,
						  type = Type,
						  passwd_crypto = <<"SHA1">>,
						  type_email = TypeEmail,
						  endereco = ?UTF8_STRING(Endereco),
						  complemento_endereco = ?UTF8_STRING(ComplementoEndereco),
						  bairro = ?UTF8_STRING(Bairro),
						  cidade = ?UTF8_STRING(Cidade),
						  uf = ?UTF8_STRING(Uf),
						  cep = ?UTF8_STRING(Cep),
						  rg = ?UTF8_STRING(Rg),
						  data_nascimento = ems_util:date_to_binary(DataNascimento),
						  sexo = Sexo,
						  telefone = ?UTF8_STRING(Telefone),
						  celular = ?UTF8_STRING(Celular),
						  ddd = ?UTF8_STRING(DDD),
						  matricula = Matricula,
						  lotacao = ?UTF8_STRING(Lotacao),
						  lotacao_sigla = ?UTF8_STRING(LotacaoSigla),
						  lotacao_centro = ?UTF8_STRING(LotacaoCentro),
						  lotacao_codigo_funcao = LotacaoCodigoFuncao,
						  lotacao_funcao = ?UTF8_STRING(LotacaoFuncao),
						  lotacao_orgao = ?UTF8_STRING(LotacaoOrgao),
						  lotacao_codigo_cargo = LotacaoCodigoCargo,
						  lotacao_cargo = ?UTF8_STRING(LotacaoCargo),
						  active = Active == 1,
						  ctrl_insert = CtrlUpdate}
	end,
	mnesia:write(User2),
	?DEBUG("ems_user_loader update user: ~p.\n", [User2]),
	update_users(T, Count+1, CtrlUpdate).

sql_load_users_tipo_pessoa() ->	 
	"select  		CodigoPessoa, 
					lower(rtrim(LoginPessoa)) as LoginPessoa, 
					rtrim(NomePessoa) as NomePessoa, 
					rtrim(CpfCnpjPessoa) as CpfCnpjPessoa, 
					lower(rtrim(EmailPessoa)) as EmailPessoa, 
					rtrim(SenhaPessoa) as SenhaPessoa,
					TipoPessoa,
					PasswdCryptoPessoa,
					TipoEmailPessoa,
					1 as ActivePessoa,
					rtrim(Endereco) as Endereco,
					rtrim(ComplementoEndereco) as ComplementoEndereco,
					rtrim(Bairro) as Bairro,
					rtrim(Cidade) as Cidade,
					Uf,
					Cep,
					Rg,
					DataNascimento,
					Sexo,
					Telefone,
					Celular,
					DDD,
				    Matricula,
				    rtrim(Lotacao) as Lotacao,
				    LotacaoSigla,
				    rtrim(LotacaoCentro) as LotacaoCentro,
				    LotacaoCodigoFuncao,
				    rtrim(LotacaoFuncao) as LotacaoFuncao,
				    rtrim(LotacaoOrgao) as LotacaoOrgao,
				    LotacaoCodigoCargo,
				    rtrim(LotacaoCargo) as LotacaoCargo
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
				   em.EmaTipo as TipoEmailPessoa,
				   coalesce(p.PesEndereco, df.Endereco) as Endereco,
				   p.PesComplementoEndereco as ComplementoEndereco,
				   coalesce(p.PesBairro, df.Bairro) as Bairro,
				   coalesce(p.PesCidade, df.CidadeEndereco) as Cidade,
				   coalesce(p.PesUf, df.UFEndereco) as Uf,
				   coalesce(p.PesCep, df.CEP) as Cep,
				   coalesce(p.PesRg, df.rgnro) as Rg,
				   coalesce(p.PesDataNascimento, df.dtnascimento) as DataNascimento,
				   coalesce(p.PesSexo, df.sexo) as Sexo,
				   coalesce(p.PesTelefone, df.Telefone) as Telefone,
				   p.PesCelular as Celular,
				   coalesce(p.PesDDD, df.DDD) as DDD,
				   df.MatSipes as Matricula,
				   lf.CC as Lotacao,
				   lf.Sigla as LotacaoSigla,
				   lf.Centro as LotacaoCentro,
				   lf.Codigo as LotacaoCodigoFuncao,
				   lf.Funcao as LotacaoFuncao,
				   '' as LotacaoOrgao,
				   lf.Cod as LotacaoCodigoCargo,
				   lf.Cargo as LotacaoCargo
			from BDAcesso.dbo.TB_Usuario u join BDPessoa.dbo.TB_Pessoa p
						 on u.UsuPesIdPessoa = p.PesCodigoPessoa
				 left join BDPessoa.dbo.TB_PessoaFisicaEmail pfe
						 on p.PesCodigoPessoa = pfe.PFmPesCodigoPessoa             
				 join BDPessoa.dbo.TB_Email em
						 on pfe.PFmEmaCodigo = em.EmaCodigo
				 left join Sipes.dbo.DadosFuncionais df
						 on p.PesCodigoPessoa = df.PesCodigoPessoa
				 left join Sipes.dbo.vw_Genericos_LotacaoFuncao lf
						 on df.MatSipes = lf.Sipes
	) as t_users
	order by t_users.TipoEmailPessoa
	".


sql_load_users_tipo_aluno() ->	 
	"select  		CodigoPessoa, 
					lower(rtrim(LoginPessoa)) as LoginPessoa, 
					rtrim(NomePessoa) as NomePessoa, 
					rtrim(CpfCnpjPessoa) as CpfCnpjPessoa, 
					lower(rtrim(EmailPessoa)) as EmailPessoa, 
					rtrim(SenhaPessoa) as SenhaPessoa,
					TipoPessoa,
					PasswdCryptoPessoa,
					TipoEmailPessoa,
					1 as ActivePessoa,
					rtrim(Endereco) as Endereco,
					rtrim(ComplementoEndereco) as ComplementoEndereco,
					rtrim(Bairro) as Bairro,
					rtrim(Cidade) as Cidade,
					Uf,
					Cep,
					Rg,
					DataNascimento,
					Sexo,
					Telefone,
					Celular,
					DDD,
				    Matricula,
				    rtrim(Lotacao) as Lotacao,
				    LotacaoSigla,
				    rtrim(LotacaoCentro) as LotacaoCentro,
				    LotacaoCodigoFuncao,
				    rtrim(LotacaoFuncao) as LotacaoFuncao,
				    rtrim(LotacaoOrgao) as LotacaoOrgao,
				    LotacaoCodigoCargo,
				    rtrim(LotacaoCargo) as LotacaoCargo
	from (
			-- Busca dados de alunos em BDSiac com AluRA
			select p.PesCodigoPessoa as CodigoPessoa, 
				   cast(al.AluRA as varchar(100)) as LoginPessoa,
				   p.PesNome as NomePessoa, 
				   cast(coalesce(p.PesCpf, cast(al.AluCPF as varchar(11))) as varchar(14)) as CpfCnpjPessoa, 
				   cast(coalesce(em.EmaEmail, al.AluEmail) as varchar(60)) as EmailPessoa, 
				   cast(al.AluSenha as varchar(60)) as SenhaPessoa,
				   2 as TipoPessoa,  -- Aluno
				   null as PasswdCryptoPessoa,
				   em.EmaTipo as TipoEmailPessoa,
				   coalesce(p.PesEndereco, al.AluEndereco) as Endereco,
				   p.PesComplementoEndereco as ComplementoEndereco,
				   p.PesBairro as Bairro,
				   coalesce(p.PesCidade, al.AluEndCidade) as Cidade,
				   coalesce(p.PesUf, al.AluEndUf) as Uf,
				   p.PesCep as Cep,
				   p.PesRg as Rg,
				   coalesce(p.PesDataNascimento, al.AluDtNasc) as DataNascimento,
				   p.PesSexo as Sexo,
				   coalesce(p.PesTelefone, al.AluTelefone) as Telefone,
				   coalesce(p.PesCelular, al.AluCelular) as Celular,
				   p.PesDDD as DDD,
				   al.AluMatricula as Matricula,
				   null as Lotacao,
				   null as LotacaoSigla,
				   null as LotacaoCentro,
				   null as LotacaoCodigoFuncao,
				   null as LotacaoFuncao,
				   null as LotacaoOrgao,
				   null as LotacaoCodigoCargo,
				   null as LotacaoCargo
			from BDSiac.dbo.TB_Aluno al join BDPessoa.dbo.TB_Pessoa p
						 on al.AluPesCodigoPessoa = p.PesCodigoPessoa
				 left join BDPessoa.dbo.TB_PessoaFisicaEmail pfe
						 on p.PesCodigoPessoa = pfe.PFmPesCodigoPessoa             
				 join BDPessoa.dbo.TB_Email em
						 on pfe.PFmEmaCodigo = em.EmaCodigo
			where al.AluPerSaiUnB = 99999
	) as t_users
	order by t_users.TipoEmailPessoa
	".


sql_update_users_tipo_pessoa() ->	 
	"select 		CodigoPessoa, 
					lower(rtrim(LoginPessoa)) as LoginPessoa, 
					rtrim(NomePessoa) as NomePessoa, 
					rtrim(CpfCnpjPessoa) as CpfCnpjPessoa, 
					lower(rtrim(EmailPessoa)) as EmailPessoa, 
					rtrim(SenhaPessoa) as SenhaPessoa,
					TipoPessoa,
					PasswdCryptoPessoa,
					TipoEmailPessoa,
					1 as ActivePessoa,
					rtrim(Endereco) as Endereco,
					rtrim(ComplementoEndereco) as ComplementoEndereco,
					rtrim(Bairro) as Bairro,
					rtrim(Cidade) as Cidade,
					Uf,
					Cep,
					Rg,
					DataNascimento,
					Sexo,
					Telefone,
					Celular,
					DDD,
				    Matricula,
				    rtrim(Lotacao) as Lotacao,
				    LotacaoSigla,
				    rtrim(LotacaoCentro) as LotacaoCentro,
				    LotacaoCodigoFuncao,
				    rtrim(LotacaoFuncao) as LotacaoFuncao,
				    rtrim(LotacaoOrgao) as LotacaoOrgao,
				    LotacaoCodigoCargo,
				    rtrim(LotacaoCargo) as LotacaoCargo
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
				   em.EmaTipo as TipoEmailPessoa,
				   coalesce(p.PesEndereco, df.Endereco) as Endereco,
				   p.PesComplementoEndereco as ComplementoEndereco,
				   coalesce(p.PesBairro, df.Bairro) as Bairro,
				   coalesce(p.PesCidade, df.CidadeEndereco) as Cidade,
				   coalesce(p.PesUf, df.UFEndereco) as Uf,
				   coalesce(p.PesCep, df.CEP) as Cep,
				   coalesce(p.PesRg, df.rgnro) as Rg,
				   coalesce(p.PesDataNascimento, df.dtnascimento) as DataNascimento,
				   coalesce(p.PesSexo, df.sexo) as Sexo,
				   coalesce(p.PesTelefone, df.Telefone) as Telefone,
				   p.PesCelular as Celular,
				   coalesce(p.PesDDD, df.DDD) as DDD,
				   df.MatSipes as Matricula,
				   lf.CC as Lotacao,
				   lf.Sigla as LotacaoSigla,
				   lf.Centro as LotacaoCentro,
				   lf.Codigo as LotacaoCodigoFuncao,
				   lf.Funcao as LotacaoFuncao,
				   '' as LotacaoOrgao,
				   lf.Cod as LotacaoCodigoCargo,
				   lf.Cargo as LotacaoCargo
			from BDAcesso.dbo.TB_Usuario u join BDPessoa.dbo.TB_Pessoa p
						 on u.UsuPesIdPessoa = p.PesCodigoPessoa
				 left join BDPessoa.dbo.TB_PessoaFisicaEmail pfe
						 on p.PesCodigoPessoa = pfe.PFmPesCodigoPessoa             
				 join BDPessoa.dbo.TB_Email em
						 on pfe.PFmEmaCodigo = em.EmaCodigo
				 left join Sipes.dbo.DadosFuncionais df
						 on p.PesCodigoPessoa = df.PesCodigoPessoa
				 left join Sipes.dbo.vw_Genericos_LotacaoFuncao lf
						 on df.MatSipes = lf.Sipes
			where u.UsuDataAlteracao >= ? or p.PesDataAlteracao >= ? or em.EmaDataAlteracao >= ?
	) as t_users
	order by t_users.TipoPessoa, t_users.TipoEmailPessoa
	".
	
sql_update_users_tipo_aluno() ->	 
	"select 		CodigoPessoa, 
					lower(rtrim(LoginPessoa)) as LoginPessoa, 
					rtrim(NomePessoa) as NomePessoa, 
					rtrim(CpfCnpjPessoa) as CpfCnpjPessoa, 
					lower(rtrim(EmailPessoa)) as EmailPessoa, 
					rtrim(SenhaPessoa) as SenhaPessoa,
					TipoPessoa,
					PasswdCryptoPessoa,
					TipoEmailPessoa,
					1 as ActivePessoa,
					rtrim(Endereco) as Endereco,
					rtrim(ComplementoEndereco) as ComplementoEndereco,
					rtrim(Bairro) as Bairro,
					rtrim(Cidade) as Cidade,
					Uf,
					Cep,
					Rg,
					DataNascimento,
					Sexo,
					Telefone,
					Celular,
					DDD,
				    Matricula,
				    rtrim(Lotacao) as Lotacao,
				    LotacaoSigla,
				    rtrim(LotacaoCentro) as LotacaoCentro,
				    LotacaoCodigoFuncao,
				    rtrim(LotacaoFuncao) as LotacaoFuncao,
				    rtrim(LotacaoOrgao) as LotacaoOrgao,
				    LotacaoCodigoCargo,
				    rtrim(LotacaoCargo) as LotacaoCargo
	from (
			-- Busca dados de alunos em BDSiac com AluRA
			select p.PesCodigoPessoa as CodigoPessoa, 
				   cast(al.AluRA as varchar(100)) as LoginPessoa,
				   p.PesNome as NomePessoa, 
				   cast(coalesce(p.PesCpf, cast(al.AluCPF as varchar(11))) as varchar(14)) as CpfCnpjPessoa, 
				   cast(coalesce(em.EmaEmail, al.AluEmail) as varchar(60)) as EmailPessoa, 
				   cast(al.AluSenha as varchar(60)) as SenhaPessoa,
				   2 as TipoPessoa,  -- Aluno
				   null as PasswdCryptoPessoa,
				   em.EmaTipo as TipoEmailPessoa,
				   coalesce(p.PesEndereco, al.AluEndereco) as Endereco,
				   p.PesComplementoEndereco as ComplementoEndereco,
				   p.PesBairro as Bairro,
				   coalesce(p.PesCidade, al.AluEndCidade) as Cidade,
				   coalesce(p.PesUf, al.AluEndUf) as Uf,
				   p.PesCep as Cep,
				   p.PesRg as Rg,
				   coalesce(p.PesDataNascimento, al.AluDtNasc) as DataNascimento,
				   p.PesSexo as Sexo,
				   coalesce(p.PesTelefone, al.AluTelefone) as Telefone,
				   coalesce(p.PesCelular, al.AluCelular) as Celular,
				   p.PesDDD as DDD,
				   al.AluMatricula as Matricula,
				   null as Lotacao,
				   null as LotacaoSigla,
				   null as LotacaoCentro,
				   null as LotacaoCodigoFuncao,
				   null as LotacaoFuncao,
				   null as LotacaoOrgao,
				   null as LotacaoCodigoCargo,
				   null as LotacaoCargo
			from BDSiac.dbo.TB_Aluno al join BDPessoa.dbo.TB_Pessoa p
						 on al.AluPesCodigoPessoa = p.PesCodigoPessoa
				 left join BDPessoa.dbo.TB_PessoaFisicaEmail pfe
						 on p.PesCodigoPessoa = pfe.PFmPesCodigoPessoa             
				 join BDPessoa.dbo.TB_Email em
						 on pfe.PFmEmaCodigo = em.EmaCodigo 
			where al.AluPerSaiUnB = 99999 and (al.AluDataAlteracao >= ? or p.PesDataAlteracao >= ? or em.EmaDataAlteracao >= ?)
	) as t_users
	order by t_users.TipoPessoa, t_users.TipoEmailPessoa
	".
