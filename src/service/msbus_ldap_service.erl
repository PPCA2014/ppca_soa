%%********************************************************************
%% @title Module msbus_ldap_service
%% @version 1.0.0
%% @doc It provides ldap service.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_ldap_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).


-include("../../include/msbus_config.hrl").
-include("../../include/msbus_schema.hrl").
-include("../../include/LDAP.hrl").


%% Server API
-export([start/0, start_link/1, stop/0]).

%% Client API
-export([execute/2, find_user_by_login/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Stores the state of the service.
-record(state, {connection,			 	%% connection to ldap database
				data_source,		 	%% odbc datasource
				sql_find_user,		 	%% sql to find user
				dn_admin_ldap,		 	%% DN of admin ldap
				password_admin_ldap}).  %% Password of admin ldap


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Client API
%%====================================================================
 
execute(Request, From) ->
	msbus_pool:cast(msbus_ldap_service_pool, {search, Request, From}).

find_user_by_login(UsuLogin) ->
	msbus_pool:call(msbus_ldap_service_pool, {find_user_by_login, UsuLogin}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    State = #state{connection = close_connection,
				   data_source = "DSN=pessoa;UID=usupessoa;PWD=usupessoa",
				   sql_find_user = "select  top 1 p.PesCodigoPessoa, p.PesNome, p.PesCpf, p.PesEmail, u.UsuSenha from BDPessoa.dbo.TB_Pessoa p left join BDAcesso.dbo.TB_Usuario u on p.PesCodigoPessoa = u.UsuPesIdPessoa where u.UsuLogin = ?",
				   dn_admin_ldap = <<"admin">>,
				   password_admin_ldap = <<"123456">>
			   },
    {ok, State}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({search, Request, _From}, State) ->
	{Result, NewState} = handle_request(Request, State),
	msbus_eventmgr:notifica_evento(ok_request, {servico, Request, Result}),
	{noreply, NewState}.

handle_call({search, Request}, _From, State) ->
	{Result, NewState} = handle_request(Request, State),
	{reply, Result, NewState};

handle_call({find_user_by_login, UsuLogin}, _From, State) ->
	Result = find_user_by_login(UsuLogin, State),
	{reply, Result, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Internal functions
%%====================================================================
    
autentica("cn", DnAdmin, Password, State = #state{dn_admin_ldap = DnAdminLdap, 
												  password_admin_ldap = PasswordAdminLdap}) -> 
	autentica(list_to_binary(DnAdmin), Password, DnAdminLdap, PasswordAdminLdap, State);

autentica("uid", DnUser, _Password, State) -> 
	case find_user_by_login(DnUser, State) of
		{error, unavailable} -> 
			{unavailable, State};
		{{}, State2} -> 
			{invalidCredentials, State2};
		{{_, _, _, _, _PasswordLdap}, State2} -> 
			%autentica(DnUser, Password, DnUser, PasswordLdap)
			{success, State2}
	end.

autentica(Dn, Password, DnLdap, PasswordLdap, State) when Dn =:= DnLdap, Password =:= PasswordLdap -> {success, State};

autentica(_, _, _, _, State) -> {invalidCredentials, State}.

	
    
handle_request({bindRequest, #'BindRequest'{version = _Version, 
											name = Name, 
											authentication = {_, Password}}}, State) ->
	Name2 = parse_base_object(Name),
	case lists:keytake("cn", 1, Name2) of  
		{_, {"cn", CnValue}, _} -> 
			{ResultCode, State2} = autentica("cn", CnValue, Password, State);
		false -> 
			case lists:keytake("uid", 1, Name2) of  
				{_, {"uid", UIdValue}, _} -> 
					{ResultCode, State2} = autentica("uid", UIdValue, Password, State);
				false -> 
					{ResultCode, State2} = {operationsError, State}
			end
	end,
	BindResponse = make_bind_response(ResultCode, Name),
	{{ok, [BindResponse]}, State2};
    

handle_request({searchRequest, #'SearchRequest'{baseObject = _BaseObject, 
												scope = _Scope, 
												derefAliases = _DerefAliases, 
												sizeLimit = _SizeLimit, 
												timeLimit = _TimeLimit, 
												typesOnly = _TypesOnly, 
												filter =  {equalityMatch, {'AttributeValueAssertion', <<"uid">>, UsuLoginBin}},
												attributes = _Attributes}}, State) ->
	UsuLogin = binary_to_list(UsuLoginBin),
	%BaseObject2 = parse_base_object(BaseObject),
	case find_user_by_login(UsuLogin, State) of
		{error, unavailable} ->
			ResultDone = make_result_done(unavailable),
			{{ok, [ResultDone]}, State};
		{{}, State2} -> 
			ResultDone = make_result_done(invalidCredentials),
			{{ok, [ResultDone]}, State2};
		{DadosUser, State2} ->
			ResultEntry = make_result_entry(UsuLoginBin, DadosUser, State2),
			ResultDone = make_result_done(success),
			{{ok, [ResultEntry, ResultDone]}, State2}
	end;


handle_request({unbindRequest, _}, State) ->
	{{ok, unbindRequest}, State};


handle_request(#request{payload = LdapMsg}, State) ->
	handle_request(LdapMsg#'LDAPMessage'.protocolOp, State).


parse_base_object(BaseObject) ->
	lists:map(fun(P) -> 
						list_to_tuple(string:tokens(P, "=")) 
			  end, string:tokens(binary_to_list(BaseObject), ",")).


make_object_name(UsuId) ->
	R1 = [<<"uid="/utf8>>, UsuId, <<",ou=funcdis,ou=Classes,dc=unb,dc=br"/utf8>>],
	R2 = iolist_to_binary(R1),
	R2.

make_bind_response(unavailable, _) ->
	make_bind_response(unavailable, <<"">>, <<"LDAP database or server machine unavailable!!!">>);

make_bind_response(ResultCode, MatchedDN) ->
	make_bind_response(ResultCode, MatchedDN, <<"">>).

make_bind_response(ResultCode, MatchedDN, DiagnosticMessage) ->
	{bindResponse, #'BindResponse'{resultCode = ResultCode,
												  matchedDN = MatchedDN,
												  diagnosticMessage = DiagnosticMessage,
												  referral = asn1_NOVALUE,
												  serverSaslCreds = asn1_NOVALUE}
	}.

make_result_entry(UsuLogin, {UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}, #state{dn_admin_ldap = DnAdminLdap}) ->
	ObjectName = make_object_name(UsuLogin),
	{searchResEntry, #'SearchResultEntry'{objectName = ObjectName,
										  attributes = [#'PartialAttribute'{type = <<"uid">>, vals = [UsuLogin]},
														#'PartialAttribute'{type = <<"cn">>, vals = [DnAdminLdap]},
														#'PartialAttribute'{type = <<"mail">>, vals = [UsuEmail]},
														#'PartialAttribute'{type = <<"cpf">>, vals = [UsuCpf]},
														#'PartialAttribute'{type = <<"passwd">>, vals = [UsuSenha]},
														#'PartialAttribute'{type = <<"givenName">>, vals = [UsuNome]},
														#'PartialAttribute'{type = <<"employeeNumber">>, vals = [UsuId]}
														]
										}
	}.


make_result_done(ResultCode) ->
	{searchResDone, #'LDAPResult'{resultCode = ResultCode, 
								  matchedDN = <<"">>, 
								  diagnosticMessage = <<"">>,
								  referral = asn1_NOVALUE}
	
	}.
	

find_user_by_login(UserLogin, State = #state{sql_find_user = Sql}) ->
	case get_database_connection(State) of
		close_connection -> 
			{error, unavailable};
		Conn -> 
			State2 = State#state{connection = Conn},
			Params = [{{sql_varchar, 100}, [UserLogin]}],
			 {_, _, DadosUser} = odbc:param_query(Conn, Sql, Params),
			case DadosUser of
				[{UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}] ->
					DadosUser2 = {format_user_field(UsuId),
								  format_user_field(UsuNome),
								  format_user_field(UsuCpf),
								  format_user_field(UsuEmail),
								  format_user_field(UsuSenha)}, 
					{DadosUser2, State2};
				_->
					{{}, State2}
			end
	end.


format_user_field(null) -> <<"">>;
format_user_field([]) -> <<"">>;
format_user_field(Number) when is_integer(Number) -> list_to_binary(integer_to_list(Number));
format_user_field(Text) -> list_to_binary(string:strip(Text)).
	

get_database_connection(#state{connection = close_connection, 
							   data_source = DataSource}) ->
	try
		case odbc:connect(DataSource, [{scrollable_cursors, off},
									   {timeout, 3500},
									   {trace_driver, off}]) of
			{ok, Conn}	->																	  
				msbus_logger:info("Create connection to ldap datasource ~p.", [DataSource]),
				Conn;
			{error, Reason} -> 
				msbus_logger:error("Connection ldap database error. Reason: ~p", [Reason]),
				close_connection
		end
	catch
		_Exception:Reason2 -> 
			msbus_logger:error("Connection ldap database error. Reason: ~p", [Reason2]),
			close_connection
	end;

	
get_database_connection(#state{connection = Conn}) -> Conn.
	
			




