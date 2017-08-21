%%********************************************************************
%% @title Module ems_ldap_handler
%% @version 1.0.0
%% @doc Process ldap messages
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_ldap_handler).

-behaviour(ranch_protocol).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/LDAP.hrl").

%  Stores the state of the service.
-record(state, {datasource,		 	%% datasource
				middleware,		 	%% middleware to service logic
				admin,		 		%% admin ldap
				password_admin,     %% Password of admin ldap
				tcp_allowed_address_t}).   

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Service) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Service]),
	{ok, Pid}.

init(Ref, Socket, Transport, [#service{datasource = Datasource,
										middleware = Middleware,
										tcp_allowed_address_t = AllowedAddress,
										properties = Props}]) ->
	ranch:accept_ack(Ref),
	LdapAdmin = maps:get(<<"ldap_admin">>, Props),
	LdapPasswdAdmin = maps:get(<<"ldap_password_admin">>, Props),
    State = #state{datasource = Datasource,
				   middleware = Middleware,
				   admin = LdapAdmin,
				   password_admin = LdapPasswdAdmin,
				   tcp_allowed_address_t = AllowedAddress
			   },
	loop(Socket, Transport, State).

loop(Socket, Transport, State = #state{tcp_allowed_address_t = AllowedAddress}) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			case inet:peername(Socket) of
				{ok, {Ip,_Port}} ->
					case ems_tcp_util:allow_ip_address(Ip, AllowedAddress) of				
						true ->
							case decode_ldap_message(Data) of
								{ok, LdapMessage} ->
									ems_logger:debug2("ems_ldap_handler request: ~p\n.", [LdapMessage]),
									MessageID = LdapMessage#'LDAPMessage'.messageID,
									Result = handle_request(LdapMessage, State),
									case Result of
										{ok, unbindRequest} ->
											?DEBUG("ems_ldap_handler unbindRequest and close socket."),
											Transport:close(Socket);
										{ok, Msg} -> 
											?DEBUG("ems_ldap_handler response: ~p.", [Msg]),
											Response = [ encode_response(MessageID, M) || M <- Msg ],
											Transport:send(Socket, Response)
									end;
								{error, Reason} ->
									ems_logger:error("ems_ldap_handler decode invalid message. Reason: ~p.", [Reason]),
									ResultDone = make_result_done(inappropriateMatching),
									Response = [ encode_response(1, ResultDone) ],
									Transport:send(Socket, Response),
									Transport:close(Socket)
							end;
						false ->
							ems_logger:warn("ems_ldap_handler does not grant access to IP ~p. Reason: IP denied.", [Ip]),
							ResultDone = make_result_done(insufficientAccessRights),
							Response = [ encode_response(1, ResultDone) ],
							Transport:send(Socket, Response),
							Transport:close(Socket)
					end;
				Error -> 
					ems_logger:error("ems_ldap_handler peername error. Reason: ~p.", [Error]),
					Transport:close(Socket)
			end,
			loop(Socket, Transport, State);		
		_ ->
			Transport:close(Socket)
	end.



encode_response(MessageID, Msg) ->
	Response = #'LDAPMessage'{messageID = MessageID,
							  protocolOp = Msg,
							  controls = asn1_NOVALUE},
    case 'LDAP':encode('LDAPMessage', Response) of
        {ok, Result} -> Result;
        {error, Reason} -> {error, Reason}
    end.


decode_ldap_message(RequestBin) ->
	case 'LDAP':decode('LDAPMessage', RequestBin) of
        {ok, {'LDAPMessage', _MessageID, _ProtocolOp, _} = LdapMsg} -> {ok, LdapMsg};
		Error -> Error
    end.

  
handle_request({'LDAPMessage', _,
					{bindRequest, #'BindRequest'{version = _Version, 
												 name = Name, 
												 authentication = {_, Password}}},
				 _}, #state{admin = AdminLdap, 
							password_admin = PasswordAdminLdap}) ->
	case Name of
		<<Cn:3/binary, _/binary>> ->
			case Cn of
				<<"cn=">> ->
					BindResponse = case Name =:= AdminLdap andalso Password =:= PasswordAdminLdap of
						true -> 
							ems_logger:info("ems_ldap_handler bind_cn ~p success.", [Name]),
							make_bind_response(success, Name);
						_-> 
							ems_logger:error("ems_ldap_handler bind_cn ~p invalid credential.", [Name]),
							make_bind_response(invalidCredentials, Name)
					end;
				<<"uid">> -> 
					<<_:4/binary, UserLogin/binary>> = hd(binary:split(Name, <<",">>)),
					BindResponse = case do_authenticate(UserLogin, Password) of
						ok -> 
							ems_logger:info("ems_ldap_handler bind_uid ~p success.", [Name]),
							make_bind_response(success, Name);
						{error, _Reason} ->	
							ems_logger:error("ems_ldap_handler bind_uid ~p invalid credential.", [Name]),
							make_bind_response(invalidCredentials, Name)
					end;
				_ -> 
					BindResponse = case do_authenticate(Name, Password) of
						ok -> 
							ems_logger:info("ems_ldap_handler bind ~p success.", [Name]),
							make_bind_response(success, Name);
						{error, _Reason} ->	
							ems_logger:error("ems_ldap_handler bind ~p invalid credential.", [Name]),
							make_bind_response(invalidCredentials, Name)
					end
			end;
		_ -> BindResponse = make_bind_response(invalidCredentials, Name)
	end,
	{ok, [BindResponse]};
handle_request({'LDAPMessage', _,
					{searchRequest, #'SearchRequest'{baseObject = _BaseObject, 
													scope = _Scope, 
													derefAliases = _DerefAliases, 
													sizeLimit = _SizeLimit, 
													timeLimit = _TimeLimit, 
													typesOnly = _TypesOnly, 
													filter =  {equalityMatch, {'AttributeValueAssertion', <<"uid">>, UsuLoginBin}},
													attributes = _Attributes}},
				 _}, State) ->
	handle_request_search_login(UsuLoginBin, State);
handle_request({'LDAPMessage', _,
					{searchRequest, #'SearchRequest'{baseObject = _BaseObject, 
													scope = _Scope, 
													derefAliases = _DerefAliases, 
													sizeLimit = _SizeLimit, 
													timeLimit = _TimeLimit, 
													typesOnly = _TypesOnly, 
													filter =  {present, ObjectClass},
													attributes = _Attributes}},
				 _}, _State) ->
	ObjectName = make_object_name(ObjectClass),
	ResultEntry = {searchResEntry, #'SearchResultEntry'{objectName = ObjectName,
										  attributes = [#'PartialAttribute'{type = <<"supportedCapabilities">>, vals = [<<"yes">>]},
														#'PartialAttribute'{type = <<"supportedControl">>, vals = [<<"no">>]},
														#'PartialAttribute'{type = <<"supportedExtension">>, vals = [<<"no">>]},
														#'PartialAttribute'{type = <<"supportedFeatures">>, vals = [<<"no">>]},
														#'PartialAttribute'{type = <<"supportedLdapVersion">>, vals = [<<"3">>]},
														#'PartialAttribute'{type = <<"supportedSASLMechanisms">>, vals = [<<"no">>]}
														]
										}
	},
	ems_logger:info("ems_ldap_handler request supported capabilities."),
	ResultDone = make_result_done(success),
	{ok, [ResultEntry, ResultDone]};
handle_request({'LDAPMessage', _,
					{searchRequest, #'SearchRequest'{baseObject = _BaseObject, 
													scope = _Scope, 
													derefAliases = _DerefAliases, 
													sizeLimit = _SizeLimit, 
													timeLimit = _TimeLimit, 
													typesOnly = _TypesOnly, 
													filter =  {'and',
																[{present,<<"objectClass">>},
																	{equalityMatch, {'AttributeValueAssertion', <<"uid">>, UsuLoginBin}}
																]},
													attributes = _Attributes}},
				_}, State) ->
	handle_request_search_login(UsuLoginBin, State);
handle_request({'LDAPMessage', _, 
					{unbindRequest, _},
				 _}, _State) ->
	{ok, unbindRequest};
handle_request({'LDAPMessage', _, 
					_UnknowMsg,
				 _} = LdapMsg, _State) ->
	ems_logger:warn("ems_ldap_handler received unknow msg ~p\n", [LdapMsg]),
	{ok, unbindRequest}.
	

make_object_name(UsuId) ->
	R1 = [<<"uid="/utf8>>, UsuId, <<",ou=funcdis,ou=Classes,dc=unb,dc=br"/utf8>>],
	R2 = iolist_to_binary(R1),
	R2.

make_bind_response(unavailable, _) ->
	make_bind_response(unavailable, <<"">>, <<"LDAP unavailable!!!">>);

make_bind_response(ResultCode, MatchedDN) ->
	make_bind_response(ResultCode, MatchedDN, <<"">>).

make_bind_response(ResultCode, MatchedDN, DiagnosticMessage) ->
	{bindResponse, #'BindResponse'{resultCode = ResultCode,
												  matchedDN = MatchedDN,
												  diagnosticMessage = DiagnosticMessage,
												  referral = asn1_NOVALUE,
												  serverSaslCreds = asn1_NOVALUE}
	}.

make_result_entry(#user{user_id = UsuId, 
					    login = UsuLogin,	
					    name = UsuName, 
					    cpf = UsuCpf, 
					    email = UsuEmail, 
					    password = UsuPasswd, 
					    type = UsuType, 
					    type_email = UsuTypeEmail, 
					    ctrl_insert = UsuCtrlInsert, 
						ctrl_update = UsuCtrlUpdate,
					    matricula = Matricula,
	
						active = Active,
						endereco = Endereco,
						complemento_endereco = ComplementoEndereco,
						bairro = Bairro,
						cidade = Cidade,
						uf = UF,
						rg = RG,
						data_nascimento = DataNascimento,
						sexo = Sexo,
						telefone = Telefone,
						celular = Celular,
						ddd = DDD,
		
					    lotacao = Lotacao,
					    lotacao_sigla = LotacaoSigla,
					    lotacao_centro = LotacaoCentro,
					    lotacao_codigo_funcao = LotacaoCodigoFuncao,
					    lotacao_funcao = LotacaoFuncao,
					    lotacao_orgao = LotacaoOrgao,
					    lotacao_codigo_cargo = LotacaoCodigoCargo,
					    lotacao_cargo = LotacaoCargo
}, 
				  AdminLdap) ->
	ObjectName = make_object_name(UsuLogin),
	UsuId2 = format_user_field(UsuId),
	UsuLogin2 = format_user_field(UsuLogin),
	UsuNome2 = format_user_field(UsuName),
	UsuCpf2 = format_user_field(UsuCpf),
	UsuEmail2 = format_user_field(UsuEmail),
	UsuSenha2 = format_user_field(UsuPasswd),
	UsuType2 = format_user_field(UsuType),
	UsuTypeEmail2 = format_user_field(UsuTypeEmail),
	UsuCtrlInsert2 = format_user_field(UsuCtrlInsert),
	UsuCtrlUpdate2 = format_user_field(UsuCtrlUpdate),
	
	Active2 = format_user_field(Active),
	Endereco2 = format_user_field(Endereco),
	ComplementoEndereco2 = format_user_field(ComplementoEndereco),
	Bairro2 = format_user_field(Bairro),
	Cidade2 = format_user_field(Cidade),
	UF2 = format_user_field(UF),
	RG2 = format_user_field(RG),
	DataNascimento2 = format_user_field(DataNascimento),
	Sexo2 = format_user_field(Sexo),
	Telefone2 = format_user_field(Telefone),
	Celular2 = format_user_field(Celular),
	DDD2 = format_user_field(DDD),

	Matricula2 = format_user_field(Matricula),
	Lotacao2 = format_user_field(Lotacao),
	LotacaoSigla2 = format_user_field(LotacaoSigla),
	LotacaoCentro2 = format_user_field(LotacaoCentro),
	LotacaoCodigoFuncao2 = format_user_field(LotacaoCodigoFuncao),
	LotacaoFuncao2 = format_user_field(LotacaoFuncao),
	LotacaoOrgao2 = format_user_field(LotacaoOrgao),
	LotacaoCodigoCargo2 = format_user_field(LotacaoCodigoCargo),
	LotacaoCargo2 = format_user_field(LotacaoCargo),

	Names = binary:split(UsuName, <<" ">>),
	SN = format_user_field(lists:last(Names)),
	GivenName = format_user_field(hd(Names)),


	{searchResEntry, #'SearchResultEntry'{objectName = ObjectName,
										  attributes = [#'PartialAttribute'{type = <<"uid">>, vals = [UsuId2]},
 														#'PartialAttribute'{type = <<"employeeNumber">>, vals = [UsuId2]},
														#'PartialAttribute'{type = <<"uidNumber">>, vals = [UsuId2]},
														
														#'PartialAttribute'{type = <<"objectClass">>, vals = [<<"top">>]},
														#'PartialAttribute'{type = <<"objectClass">>, vals = [<<"person">>]},
														#'PartialAttribute'{type = <<"objectClass">>, vals = [<<"organizationalPerson">>]},
														#'PartialAttribute'{type = <<"objectClass">>, vals = [<<"inetOrgPerson">>]},
														#'PartialAttribute'{type = <<"objectClass">>, vals = [<<"posixAccount">>]},
														
														#'PartialAttribute'{type = <<"gecos">>, vals = [UsuNome2]},
														#'PartialAttribute'{type = <<"cn">>, vals = [UsuNome2]},
														#'PartialAttribute'{type = <<"givenName">>, vals = [GivenName]},
														#'PartialAttribute'{type = <<"sn">>, vals = [SN]},

														#'PartialAttribute'{type = <<"creatorsName">>, vals = [AdminLdap]},
														#'PartialAttribute'{type = <<"o">>, vals = [<<"UnB">>]},
														
														
														#'PartialAttribute'{type = <<"mail">>, vals = [UsuEmail2]},
														#'PartialAttribute'{type = <<"email">>, vals = [UsuEmail2]},
														#'PartialAttribute'{type = <<"login">>, vals = [UsuLogin2]},
														#'PartialAttribute'{type = <<"cpf">>, vals = [UsuCpf2]},
														#'PartialAttribute'{type = <<"passwd">>, vals = [UsuSenha2]},
														
														#'PartialAttribute'{type = <<"distinguishedName">>, vals = [UsuLogin2]},
														
														#'PartialAttribute'{type = <<"active">>, vals = [Active2]},
														#'PartialAttribute'{type = <<"endereco">>, vals = [Endereco2]},
														#'PartialAttribute'{type = <<"complemento_endereco">>, vals = [ComplementoEndereco2]},
														#'PartialAttribute'{type = <<"bairro">>, vals = [Bairro2]},
														#'PartialAttribute'{type = <<"cidade">>, vals = [Cidade2]},
														#'PartialAttribute'{type = <<"uf">>, vals = [UF2]},
														#'PartialAttribute'{type = <<"rg">>, vals = [RG2]},
														#'PartialAttribute'{type = <<"dataNascimento">>, vals = [DataNascimento2]},
														#'PartialAttribute'{type = <<"sexo">>, vals = [Sexo2]},
														#'PartialAttribute'{type = <<"telefone">>, vals = [Telefone2]},

														#'PartialAttribute'{type = <<"celular">>, vals = [Celular2]},

														#'PartialAttribute'{type = <<"ddd">>, vals = [DDD2]},
														
														#'PartialAttribute'{type = <<"matsipes">>, vals = [Matricula2]},

														#'PartialAttribute'{type = <<"lotacao">>, vals = [Lotacao2]},
														#'PartialAttribute'{type = <<"lotacaoSigla">>, vals = [LotacaoSigla2]},
														#'PartialAttribute'{type = <<"lotacaoCentro">>, vals = [LotacaoCentro2]},
														#'PartialAttribute'{type = <<"lotacaoCodigoFuncao">>, vals = [LotacaoCodigoFuncao2]},
														#'PartialAttribute'{type = <<"lotacaoFuncao">>, vals = [LotacaoFuncao2]},
														#'PartialAttribute'{type = <<"lotacaoOrgao">>, vals = [LotacaoOrgao2]},
														#'PartialAttribute'{type = <<"lotacaoCodigoCargo">>, vals = [LotacaoCodigoCargo2]},
														#'PartialAttribute'{type = <<"lotacaoCargo">>, vals = [LotacaoCargo2]}

														#'PartialAttribute'{type = <<"ems_type_user">>, vals = [UsuType2]},
														#'PartialAttribute'{type = <<"ems_type_email">>, vals = [UsuTypeEmail2]},
														#'PartialAttribute'{type = <<"ems_ctrl_insert">>, vals = [UsuCtrlInsert2]},
														#'PartialAttribute'{type = <<"ems_ctrl_update">>, vals = [UsuCtrlUpdate2]}

														]
										}
	}.


make_result_done(ResultCode) ->
	{searchResDone, #'LDAPResult'{resultCode = ResultCode, 
								  matchedDN = <<"">>, 
								  diagnosticMessage = <<"">>,
								  referral = asn1_NOVALUE}
	
	}.
	

-spec handle_request_search_login(binary(), #state{}) -> {ok, tuple()}.
handle_request_search_login(UserLogin, #state{admin = AdminLdap}) ->	
	case do_find_user_by_login(UserLogin) of
		{error, enoent} ->
			ems_logger:error("ems_ldap_handler search ~p does not exist.", [UserLogin]),
			ResultDone = make_result_done(invalidCredentials),
			{ok, [ResultDone]};
		{error, Reason} ->
			ems_logger:error("ems_ldap_handler search ~p fail. Reason: ~p.", [UserLogin, Reason]),
			ResultDone = make_result_done(unavailable),
			{ok, [ResultDone]};
		{ok, User} ->
			ems_logger:info("ems_ldap_handler search ~p ~p success.", [UserLogin, User#user.name]),
			ResultEntry = make_result_entry(User, AdminLdap),
			ResultDone = make_result_done(success),
			{ok, [ResultEntry, ResultDone]}
	end.
	

do_authenticate(UserLogin, UserPassword) ->
	ems_user:authenticate_login_password(UserLogin, UserPassword).

do_find_user_by_login(UserLogin) ->
	case ems_user:find_by_login(UserLogin) of
		{ok, User} ->
			?DEBUG("ems_ldap_handler exec ems_user:find_by_login user: ~p.", [User]),
			{ok, User};
		Error -> Error
	end.

format_user_field(undefined) -> <<"">>;
format_user_field(null) -> <<"">>;
format_user_field([]) -> <<"">>;
format_user_field(Value) when is_integer(Value) -> integer_to_binary(Value);
format_user_field(Value) when is_boolean(Value) -> ems_util:boolean_to_binary(Value);
format_user_field(Value) when is_list(Value) -> list_to_binary(Value);
format_user_field(Value) when is_binary(Value) -> Value.
	

