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

-record(state, {listener_name,
				server_name,
				admin,		 		%% admin ldap
				password_admin,     %% Password of admin ldap
				tcp_allowed_address_t,
				bind_cn_success_metric_name,
				bind_uid_success_metric_name,
				bind_success_metric_name,
				bind_cn_invalid_credential_metric_name,
				bind_uid_invalid_credential_metric_name,
				bind_invalid_credential_metric_name,
				search_invalid_credential_metric_name,
				search_unavailable_metric_name,
				search_success_metric_name,
				host_denied_metric_name,
				error_metric_name,
				request_capabilities_metric_name	
			}).   


-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Service) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Service]),
	{ok, Pid}.

init(Ref, Socket, Transport, [State]) ->
	ranch:accept_ack(Ref),
	loop(Socket, Transport, State).

loop(Socket, Transport, State = #state{tcp_allowed_address_t = AllowedAddress,
									   host_denied_metric_name = HostDeniedMetricName,
									   error_metric_name = ErrorMetricName}) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			case inet:peername(Socket) of
				{ok, {Ip,_Port}} ->
					case ems_util:allow_ip_address(Ip, AllowedAddress) of				
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
									ems_db:inc_counter(ErrorMetricName),
									ems_logger:error("ems_ldap_handler decode invalid message. Reason: ~p.", [Reason]),
									ResultDone = make_result_done(inappropriateMatching),
									Response = [ encode_response(1, ResultDone) ],
									Transport:send(Socket, Response),
									Transport:close(Socket)
							end;
						false ->
							ems_db:inc_counter(HostDeniedMetricName),
							ems_logger:warn("ems_ldap_handler does not grant access to IP ~p. Reason: IP denied.", [Ip]),
							ResultDone = make_result_done(insufficientAccessRights),
							Response = [ encode_response(1, ResultDone) ],
							Transport:send(Socket, Response),
							Transport:close(Socket)
					end;
				Error -> 
					ems_db:inc_counter(ErrorMetricName),
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
							password_admin = PasswordAdminLdap,
							bind_cn_success_metric_name = BindCnSuccessMetricName,
						    bind_uid_success_metric_name = BindUidSuccessMetricName,
						    bind_success_metric_name = BindSuccessMetricName,
						    bind_cn_invalid_credential_metric_name = BindCnInvalidCredentialMetricName,
						    bind_uid_invalid_credential_metric_name = BindUidInvalidCredentialMetricName,
						    bind_invalid_credential_metric_name = BindInvalidCredentialMetricName}) ->
	case Name of
		<<Cn:3/binary, _/binary>> ->
			case Cn of
				<<"cn=">> ->
					BindResponse = case Name =:= AdminLdap andalso Password =:= PasswordAdminLdap of
						true -> 
							ems_db:inc_counter(BindCnSuccessMetricName),
							ems_logger:info("ems_ldap_handler bind_cn ~p success.", [Name]),
							make_bind_response(success, Name);
						_-> 
							ems_db:inc_counter(BindCnInvalidCredentialMetricName),
							ems_logger:error("ems_ldap_handler bind_cn ~p invalid credential.", [Name]),
							make_bind_response(invalidCredentials, Name)
					end;
				<<"uid">> -> 
					<<_:4/binary, UserLogin/binary>> = hd(binary:split(Name, <<",">>)),
					BindResponse = case do_authenticate(UserLogin, Password) of
						ok -> 
							ems_db:inc_counter(BindUidSuccessMetricName),
							ems_logger:info("ems_ldap_handler bind_uid ~p success.", [Name]),
							make_bind_response(success, Name);
						{error, _Reason} ->	
							ems_db:inc_counter(BindUidInvalidCredentialMetricName),
							ems_logger:error("ems_ldap_handler bind_uid ~p invalid credential.", [Name]),
							make_bind_response(invalidCredentials, Name)
					end;
				_ -> 
					BindResponse = case do_authenticate(Name, Password) of
						ok -> 
							ems_db:inc_counter(BindSuccessMetricName),
							ems_logger:info("ems_ldap_handler bind ~p success.", [Name]),
							make_bind_response(success, Name);
						{error, _Reason} ->	
							ems_db:inc_counter(BindInvalidCredentialMetricName),
							ems_logger:error("ems_ldap_handler bind ~p invalid credential.", [Name]),
							make_bind_response(invalidCredentials, Name)
					end
			end;
		_ -> 
			ems_db:inc_counter(BindInvalidCredentialMetricName),
			ems_logger:error("ems_ldap_handler bind ~p invalid credential.", [Name]),
			BindResponse = make_bind_response(invalidCredentials, Name)
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
				 _}, #state{request_capabilities_metric_name = RequestCapabilitiesMetricName}) ->
	ems_db:inc_counter(RequestCapabilitiesMetricName),	
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

make_result_entry(#user{id = UsuId, 
                        codigo = CodigoPessoa,
					    login = UsuLogin,	
					    name = UsuName, 
					    cpf = UsuCpf, 
					    email = UsuEmail, 
					    password = UsuPasswd, 
					    type = UsuType, 
					    subtype = UsuSubType,
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
						nome_pai = NomePai,
						nome_mae = NomeMae,
						nacionalidade = Nacionalidade
}, 
				  AdminLdap) ->
	UsuId2 = format_user_field(UsuId),
	ObjectName = make_object_name(UsuLogin),
	CodigoPessoa2 = format_user_field(CodigoPessoa),
	UsuLogin2 = format_user_field(UsuLogin),
	UsuNome2 = format_user_field(UsuName),
	UsuCpf2 = format_user_field(UsuCpf),
	UsuEmail2 = format_user_field(UsuEmail),
	UsuSenha2 = format_user_field(UsuPasswd),
	UsuType2 = format_user_field(UsuType),
	UsuSubType2 = format_user_field(UsuSubType),
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
	NomePai2 = format_user_field(NomePai),
	NomeMae2 = format_user_field(NomeMae),
	Nacionalidade2 = format_user_field(Nacionalidade),
	Matricula2 = format_user_field(Matricula),
	Names = binary:split(UsuName, <<" ">>),
	SN = format_user_field(lists:last(Names)),
	GivenName = format_user_field(hd(Names)),

	{searchResEntry, #'SearchResultEntry'{objectName = ObjectName,
										  attributes = [#'PartialAttribute'{type = <<"uid">>, vals = [CodigoPessoa2]},
 														#'PartialAttribute'{type = <<"employeeNumber">>, vals = [CodigoPessoa2]},
														#'PartialAttribute'{type = <<"uidNumber">>, vals = [CodigoPessoa2]},
														#'PartialAttribute'{type = <<"usu_id">>, vals = [UsuId2]},
														
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
														#'PartialAttribute'{type = <<"nome_pai">>, vals = [NomePai2]},
														#'PartialAttribute'{type = <<"nome_mae">>, vals = [NomeMae2]},
														#'PartialAttribute'{type = <<"nacionalidade">>, vals = [Nacionalidade2]},
														#'PartialAttribute'{type = <<"matricula">>, vals = [Matricula2]},

														#'PartialAttribute'{type = <<"type">>, vals = [UsuType2]},
														#'PartialAttribute'{type = <<"subtype">>, vals = [UsuSubType2]},
														#'PartialAttribute'{type = <<"type_email">>, vals = [UsuTypeEmail2]},

														#'PartialAttribute'{type = <<"ctrl_insert">>, vals = [UsuCtrlInsert2]},
														#'PartialAttribute'{type = <<"ctrl_update">>, vals = [UsuCtrlUpdate2]}

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
handle_request_search_login(UserLogin, #state{admin = AdminLdap,
										      search_invalid_credential_metric_name = SearchInvalidCredentialMetricName,
											  search_unavailable_metric_name = SearchUnavailableMetricName,
											  search_success_metric_name = SearchSuccessMetricName}) ->	
	case ems_user:find_by_login_with_metric(UserLogin) of
		{error, enoent} ->
			ems_db:inc_counter(SearchInvalidCredentialMetricName),
			ems_logger:error("ems_ldap_handler search ~p does not exist.", [UserLogin]),
			ResultDone = make_result_done(invalidCredentials),
			{ok, [ResultDone]};
		{error, Reason} ->
			ems_db:inc_counter(SearchUnavailableMetricName),
			ems_logger:error("ems_ldap_handler search ~p fail. Reason: ~p.", [UserLogin, Reason]),
			ResultDone = make_result_done(unavailable),
			{ok, [ResultDone]};
		{ok, User} ->
			ems_db:inc_counter(SearchSuccessMetricName),
			ems_logger:info("ems_ldap_handler search ~p ~p success.", [UserLogin, User#user.name]),
			ResultEntry = make_result_entry(User, AdminLdap),
			ResultDone = make_result_done(success),
			{ok, [ResultEntry, ResultDone]}
	end.
	

do_authenticate(UserLogin, UserPassword) ->
	ems_user:authenticate_login_password(UserLogin, UserPassword).

format_user_field(undefined) -> <<"">>;
format_user_field(null) -> <<"">>;
format_user_field([]) -> <<"">>;
format_user_field(Value) when is_integer(Value) -> integer_to_binary(Value);
format_user_field(Value) when is_boolean(Value) -> ems_util:boolean_to_binary(Value);
format_user_field(Value) when is_list(Value) -> list_to_binary(Value);
format_user_field(Value) when is_binary(Value) -> Value.
	

