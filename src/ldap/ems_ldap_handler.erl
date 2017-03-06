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
				password_admin}).   %% Password of admin ldap

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Service) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Service]),
	{ok, Pid}.

init(Ref, Socket, Transport, [#service{datasource = Datasource,
										middleware = Middleware,
										properties = Props}]) ->
	ok = ranch:accept_ack(Ref),
	LdapAdmin = maps:get(<<"ldap_admin">>, Props),
	LdapPasswdAdmin = maps:get(<<"ldap_password_admin">>, Props),
    State = #state{datasource = Datasource,
				   middleware = Middleware,
				   admin = LdapAdmin,
				   password_admin = LdapPasswdAdmin
			   },
	loop(Socket, Transport, State).

loop(Socket, Transport, State) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			case decode_ldap_message(Data) of
				{ok, LdapMessage} ->
					ems_logger:info("ems_ldap_handler request: ~p.", [LdapMessage]),
					MessageID = LdapMessage#'LDAPMessage'.messageID,
					Result = handle_request(LdapMessage, State),
					case Result of
						{ok, unbindRequest} ->
							?DEBUG("ems_ldap_handler unbindRequest."),
							Transport:close(Socket);
						{ok, Msg} -> 
							?DEBUG("ems_ldap_handler response: ~p.", [Msg]),
							Response = [ encode_response(MessageID, M) || M <- Msg ],
							Transport:send(Socket, Response)
					end,
					loop(Socket, Transport, State);
				{error, Reason} ->
					ems_logger:error("ems_ldap_handler decode invalid message. Reason: ~p.", [Reason]),
					Transport:close(Socket),
					loop(Socket, Transport, State)
			end;
		_ ->
			Transport:close(Socket)
	end.


encode_response(MessageID, Msg) ->
	Response = #'LDAPMessage'{messageID = MessageID,
							  protocolOp = Msg,
							  controls = asn1_NOVALUE},
    case asn1rt:encode('LDAP', 'LDAPMessage', Response) of
        {ok, Result} -> Result;
        {error, Reason} -> {error, Reason}
    end.


decode_ldap_message(RequestBin) ->
	case asn1rt:decode('LDAP', 'LDAPMessage', RequestBin) of
        {ok, {'LDAPMessage', _MessageID, _ProtocolOp, _} = LdapMsg} -> {ok, LdapMsg};
		Error -> Error
    end.

  
handle_request({'LDAPMessage', _,
					{bindRequest, #'BindRequest'{version = _Version, 
												 name = Name, 
												 authentication = {_, Password}}} = Msg,
				 _}, State = #state{admin = AdminLdap, 
									password_admin = PasswordAdminLdap}) ->
	 <<Cn:3/binary, _/binary>> = Name,
	case Cn of
		<<"cn=">> ->
			BindResponse = case Name =:= AdminLdap andalso Password =:= PasswordAdminLdap of
				true -> 
					ems_logger:info("ems_ldap_handler bind ~p success.", [Name]),
					make_bind_response(success, Name);
				_-> 
					ems_logger:error("ems_ldap_handler bind ~p does not exist.", [Name]),
					make_bind_response(invalidCredentials, Name)
			end;
		<<"uid">> -> 
			<<_:4/binary, UserLogin/binary>> = hd(binary:split(Name, <<",">>)),
			BindResponse = case middleware_autentica(UserLogin, Password, State) of
				{error, _Reason} ->	
					ems_logger:error("ems_ldap_handler bind ~p does not exist.", [Name]),
					make_bind_response(invalidCredentials, Name);
				ok -> 
					ems_logger:info("ems_ldap_handler bind ~p success.", [Name]),
					make_bind_response(success, Name)
			end;
		_UnknowCn -> 
			ems_logger:error("ems_ldap_handler bind unknow ~p.", [Msg]),
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

make_result_entry(UsuLogin, {UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha, Type, TypeEmail, CtrlInsert, CtrlUpdate}, AdminLdap) ->
	ObjectName = make_object_name(UsuLogin),
	{searchResEntry, #'SearchResultEntry'{objectName = ObjectName,
										  attributes = [#'PartialAttribute'{type = <<"uid">>, vals = [UsuLogin]},
														#'PartialAttribute'{type = <<"cn">>, vals = [AdminLdap]},
														#'PartialAttribute'{type = <<"mail">>, vals = [UsuEmail]},
														#'PartialAttribute'{type = <<"email">>, vals = [UsuEmail]},
														#'PartialAttribute'{type = <<"cpf">>, vals = [UsuCpf]},
														#'PartialAttribute'{type = <<"passwd">>, vals = [UsuSenha]},
														#'PartialAttribute'{type = <<"givenName">>, vals = [UsuNome]},
														#'PartialAttribute'{type = <<"employeeNumber">>, vals = [UsuId]},
														#'PartialAttribute'{type = <<"distinguishedName">>, vals = [UsuId]},
														#'PartialAttribute'{type = <<"ems_type_user">>, vals = [Type]},
														#'PartialAttribute'{type = <<"ems_type_email">>, vals = [TypeEmail]},
														#'PartialAttribute'{type = <<"ems_ctrl_insert">>, vals = [CtrlInsert]},
														#'PartialAttribute'{type = <<"ems_ctrl_update">>, vals = [CtrlUpdate]}
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
handle_request_search_login(UserLogin, State = #state{admin = AdminLdap}) ->	
	case middleware_find_user_by_login(UserLogin, State) of
		{error, enoent} ->
			ems_logger:error("ems_ldap_handler search ~p does not exist.", [UserLogin]),
			ResultDone = make_result_done(invalidCredentials),
			{ok, [ResultDone]};
		{error, Reason} ->
			ems_logger:error("ems_ldap_handler search ~p fail. Reason: ~p.", [UserLogin, Reason]),
			ResultDone = make_result_done(unavailable),
			{ok, [ResultDone]};
		{ok, UserRecord = {_, UsuNome, _, _, _, _, _, _, _}} ->
			ems_logger:info("ems_ldap_handler search ~p ~p success.", [UserLogin, UsuNome]),
			ResultEntry = make_result_entry(UserLogin, UserRecord, AdminLdap),
			ResultDone = make_result_done(success),
			{ok, [ResultEntry, ResultDone]}
	end.
	

middleware_autentica(UserLogin, UserPassword, #state{middleware = undefined}) ->
	ems_user:authenticate_login_password(UserLogin, UserPassword);
middleware_autentica(UserLogin, UserPassword, #state{middleware = Middleware, 
													 datasource = Datasource}) ->
	case code:ensure_loaded(Middleware) of
		{module, _} ->
			case erlang:function_exported(Middleware, autentica, 3) of
				true -> apply(Middleware, autentica, [UserLogin, UserPassword, Datasource]);
				false -> {error, einvalid_middleware}
			end;
		{error, Reason} -> {
			error, {Reason, Middleware}}
	end.

middleware_find_user_by_login(UserLogin, #state{middleware = undefined}) ->
	case ems_user:find_by_login(UserLogin) of
		{ok, _User = #user{codigo = UsuId, 
						   name = UsuNome, 
						   cpf = UsuCpf, 
						   email = UsuEmail, 
						   password = UsuSenha, 
						   type = Type, 
						   type_email = TypeEmail, 
						   ctrl_insert = CtrlInsert, 
						   ctrl_update = CtrlUpdate}} ->
			?DEBUG("ems_ldap_handler exec ems_user:find_by_login user: ~p.", [_User]),
			UserRecord2 = {format_user_field(UsuId),
						   format_user_field(UsuNome),
						   format_user_field(UsuCpf),
						   format_user_field(UsuEmail),
						   format_user_field(UsuSenha),
						   format_user_field(Type),
						   format_user_field(TypeEmail),
						   format_user_field(CtrlInsert),
						   format_user_field(CtrlUpdate)}, 
			{ok, UserRecord2};
		Error -> Error
	end;
middleware_find_user_by_login(UserLogin, #state{middleware = Middleware, 
												datasource = Datasource}) ->
	case code:ensure_loaded(Middleware) of
		{module, _} ->
			case erlang:function_exported(Middleware, find_user_by_login, 2) of
				true -> 
					case apply(Middleware, find_user_by_login, [UserLogin, Datasource]) of
						{ok, {UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}} ->
							?DEBUG("ems_ldap_handler exec middleware_find_user_by_login user: ~p.", [{UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}]),
							UserRecord2 = {format_user_field(UsuId),
										   format_user_field(UsuNome),
										   format_user_field(UsuCpf),
										   format_user_field(UsuEmail),
										   format_user_field(UsuSenha),
										   <<>>,
										   <<>>,
										   <<>>,
										   <<>>}, 
							{ok, UserRecord2};
						Error -> Error
					end;
				false -> {error, einvalid_middleware}
			end;
		{error, Reason} -> {
			error, {Reason, Middleware}}
	end.


format_user_field(undefined) -> <<"">>;
format_user_field(null) -> <<"">>;
format_user_field([]) -> <<"">>;
format_user_field(Number) when is_integer(Number) -> list_to_binary(integer_to_list(Number));
format_user_field(Text) when is_list(Text) -> list_to_binary(string:strip(Text));
format_user_field(Text) when is_binary(Text) -> Text.
	

