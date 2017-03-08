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
					end,
					loop(Socket, Transport, State);
				{error, Reason} ->
					ems_logger:error("ems_ldap_handler decode invalid message. Reason: ~p.", [Reason]),
					Transport:close(Socket),
					loop(Socket, Transport, State)
			end;
		_ ->
			?DEBUG("ems_ldap_handler close socket."),
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
												 authentication = {_, Password}}},
				 _}, State = #state{admin = AdminLdap, 
									password_admin = PasswordAdminLdap}) ->
	 <<Cn:3/binary, _/binary>> = Name,
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
			BindResponse = case middleware_autentica(UserLogin, Password, State) of
				ok -> 
					ems_logger:info("ems_ldap_handler bind_uid ~p success.", [Name]),
					make_bind_response(success, Name);
				{error, _Reason} ->	
					ems_logger:error("ems_ldap_handler bind_uid ~p invalid credential.", [Name]),
					make_bind_response(invalidCredentials, Name)
			end;
		_ -> 
			BindResponse = case middleware_autentica(Name, Password, State) of
				ok -> 
					ems_logger:info("ems_ldap_handler bind ~p success.", [Name]),
					make_bind_response(success, Name);
				{error, _Reason} ->	
					ems_logger:error("ems_ldap_handler bind ~p invalid credential.", [Name]),
					make_bind_response(invalidCredentials, Name)
			end
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

make_result_entry(#user{codigo = UsuId, 
					    login = UsuLogin,	
					    name = UsuName, 
					    cpf = UsuCpf, 
					    email = UsuEmail, 
					    password = UsuPasswd, 
					    type = UsuType, 
					    type_email = UsuTypeEmail, 
					    ctrl_insert = UsuCtrlInsert, 
						ctrl_update = UsuCtrlUpdate}, 
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
	{searchResEntry, #'SearchResultEntry'{objectName = ObjectName,
										  attributes = [#'PartialAttribute'{type = <<"uid">>, vals = [UsuId2]},
														#'PartialAttribute'{type = <<"cn">>, vals = [AdminLdap]},
														#'PartialAttribute'{type = <<"mail">>, vals = [UsuEmail2]},
														#'PartialAttribute'{type = <<"login">>, vals = [UsuLogin2]},
														#'PartialAttribute'{type = <<"email">>, vals = [UsuEmail2]},
														#'PartialAttribute'{type = <<"cpf">>, vals = [UsuCpf2]},
														#'PartialAttribute'{type = <<"passwd">>, vals = [UsuSenha2]},
														#'PartialAttribute'{type = <<"givenName">>, vals = [UsuNome2]},
														#'PartialAttribute'{type = <<"employeeNumber">>, vals = [UsuId2]},
														#'PartialAttribute'{type = <<"distinguishedName">>, vals = [UsuLogin2]},
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
		{ok, User} ->
			ems_logger:info("ems_ldap_handler search ~p ~p success.", [UserLogin, User#user.name]),
			ResultEntry = make_result_entry(User, AdminLdap),
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
		{ok, User} ->
			?DEBUG("ems_ldap_handler exec ems_user:find_by_login user: ~p.", [User]),
			{ok, User};
		Error -> Error
	end;
middleware_find_user_by_login(UserLogin, #state{middleware = Middleware, 
												datasource = Datasource}) ->
	case code:ensure_loaded(Middleware) of
		{module, _} ->
			case erlang:function_exported(Middleware, find_user_by_login, 2) of
				true -> 
					case apply(Middleware, find_user_by_login, [UserLogin, Datasource]) of
						{ok, User} ->
							?DEBUG("ems_ldap_handler exec middleware_find_user_by_login user: ~p.", [User]),
							{ok, User};
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
	

