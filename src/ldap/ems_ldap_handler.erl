%%********************************************************************
%% @title Module ems_ldap_handler
%% @version 1.0.0
%% @doc Main module HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_ldap_handler).

-behaviour(ranch_protocol).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").
-include("../include/LDAP.hrl").

%  Stores the state of the service.
-record(state, {connection,			 	%% connection to ldap database
				datasource,		 		%% odbc datasource
				sql_find_user,		 	%% sql to find user
				admin,		 			%% admin ldap
				password_admin}).  %% Password of admin ldap

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
    Config = ems_config:getConfig(),
    State = #state{connection = close_connection,
				   datasource = Config#config.ldap_datasource,
				   sql_find_user = Config#config.ldap_sql_find_user,
				   admin = Config#config.ldap_admin,
				   password_admin = Config#config.ldap_password_admin
			   },
	loop(Socket, Transport, State).

loop(Socket, Transport, State) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			LdapMessage = decode_ldap_message(Data),
			io:format("msg is ~p\n", [LdapMessage]),
			MessageID = LdapMessage#'LDAPMessage'.messageID,
			Result = handle_request(LdapMessage, State),
			case Result of
				{ok, unbindRequest} ->
					Transport:close(Socket);
				{ok, Msg} -> 
					Response = lists:map(fun(M) -> encode_response(MessageID, M) end, Msg),
					Transport:send(Socket, Response)
			end,
			loop(Socket, Transport, State);
		_ ->
			ok = Transport:close(Socket)
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
        {ok, {'LDAPMessage', _MessageID, _ProtocolOp, _} = LdapMsg} ->
			LdapMsg;
		{error, Reason} -> 
			{error, Reason}
    end.

autentica("cn", DnAdmin, Password, State = #state{admin = AdminLdap, 
												  password_admin = PasswordAdminLdap}) -> 
	autentica(list_to_binary(DnAdmin), Password, AdminLdap, PasswordAdminLdap, State);

autentica("uid", DnUser, PasswordUser, State) -> 
	io:format("autentica  ~p ~p\n", [DnUser, PasswordUser]),
	case find_user_by_login(DnUser, State) of
		{error, unavailable, State2} -> 
			{unavailable, State2};
		{{}, State2} -> 
			{invalidCredentials, State2};
		{{_, _, _, _, PasswordLdap}, State2} -> 
			PasswordUser2 = criptografia(PasswordUser),
			autentica(DnUser, PasswordUser2, DnUser, PasswordLdap, State2)
	end.

autentica(Dn, Password, DnLdap, PasswordLdap, State) when Dn =:= DnLdap, Password =:= PasswordLdap -> {success, State};

autentica(_, _, _, _, State) -> {invalidCredentials, State}.

criptografia(Password) -> 
	Password2 = binary_to_list(Password),
	base64:encode(sha1:binstring(Password2)).
    
handle_request({'LDAPMessage', _,
					{bindRequest, #'BindRequest'{version = _Version, 
												 name = Name, 
												 authentication = {_, Password}}},
				 _}, State) ->
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
	UsuLogin = binary_to_list(UsuLoginBin),
	case find_user_by_login(UsuLogin, State) of
		{error, unavailable, State2} ->
			ResultDone = make_result_done(unavailable),
			{ok, [ResultDone]};
		{{}, State2} -> 
			ResultDone = make_result_done(invalidCredentials),
			{ok, [ResultDone]};
		{UserRecord, State2} ->
			ResultEntry = make_result_entry(UsuLoginBin, UserRecord, State2),
			ResultDone = make_result_done(success),
			{ok, [ResultEntry, ResultDone]}
	end;


handle_request({'LDAPMessage', _, 
					{unbindRequest, _},
				 _}, State) ->
	{ok, unbindRequest};
	
handle_request({'LDAPMessage', _, 
					UnknowMsg,
				 _}, State) ->
	{ok, unbindRequest}.
	




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

make_result_entry(UsuLogin, {UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}, #state{admin = AdminLdap}) ->
	ObjectName = make_object_name(UsuLogin),
	{searchResEntry, #'SearchResultEntry'{objectName = ObjectName,
										  attributes = [#'PartialAttribute'{type = <<"uid">>, vals = [UsuLogin]},
														#'PartialAttribute'{type = <<"cn">>, vals = [AdminLdap]},
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
	case true of
		true ->
			UserRecord2 = {format_user_field(1),
						   format_user_field("geral"),
						   format_user_field("00167743023"),
						   format_user_field("evertonagilar@gmail.com"),
						   format_user_field("123456")},
			{UserRecord2, State};
		false ->
			case get_database_connection(State) of
				close_connection -> 
					State2 = State#state{connection = close_connection},
					{error, unavailable, State2};
				Conn -> 
					Params = [{{sql_varchar, 100}, [UserLogin]}],
					try
						case odbc:param_query(Conn, Sql, Params, 3500) of
							{_, _, UserRecord} -> 
								State2 = State#state{connection = Conn},
								case UserRecord of
									[{UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}] ->
										UserRecord2 = {format_user_field(UsuId),
													   format_user_field(UsuNome),
													   format_user_field(UsuCpf),
													   format_user_field(UsuEmail),
													   format_user_field(UsuSenha)}, 
										{UserRecord2, State2};
									_->
										{{}, State2}
								end;
							{error, Reason} ->
								State2 = State#state{connection = close_connection},
								ems_logger:error("Query ldap database error. Reason: ~p", [Reason]),
								{error, unavailable, State2}
						end
					catch
						_Exception:Reason3 -> 
							State3 = State#state{connection = close_connection},
							ems_logger:error("Connection or query ldap database error. Reason: ~p", [Reason3]),
							{error, unavailable, State3}
					end
			end
	end.

format_user_field(null) -> <<"">>;
format_user_field([]) -> <<"">>;
format_user_field(Number) when is_integer(Number) -> list_to_binary(integer_to_list(Number));
format_user_field(Text) -> list_to_binary(string:strip(Text)).
	

get_database_connection(#state{connection = close_connection, 
							   datasource = DataSource}) ->
	try
		case odbc:connect(DataSource, [{scrollable_cursors, off},
									   {timeout, 3500},
									   {trace_driver, off}]) of
			{ok, Conn}	->																	  
				ems_logger:info("Create connection to ldap datasource ~p.", [DataSource]),
				Conn;
			{error, Reason} -> 
				ems_logger:error("Connection ldap database error. Reason: ~p", [Reason]),
				close_connection
		end
	catch
		_Exception:Reason2 -> 
			ems_logger:error("Connection ldap database error. Reason: ~p", [Reason2]),
			close_connection
	end;

	
get_database_connection(#state{connection = Conn}) -> Conn.
