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
-record(state, {connection}). 


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
    State = #state{connection = close_connection},
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
    
handle_request({bindRequest, #'BindRequest'{version = _Version, 
											name = _Name, 
											authentication = _Authentication}}, _State) ->
	BindResponse = make_bind_response(),
	{ok, [BindResponse]};
    

handle_request({searchRequest, #'SearchRequest'{baseObject = _BaseObject, 
												scope = _Scope, 
												derefAliases = _DerefAliases, 
												sizeLimit = _SizeLimit, 
												timeLimit = _TimeLimit, 
												typesOnly = _TypesOnly, 
												filter = _Filter, 
												attributes = _Attributes}}, State) ->
	UsuLogin = "geral",
	case find_user_by_login(UsuLogin, State) of
		{UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha} ->
			ResultEntry = make_result_entry(UsuLogin, {UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}),
			ResultDone = make_result_done(success),
			{ok, [ResultEntry, ResultDone]};
		{error, ldap_out} ->
			ResultDone = make_result_done(operationsError),
			{ok, [ResultDone]}
	end;


handle_request({unbindRequest, _}, _State) ->
	{ok, unbindRequest};


handle_request(#request{payload = LdapMsg}, State) ->
	{ok, Result} = handle_request(LdapMsg#'LDAPMessage'.protocolOp, State),
	{{ok, Result}, State}.


make_bind_response() ->
	{bindResponse, #'BindResponse'{resultCode = success,
												  matchedDN =  <<"uid=edmilsoncosme,ou=funcdis,ou=Classes,dc=unb,dc=br">>,
												  diagnosticMessage = <<"">>,
												  referral = asn1_NOVALUE,
												  serverSaslCreds = asn1_NOVALUE}
	}.

make_result_entry(UsuLogin, {UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}) ->
	{searchResEntry, #'SearchResultEntry'{objectName = <<"uid=edmilsoncosme,ou=funcdis,ou=Classes,dc=unb,dc=br">>,
												    attributes = [#'PartialAttribute'{type = <<"uid">>, vals = [list_to_binary(UsuLogin)]},
																  #'PartialAttribute'{type = <<"cn">>, vals = [list_to_binary(UsuNome)]},
																  #'PartialAttribute'{type = <<"mail">>, vals = [list_to_binary(string:strip(UsuEmail))]},
																  #'PartialAttribute'{type = <<"cpf">>, vals = [list_to_binary(string:strip(UsuCpf))]},
																  #'PartialAttribute'{type = <<"passwd">>, vals = [list_to_binary(string:strip(UsuSenha))]},
																  #'PartialAttribute'{type = <<"givenName">>, vals = [list_to_binary(UsuNome)]},
																  #'PartialAttribute'{type = <<"employeeNumber">>, vals = [list_to_binary(integer_to_list(UsuId))]}
																 ]
												   }
	}.


make_result_done(ResultCode) ->
	{searchResDone, #'LDAPResult'{resultCode = ResultCode, 
										   matchedDN = <<"">>, 
										   diagnosticMessage = <<"">>,
										   referral = asn1_NOVALUE}
	
	}.
	

find_user_by_login(UserLogin, State) ->
	case get_database_connection(State) of
		close_connection -> 
			{error, ldap_out};
		Conn -> 
			Sql = "select  top 1 p.PesCodigoPessoa, p.PesNome, p.PesCpf, p.PesEmail, u.UsuSenha from BDPessoa.dbo.TB_Pessoa p left join BDAcesso.dbo.TB_Usuario u on p.PesCodigoPessoa = u.UsuPesIdPessoa where u.UsuLogin = ?", 
			Params = [{{sql_varchar, 100}, [UserLogin]}],
			ResultSet = odbc:param_query(Conn, Sql, Params),
			 {_, _, [User]} = ResultSet,
			User
	end.


get_database_connection(#state{connection = close_connection}) ->
	try
		case odbc:connect("DSN=pessoa;UID=usupessoa;PWD=usupessoa", [{scrollable_cursors, off},
																	 {timeout, 3500},
																	 {trace_driver, off}]) of
			{ok, Conn}	->																	  
				io:format("Criando conexão: OK!\n"),
				Conn;
			{error, Reason} -> 
				io:format("Connection ldap database error. Reason: ~p\n", [Reason]),
				close_connection
		end
	catch
		_Exception:Reason2 -> 
			io:format("Connection ldap database error. Reason: ~p\n", [Reason2]),
			close_connection
	end;

	
get_database_connection(#state{connection = Conn}) -> Conn.
	
			




