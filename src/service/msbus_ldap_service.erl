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
    Conn = get_database_connection(),
    State = #state{connection = Conn},
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
    
handle_request({bindRequest, #'BindRequest'{version = Version, 
											name = Name, 
											authentication = Authentication}}, State) ->
	%io:format("processar bind\n\n"),
	Result = {bindResponse, #'BindResponse'{resultCode = success,
											matchedDN =  <<"uid=edmilsoncosme,ou=funcdis,ou=Classes,dc=unb,dc=br">>,
											diagnosticMessage = <<"">>,
											referral = asn1_NOVALUE,
											serverSaslCreds = asn1_NOVALUE}
			 },
	{ok, Result};
    

handle_request({searchRequest, #'SearchRequest'{baseObject = BaseObject, 
												scope = Scope, 
												derefAliases = DerefAliases, 
												sizeLimit = SizeLimit, 
												timeLimit = TimeLimit, 
												typesOnly = TypesOnly, 
												filter = Filter, 
												attributes = Attributes}}, State) ->
	%io:format("processar search request\n\n"),

	UsuLogin = "geral",
	{UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha} = find_user_by_login(UsuLogin, State),
	io:format("dados da consulta ~p\n\n", [{UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}]),


	Result1 = {searchResEntry, #'SearchResultEntry'{objectName = <<"uid=edmilsoncosme,ou=funcdis,ou=Classes,dc=unb,dc=br">>,
												    attributes = [#'PartialAttribute'{type = <<"uid">>, vals = [list_to_binary(UsuLogin)]},
																  #'PartialAttribute'{type = <<"cn">>, vals = [list_to_binary(UsuNome)]},
																  #'PartialAttribute'{type = <<"mail">>, vals = [list_to_binary(string:strip(UsuEmail))]},
																  #'PartialAttribute'{type = <<"givenName">>, vals = [list_to_binary(UsuNome)]},
																  #'PartialAttribute'{type = <<"employeeNumber">>, vals = [list_to_binary(integer_to_list(UsuId))]}
																 ]
												   }
			  },
	

	
	Result2 = {searchResDone, #'LDAPResult'{resultCode = success, 
										   matchedDN = <<"">>, 
										   diagnosticMessage = <<"">>,
										   referral = asn1_NOVALUE}
	
			  },


	Result3 = {unbindRequest, <<"">>},

	
	{ok, [Result1, Result2, Result3]};


handle_request({unbindRequest, _}, State) ->
	%io:format("unbindRequest\n\n"),
	{ok, unbindRequest};


handle_request(#request{payload = LdapMsg}, State) ->
	{ok, Result} = handle_request(LdapMsg#'LDAPMessage'.protocolOp, State),
	{{ok, Result}, State}.


find_user_by_login(UserLogin, #state{connection = Conn}) ->
	Sql = "select  top 1 p.PesCodigoPessoa, p.PesNome, p.PesCpf, p.PesEmail, u.UsuSenha from BDPessoa.dbo.TB_Pessoa p left join BDAcesso.dbo.TB_Usuario u on p.PesCodigoPessoa = u.UsuPesIdPessoa where u.UsuLogin = ?", 
	Params = [{{sql_varchar, 100}, [UserLogin]}],
	ResultSet = odbc:param_query(Conn, Sql, Params),
	 {_, _, [User={UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}]} = ResultSet,
	User.
	
get_database_connection() ->
	io:format("Criando conexão...\n"),
	{ok, Conn} = odbc:connect("DSN=pessoa;UID=usupessoa;PWD=usupessoa", []),
	io:format("Criando conexão: OK!\n"),
	Conn.




