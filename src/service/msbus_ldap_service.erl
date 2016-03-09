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
-export([execute/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Stores the state of the service.
-record(state, {}). 


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
%% Cliente API
%%====================================================================
 
execute(Request, From)	->
	msbus_pool:cast(msbus_ldap_service_pool, {search, Request, From}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({search, Request, _From}, State) ->
	{Result, NewState} = handle_request(Request, State),
	msbus_eventmgr:notifica_evento(ok_request, {servico, Request, Result}),
	{noreply, NewState}.
    
handle_call({search, Request}, _From, State) ->
	{Result, NewState} = handle_request(Request, State),
	{reply, Result, NewState}.

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
											authentication = Authentication}}) ->
	io:format("processar bind\n\n"),
	Result = {bindResponse, #'BindResponse'{resultCode = success,
											matchedDN =  <<"uid=agilar,dc=unb,dc=com">>,
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
												attributes = Attributes}}) ->
	io:format("processar search request\n\n"),

	Result1 = {searchResEntry, #'SearchResultEntry'{objectName = <<"cn=Magnus Froberg, dc=bluetail, dc=com">>,
												    attributes = []
												   }
			  },
	
	Result2 = {searchResDone, #'LDAPResult'{resultCode = success, 
										   matchedDN = <<"cn=Magnus Froberg, dc=bluetail, dc=com">>, 
										   diagnosticMessage = <<"">>,
										   referral = asn1_NOVALUE}
	
			  },
	{ok, [Result2, Result1]}.


handle_request(#request{payload = LdapMsg}, State) ->
	io:format("Mensagem que vou tratar -> ~p\n\n\n", [LdapMsg]),
	{ok, Result} = handle_request(LdapMsg#'LDAPMessage'.protocolOp),
	io:format("Mensagem saida: ~p\n\n", [Result]),
	{{ok, Result}, State}.


