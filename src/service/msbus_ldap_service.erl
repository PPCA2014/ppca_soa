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
	{Result, NewState} = do_search(Request, State),
	msbus_eventmgr:notifica_evento(ok_request, {servico, Request, Result}),
	{noreply, NewState}.
    
handle_call({search, Request}, _From, State) ->
	{Result, NewState} = do_search(Request, State),
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
    
do_search(_Request, State) ->
	%-record('SearchResultEntry',{
	%objectName, attributes}).

	%-record('LDAPResult',{
	%resultCode, matchedDN, diagnosticMessage, referral = asn1_NOVALUE}).



	Result = #'LDAPMessage'{messageID = 1,
						   protocolOp = {bindResponse, #'BindResponse'{	resultCode = success,
																		matchedDN =  <<"uid=agilar,dc=unb,dc=com">>,
																		diagnosticMessage = <<"">>,
																		referral = asn1_NOVALUE,
																		serverSaslCreds = asn1_NOVALUE}},
						   controls = asn1_NOVALUE},



	%Entry = #'SearchResultEntry'{
	%	objectName = "uid=tobbe,ou=People,dc=bluetail,dc=com",
	%	attributes = [{"cn",["Torbjorn Tornkvist"]}]
	%},

	%LPAPResult = #'LDAPResult'{
	%	resultCode = 0, 
	%	matchedDN = <<"teste">>, 
	%	diagnosticMessage = <<"teste">>, 
	%	referral = asn1_NOVALUE
	%},
	
	%BindResponse = #'BindResponse'{
	%			resultCode = 1,
	%			matchedDN = <<"com">>,
	%			diagnosticMessage = <<"teste teste">>,
	%			referral = asn1_NOVALUE, 
	%			serverSaslCreds = asn1_NOVALUE
	%		},
	
	
	%T = #eldap_search_result{
    %   entries = [#eldap_entry{
    %                 object_name = "uid=tobbe,ou=People,dc=bluetail,dc=com",
    %                 attributes = [{"cn",["Torbjorn Tornkvist"]}]}],
    %   referrals = []},

	%T = {
    %   entries, [{
    %                 object_name, "uid=tobbe,ou=People,dc=bluetail,dc=com",
    %                 attributes, [{"cn",["Torbjorn Tornkvist"]}]
    %             }],
    %   referrals, []},

	
	io:format("Mensagem saida: ~p\n\n", [Result]),
	{{ok, Result}, State}.

