%%********************************************************************
%% @title Módulo ems_user_service
%% @version 1.0.0
%% @doc Módulo de serviço ems_user_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).


-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API  
-export([start/1, start_link/1, stop/0]).

%% Cliente interno API
-export([get/2, insert/2, update/2, delete/2, all/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do service. 
-record(state, {}). 


%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
get(Request, From)	->
	ems_pool:cast(ems_user_service, {get, Request, From}).
	
insert(Request, From)	->
	ems_pool:cast(ems_user_service, {insert, Request, From}).

update(Request, From)	->
	ems_pool:cast(ems_user_service, {update, Request, From}).

delete(Request, From)	->
	ems_pool:cast(ems_user_service, {delete, Request, From}).

all(Request, From)	->
	ems_pool:cast(ems_user_service, {all, Request, From}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({get, Request, _From}, State) ->
	Reply = do_get(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({insert, Request, _From}, State) ->
	Reply = do_insert(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({update, Request, _From}, State) ->
	Reply = do_update(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({delete, Request, _From}, State) ->
	Reply = do_delete(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({all, Request, _From}, State) ->
	Reply = do_all(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Reply}),
	{noreply, State}.
    
handle_call({get, Request}, _From, State) ->
	Reply = do_get(Request, State),
	{reply, Reply, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================

do_all(_Request, _State) -> ems_user:all().
	
do_get(Request, _State) ->
	Id = ems_request:get_param_url(<<"id">>, -1, Request),
	ems_user:get(Id).

do_insert(Request, _State) ->
	UserJson = ems_request:get_property_request(<<"payload">>, Request),
	User = #user{name  = maps:get(<<"name">>, UserJson, ""),
				 email = maps:get(<<"email">>, UserJson, ""),
				 password = maps:get(<<"password">>, UserJson, "")},
	ems_user:insert(User).

do_update(Request, _State) ->
	Id = ems_request:get_param_url(<<"id">>, -1, Request),
	UserJson = ems_request:get_property_request(<<"payload">>, Request),
	case ems_user:get(Id) of
		{ok, User} -> 
			User2 = User#user{name  = maps:get(<<"name">>, UserJson, User#user.name),
							  email = maps:get(<<"email">>, UserJson, User#user.email),
							  password = maps:get(<<"password">>, UserJson, User#user.password)},
			ems_user:update(User2);
		Error -> Error
	end.

do_delete(Request, _State) ->
	Id = ems_request:get_param_url(<<"id">>, -1, Request),
	ems_user:delete(Id).
	

