%%********************************************************************
%% @title Módulo msbus_user_service
%% @version 1.0.0
%% @doc Módulo de serviço msbus_user_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_user_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).


-include("../../include/msbus_config.hrl").
-include("../../include/msbus_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API  
-export([start/0, start_link/1, stop/0]).

%% Cliente interno API
-export([get/2, insert/2, update/2, delete/2, all/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
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
 
get(Request, From)	->
	poolboy:transaction(msbus_user_service_pool, fun(Worker) ->
		gen_server:cast(Worker, {get, Request, From})
    end).
	
insert(Request, From)	->
	poolboy:transaction(msbus_user_service_pool, fun(Worker) ->
		gen_server:cast(Worker, {insert, Request, From})
    end).

update(Request, From)	->
	poolboy:transaction(msbus_user_service_pool, fun(Worker) ->
		gen_server:cast(Worker, {update, Request, From})
    end).

delete(Request, From)	->
	poolboy:transaction(msbus_user_service_pool, fun(Worker) ->
		gen_server:cast(Worker, {delete, Request, From})
    end).

all(Request, From)	->
	poolboy:transaction(msbus_user_service_pool, fun(Worker) ->
		gen_server:cast(Worker, {all, Request, From})
    end).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({get, Request, From}, State) ->
	Reply = do_get(Request, State),
	gen_server:cast(From, {servico, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({insert, Request, From}, State) ->
	Reply = do_insert(Request, State),
	gen_server:cast(From, {servico, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({update, Request, From}, State) ->
	Reply = do_update(Request, State),
	gen_server:cast(From, {servico, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({delete, Request, From}, State) ->
	Reply = do_delete(Request, State),
	gen_server:cast(From, {servico, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({all, Request, From}, State) ->
	Reply = do_all(Request, State),
	gen_server:cast(From, {servico, Request, Reply}),
	{noreply, State}.
    
handle_call({get, Request}, _From, State) ->
	Reply= do_get(Request, State),
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

do_all(_Request, _State) ->
	msbus_user:call(all).
	
do_get(Request, _State) ->
	Id = msbus_request:get_param_url(<<"id">>, -1, Request),
	Result = msbus_user:call({get, Id}),
	Result.

do_insert(Request, _State) ->
	UserJson = msbus_request:get_property_request(<<"payload">>, Request),
	User = #user{nome  = maps:get(<<"nome">>, UserJson, ""),
				 email = maps:get(<<"email">>, UserJson, ""),
				 senha = maps:get(<<"senha">>, UserJson, "")},
	msbus_user:call({insert, User}).

do_update(Request, _State) ->
	Id = msbus_request:get_param_url(<<"id">>, -1, Request),
	UserJson = msbus_request:get_property_request(<<"payload">>, Request),
	case msbus_user:call({get, Id}) of
		{ok, User} -> 
			User2 = User#user{nome  = maps:get(<<"nome">>, UserJson, User#user.nome),
							  email = maps:get(<<"email">>, UserJson, User#user.email),
							  senha = maps:get(<<"senha">>, UserJson, User#user.senha)},
			msbus_user:call({update, User2});
		Error -> Error
	end.

do_delete(Request, _State) ->
	Id = msbus_request:get_param_url(<<"id">>, -1, Request),
	msbus_user:call({delete, Id}).
	

