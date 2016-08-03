%%********************************************************************
%% @title Módulo ems_catalog_schema_service
%% @version 1.0.0
%% @doc Módulo de serviço ems_catalog_schema_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_schema_service).

-behavior(gen_server). 

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").


%% Server API  
-export([start/0, start_link/1, stop/0]).

%% Cliente interno API
-export([all/2, find_by_id/2, insert/2, update/2, delete/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

%  Armazena o estado do service. 
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
 
find_by_id(Request, From)	->
	gen_server:cast(?SERVER, {find_by_id, Request, From}).
	
insert(Request, From)	->
	gen_server:cast(?SERVER, {insert, Request, From}).

update(Request, From)	->
	gen_server:cast(?SERVER, {update, Request, From}).

all(Request, From)	->
	gen_server:cast(?SERVER, {all, Request, From}).

delete(Request, From)	->
	gen_server:cast(?SERVER, {delete, Request, From}).
	

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({find_by_id, Request, _From}, State) ->
	Result = do_find_by_id(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Result}),
	{noreply, State};

handle_cast({insert, Request, _From}, State) ->
	Reply = do_insert(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({update, Request, _From}, State) ->
	Reply = do_update(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({all, Request, _From}, State) ->
	Result = do_all(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Result}),
	{noreply, State};

handle_cast({delete, Request, _From}, State) ->
	Reply = do_delete(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	{noreply, State}.

handle_call(_Param, _From, State) ->
	{reply, ok, State}.

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

do_find_by_id(Request, _State) -> 
	Id = ems_request:get_param_url(<<"id">>, -1, Request),
	ems_catalog_schema:find_by_id(Id).
	
do_insert(#request{payload_map = CatalogSchemaMap}, _State) ->
	ems_catalog_schema:insert(CatalogSchemaMap).

do_update(Request = #request{payload_map = CatalogSchemaMap}, _State) ->
	Id = ems_request:get_param_url(<<"id">>, -1, Request),
	ems_catalog_schema:update(Id, CatalogSchemaMap).

do_all(_Request, _State) -> 
	ems_catalog_schema:all().
	
do_delete(Request, _State) -> 
	Id = ems_request:get_param_url(<<"id">>, -1, Request),
	ems_catalog_schema:delete(Id).
	

