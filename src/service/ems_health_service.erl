%%********************************************************************
%% @title Módulo ems_health_service
%% @version 1.0.0
%% @doc Disponibiliza informações da saúde do servidor
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_health_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API  
-export([start/0, start_link/1, stop/0]).

%% Cliente interno API
-export([top_services/2, top_services_by_type/2, qtd_requests_by_date/2]).

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
 
top_services(Request, From)	->
	ems_pool:cast(ems_health_service_pool, {top_services, Request, From}).

top_services_by_type(Request, From)	->
	ems_pool:cast(ems_health_service_pool, {top_services_by_type, Request, From}).

qtd_requests_by_date(Request, From)	->
	ems_pool:cast(ems_health_service_pool, {qtd_requests_by_date, Request, From}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({top_services, Request, _From}, State) ->
	Reply = get_top_services(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	%gen_server:cast(From, {service, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({top_services_by_type, Request, _From}, State) ->
	Reply = get_top_services_by_type(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	%gen_server:cast(From, {service, Request, {ok, Reply}}),
	{noreply, State};

handle_cast({qtd_requests_by_date, Request, _From}, State) ->
	Reply = get_qtd_requests_by_date(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	%gen_server:cast(From, {service, Request, {ok, Reply}}),
	{noreply, State}.

handle_call(_Params, _From, State) ->
	{reply, ok, State}.

handle_info(State) ->
   {noreply, State}.

handle_info({'EXIT', _SomePid,_}, State)->
   io:format("Long runnnig task2\n"),
   {noreply, State};

handle_info(_Msg, State) ->
   io:format("timeout2\n"),
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================

get_top_services(Request, _State) ->
	Top = list_to_integer(ems_request:get_param_url(<<"id">>, "10", Request)),
	Periodo = ems_request:get_querystring(<<"periodo">>, "year", Request),
	Sort = ems_request:get_querystring(<<"sort">>, "qtd", Request),
	ems_health:get_top_services(Top, Periodo, Sort).
	
get_top_services_by_type(Request, _State) ->
	Top = list_to_integer(ems_request:get_param_url(<<"id">>, "10", Request)),
	Periodo = ems_request:get_querystring(<<"periodo">>, "year", Request),
	Sort = ems_request:get_querystring(<<"sort">>, "qtd", Request),
	ems_health:get_top_services_by_type(Top, Periodo, Sort).

get_qtd_requests_by_date(Request, _State) ->
	Top = list_to_integer(ems_request:get_param_url(<<"id">>, "10", Request)),
	Periodo = ems_request:get_querystring(<<"periodo">>, "month", Request),
	Sort = ems_request:get_querystring(<<"sort">>, "qtd", Request),
	ems_health:get_qtd_requests_by_date(Top, Periodo, Sort).

