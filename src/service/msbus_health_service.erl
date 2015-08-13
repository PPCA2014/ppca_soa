%%********************************************************************
%% @title Módulo msbus_health_service
%% @version 1.0.0
%% @doc Disponibiliza informações da saúde do servidor
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_health_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API  
-export([start/0, start_link/1, stop/0]).

%% Cliente interno API
-export([top_services/2, top_services_by_type/2, qtd_requests_by_date/2]).

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
 
top_services(Request, From)	->
	poolboy:transaction(msbus_health_service_pool, fun(Worker) ->
		gen_server:cast(Worker, {top_services, Request, From})
    end).

top_services_by_type(Request, From)	->
	poolboy:transaction(msbus_health_service_pool, fun(Worker) ->
		gen_server:cast(Worker, {top_services_by_type, Request, From})
    end).

qtd_requests_by_date(Request, From)	->
	poolboy:transaction(msbus_health_service_pool, fun(Worker) ->
		gen_server:cast(Worker, {qtd_requests_by_date, Request, From})
    end).

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({top_services, Request, From}, State) ->
	Reply = get_top_services(Request, State),
	From ! {ok, Reply}, 
	{noreply, State};

handle_cast({top_services_by_type, Request, From}, State) ->
	Reply = get_top_services_by_type(Request, State),
	From ! {ok, Reply}, 
	{noreply, State};

handle_cast({qtd_requests_by_date, Request, From}, State) ->
	Reply = get_qtd_requests_by_date(Request, State),
	From ! {ok, Reply}, 
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
	Top = list_to_integer(msbus_request:get_param_url(<<"top">>, "10", Request)),
	Periodo = msbus_request:get_querystring(<<"periodo">>, "year", Request),
	Sort = msbus_request:get_querystring(<<"sort">>, "qtd", Request),
	msbus_health:get_top_services(Top, Periodo, Sort).
	
get_top_services_by_type(Request, _State) ->
	Top = list_to_integer(msbus_request:get_param_url(<<"top">>, "10", Request)),
	Periodo = msbus_request:get_querystring(<<"periodo">>, "year", Request),
	Sort = msbus_request:get_querystring(<<"sort">>, "qtd", Request),
	msbus_health:get_top_services_by_type(Top, Periodo, Sort).

get_qtd_requests_by_date(Request, _State) ->
	Top = list_to_integer(msbus_request:get_param_url(<<"top">>, "10", Request)),
	Periodo = msbus_request:get_querystring(<<"periodo">>, "month", Request),
	Sort = msbus_request:get_querystring(<<"sort">>, "qtd", Request),
	msbus_health:get_qtd_requests_by_date(Top, Periodo, Sort).

