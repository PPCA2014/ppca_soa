%%********************************************************************
%% @title Módulo msbus_health_service
%% @version 1.0.0
%% @doc Disponibiliza informações da saúde do servidor
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_health_service).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API  
-export([start/0, stop/0]).

%% Cliente interno API
-export([top_services/2]).

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
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
top_services(Request, From)	->
	gen_server:cast(?SERVER, {top_services, Request, From}).
	


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({top_services, Request, From}, State) ->
	Reply = get_top_services(Request, State),
	From ! {ok, Reply}, 
	{noreply, State}.

handle_call(_Params, _From, State) ->
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

get_top_services(Request, _State) ->
	Top = list_to_integer(msbus_request:get_param_url(<<"top">>, "10", Request)),
	Periodo = msbus_request:get_querystring(<<"periodo">>, "year", Request),
	msbus_health:get_top_services(Top, Periodo).
	


