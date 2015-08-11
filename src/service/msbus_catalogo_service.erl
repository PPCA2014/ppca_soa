%%********************************************************************
%% @title Módulo msbus_catalogo_service
%% @version 1.0.0
%% @doc Módulo de serviço msbus_catalogo_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_catalogo_service).

-behavior(gen_server). 

%% Server API  
-export([start/0, stop/0]).

%% Cliente interno API
-export([lista_catalogo/2]).

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
 
lista_catalogo(Request, From)	->
	gen_server:cast(?SERVER, {lista_catalogo, Request, From}).
	

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({lista_catalogo, Request, From}, State) ->
	lista_catalogo(Request, From, State),
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

lista_catalogo(_Request, From, _State) -> 
	Result = msbus_catalogo:lista_catalogo(),
	From ! {ok, Result}.
	

