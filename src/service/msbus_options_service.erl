%%********************************************************************
%% @title Módulo msbus_option_service
%% @version 1.0.0
%% @doc Serviço que responde o verbo OPTION e retorna o header 
%%	    com os métodos suportados.
%% @end	    
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_options_service).

-behavior(gen_server). 

-include("../../include/msbus_config.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Cliente interno API
-export([execute/2]).

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
 
execute(Request, From)	->
	gen_server:cast(?SERVER, {option, Request, From}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
	%fprof:trace([start, {procs, [self()]}]),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({option, Request, _From}, State) ->
	msbus_eventmgr:notifica_evento(ok_request, {servico, Request, <<>>}),
	%gen_server:cast(From, {servico, Request, <<>>}), 
	{noreply, State}.
    
handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.    
