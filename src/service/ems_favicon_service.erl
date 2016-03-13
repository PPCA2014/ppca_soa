%%********************************************************************
%% @title Módulo favicon
%% @version 1.0.0
%% @doc Módulo responsável pelo favicon do erlangMS.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(ems_favicon_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../../include/ems_config.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Cliente interno API
-export([execute/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
-record(state, {arquivo}). 


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
	ems_pool:cast(ems_favicon_service_pool, {favicon, Request, From}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
	case get_favicon_from_disk() of
		{ok, Arquivo} ->  State = #state{arquivo=Arquivo};
		{error, _Reason} -> State = #state{arquivo=null}
    end,
    {ok, State}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({favicon, Request, _From}, State) ->
	Reply = do_get_favicon(State),
	ems_eventmgr:notifica_evento(ok_request, {servico, Request, Reply}),
	%gen_server:cast(From, {servico, Request, Reply}), 
	{noreply, State}.
    
handle_call({favicon, _Request}, _From, State) ->
	Reply = do_get_favicon(State),
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

get_favicon_from_disk()->
	case file:read_file(?FAVICON_PATH) of
		{ok, Arquivo} -> {ok, Arquivo};
		{error, Reason} -> {error, Reason}
	end.
    
do_get_favicon(State) ->
	{ok, State#state.arquivo, <<"image/x-icon">>}.

