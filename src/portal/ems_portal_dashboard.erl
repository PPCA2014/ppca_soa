%%********************************************************************
%% @title Module ems_portal_dashboard
%% @version 1.0.0
%% @doc Module dashboard
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_portal_dashboard).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../..//include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%% Server API
-export([start/1, start_link/1, stop/0]).

%% Cliente interno API
-export([open/2, sobre/2]).

-export([titulo/1, image_url/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do service. 
-record(state, {}). 

-record(?MODULE, {titulo, image_url}).


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
 
open(Request, From)	->
	ems_pool:cast(ems_portal_dashboard, {open, Request, From}).

sobre(Request, From)->
	ems_pool:cast(ems_portal_dashboard, {sobre, Request, From}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({sobre, Request, _From}, State) ->
	Result = do_sobre(Request),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Result}),
	{noreply, State};

handle_cast({open, Request, _From}, State) ->
	Result = do_open(Request),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Result}),
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
    
    
%%====================================================================
%% Funções internas
%%====================================================================
    
do_open(_Request) -> 
	[{nome_sistema, "Portal ErlangMS"},
	 {breadcrumb, [<<"Portal ErlangMS">>, <<"Aplicativos">>]},
	 {webapps, [{?MODULE, <<"Simar">>, "img/pedidos.png"}, 
				{?MODULE, <<"Relatórios"/utf8>>, "img/relatorios.png"}]}
	 
	 ].

do_sobre(_Request) -> 
	[{nome_sistema, "Portal ErlangMS"}].

	
titulo(#?MODULE{titulo = Titulo}) -> 
	io:format("aqui\n"),
	Titulo.	
image_url(#?MODULE{image_url = Url}) -> 
	io:format("aqui2\n"),
	Url.	
	
	

