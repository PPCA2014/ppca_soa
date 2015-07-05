%%********************************************************************
%% @title Módulo auth_user
%% @version 1.0.0
%% @doc Módulo responsável pela autenticação dos usuários.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_auth_user).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").

%% Server API
-export([start/0, stop/0]).

%% Cliente interno API
-export([autentica/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
-record(state, {}). 


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    msbus_logger:info("msbus_auth_user iniciado."),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
autentica(Request, From) ->
	gen_server:cast(?SERVER, {autentica, Request, From}).
	


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({autentica, Request, From}, State) ->
	{Response, NewState} = do_autentica(Request, State),
	From ! {ok, Response}, 
	{noreply, NewState}.
    
handle_call({autentica, Request}, _From, State) ->
	{Response, NewState} = do_autentica(Request, State),
	{reply, Response, NewState}.

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
    
do_autentica(_Request, State) ->
	Response = "{\"key\": \"123456789\"}",
	NewState = State#state{},
	{Response, NewState}.


