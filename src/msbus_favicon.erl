%%********************************************************************
%% @title Módulo favicon
%% @version 1.0.0
%% @doc Módulo responsável pelo favicon do erlangMS.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_favicon).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").

%% Server API
-export([start/0, stop/0]).

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
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    msbus_logger:info("msbus_favicon iniciado."),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
execute(Request, From)	->
	gen_server:cast(?SERVER, {favicon, Request, From}).
	


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	case get_favicon_from_disk() of
		{ok, Arquivo} ->  State = #state{arquivo=Arquivo};
		{error, _Reason} -> State = #state{arquivo=null}
    end,
    {ok, State}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({favicon, _Request, From}, State) ->
	Reply = do_get_favicon(State),
	From ! {ok, Reply}, 
	{noreply, State}.
    
handle_call({favicon, _Request}, _From, State) ->
	Reply = do_get_favicon(State),
	{reply, Reply, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    msbus_logger:info("msbus_favicon finalizado."),
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

