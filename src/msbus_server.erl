%%********************************************************************
%% @title Servidor HTTP
%% @version 1.0.0
%% @doc Módulo principal do servidor HTTP
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_server).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include("../include/msbus_http_messages.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([start_listen/2, stop_listen/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {listener=[]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================
 
start_listen(Port, From) ->
	gen_server:call(?SERVER, {start_listen, Port, From}).

stop_listen(Port, From) ->
	gen_server:call(?SERVER, {stop_listen, Port, From}).
	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
	case do_start_listen(?CONF_PORT, #state{}) of
		{ok, NewState} -> {ok, NewState};
		{{error, _Reason}, NewState} -> {error, NewState}
	end.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({start_listen, Port, From}, State) ->
	{Reply, NewState} = do_start_listen(Port, State),
	From ! Reply, 
	{noreply, NewState};
    
handle_cast({stop_listen, Port, From}, State) ->
	{Reply, NewState} = do_stop_listen(Port, State),
	From ! Reply, 
	{noreply, NewState}.

handle_call({start_listen, Port}, _From, State) ->
	{Reply, NewState} = do_start_listen(Port, State),
	{reply, Reply, NewState}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

do_start_listen(Port, State) ->
	case msbus_server_listener:start(Port) of
		{ok, Listen} ->
			NewState = State#state{listener=[{Listen, Port}|State#state.listener]},
			msbus_logger:info("Escutando na porta ~p.", [Port]),
			Reply = {ok, NewState};
		{error, Reason} ->
			Reply = {{error, Reason}, State}
	end,
	Reply.

do_stop_listen(Port, State) ->
	case [ S || {S,P} <- State#state.listener, P == Port] of
		[Listen|_] ->
			msbus_server_listener:stop(),
			NewState = State#state{listener=lists:delete({Listen, Port}, State#state.listener)},
			msbus_logger:info("Parou de escutar na porta ~p.", [Port]),
			Reply = {ok, NewState};
		_ -> 
			Reply = {{error, enolisten}, State}
	end,
	Reply.
