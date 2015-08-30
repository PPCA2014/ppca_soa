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
 
start_listen(Port, IpAddress) ->
	gen_server:call(?SERVER, {start_listen, Port, IpAddress}).

stop_listen(Port, IpAddress) ->
	gen_server:call(?SERVER, {stop_listen, Port, IpAddress}).
	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) -> 
	Conf = msbus_config:getConfig(),
	start_listeners(Conf#config.tcp_listen_address, Conf#config.tcp_port, #state{}).
    
handle_cast(shutdown, State) ->
    {stop, normal, State}.

handle_call({start_listen, Port, IpAddress}, _From, State) ->
	{Reply, NewState} = do_start_listen(Port, IpAddress, State),
	{reply, Reply, NewState};

handle_call({stop_listen, Port, IpAddress}, _From, State) ->
	{Reply, NewState} = do_stop_listen(Port, IpAddress, State),
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

start_listeners([], _Port, State) -> {ok, State};

start_listeners([H|T], Port, State) ->
	case do_start_listen(Port, H, State) of
		{ok, NewState} -> start_listeners(T, Port, NewState);
		{{error, _Reason}, NewState} -> {error, NewState}
	end.

do_start_listen(Port, IpAddress, State) ->
	case msbus_server_listener:start(Port, IpAddress) of
		{ok, PidListener} ->
			NewState = State#state{listener=[{PidListener, Port, IpAddress}|State#state.listener]},
			msbus_logger:info("Escutando no endereço ~s:~p.", [inet:ntoa(IpAddress), Port]),
			Reply = {ok, NewState};
		{error, Reason} ->
			Reply = {{error, Reason}, State}
	end,
	Reply.

do_stop_listen(Port, IpAddress, State) ->
	case [ S || {S,P,I} <- State#state.listener, {P,I} == {Port, IpAddress}] of
		[PidListener|_] ->
			gen_server:call(PidListener, shutdown),
			NewState = State#state{listener=lists:delete({PidListener, Port, IpAddress}, State#state.listener)},
			msbus_logger:info("Parou de escutar no endereço ~p:~p.", [inet:ntoa(IpAddress), Port]),
			Reply = {ok, NewState};
		_ -> 
			Reply = {{error, enolisten}, State}
	end,
	Reply.
