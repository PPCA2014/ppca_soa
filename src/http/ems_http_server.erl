%%********************************************************************
%% @title Module ems_http_server
%% @version 1.0.0
%% @doc Main module HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_server).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Server API
-export([start/1, stop/0]).

%% Client API
-export([start_listeners/2, stop_listener/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {listener=[]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Args) -> 
	io:format("start args is ~p\n", [Args]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================

start_listeners(Port, Listen_Address) ->
	gen_server:cast(?SERVER, {start_listeners, Port, Listen_Address}).
 
stop_listener(Port, IpAddress) ->
	gen_server:call(?SERVER, {stop_listener, Port, IpAddress}).
	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(Args) ->
 	io:format("server init is ~p\n", [Args]),
 	Config = ems_config:getConfig(),
	case start_listeners(Config#config.tcp_listen_address_t,
						 Config#config.tcp_port, 
						 #state{}) of
		{ok, State} ->
			{ok, State};
		{error, _Reason, State} -> 
			{stop, State}
	end.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({start_listeners, Port, Listen_Address}, State) ->
	{_, NewState} = start_listeners(Listen_Address, Port, State),
	{noreply, NewState}.

handle_call({stop_listener, Port, IpAddress}, _From, State) ->
	{Reply, NewState} = do_stop_listener(Port, IpAddress, State),
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
	case do_start_listener(Port, H, State) of
		{ok, NewState} -> start_listeners(T, Port, NewState);
		{{error, Reason}, NewState} -> {error, Reason, NewState}
	end.

do_start_listener(Port, IpAddress, State) ->
	case ems_http_listener:start(Port, IpAddress) of
		{ok, PidListener} ->
			NewState = State#state{listener=[{PidListener, Port, IpAddress}|State#state.listener]},
			{ok, NewState};
		{error, Reason} ->
			{{error, Reason}, State}
	end.

do_stop_listener(Port, IpAddress, State) ->
	case [ S || {S,P,I} <- State#state.listener, {P,I} == {Port, IpAddress}] of
		[PidListener|_] ->
			gen_server:call(PidListener, shutdown),
			NewState = State#state{listener=lists:delete({PidListener, Port, IpAddress}, State#state.listener)},
			ems_logger:info("Stopped listening at the address ~p:~p.", [inet:ntoa(IpAddress), Port]),
			{ok, NewState};
		_ -> 
			{{error, enolisten}, State}
	end.
