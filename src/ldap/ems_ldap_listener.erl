%%********************************************************************
%% @title Module ems_ldap_listener
%% @version 1.0.0
%% @doc Listener module for ldap server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_ldap_listener).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Server API
-export([start/3, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {listener_name,
				service}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(IpAddress, Service, ListenerName) -> 
    gen_server:start_link(?MODULE, {IpAddress, Service, ListenerName}, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init({_IpAddress, Service = #service{tcp_port = Port, 
									 tcp_max_connections = MaxConnections}, ListenerName}) ->
	{ok, _} = ranch:start_listener(ListenerName, 100, ranch_tcp, [{port, Port}, 
																  {max_connections, MaxConnections}], 
								   ems_ldap_handler, [Service]),
	{ok, #state{listener_name = ListenerName, service = Service}}.
		
handle_cast(shutdown, State) ->
    {stop, normal, State}.
    
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
    
handle_info(timeout, State) ->  {noreply, State}.

handle_info(State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
 
code_change(_OldVsn, State, _Extra) -> {ok, State}.

