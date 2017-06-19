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
 
init({IpAddress, Service = #service{protocol = Protocol,
									 tcp_port = Port, 
									 tcp_max_connections = MaxConnections}, ListenerName}) ->
	ProtocolStr = binary_to_list(Protocol),
	IpAddressStr = inet_parse:ntoa(IpAddress),
	Ret = ranch:start_listener(ListenerName, 100, ranch_tcp, [{ip, IpAddress},
															  {port, Port}, 
															  {max_connections, MaxConnections}], 
							   ems_ldap_handler, [Service]),
	case Ret of
		{ok, _PidCowboy} -> 
			ems_logger:info("ems_ldap_listener listener ~s in port ~p on IP ~s.", [ProtocolStr, Port, IpAddressStr]);
		{error,eaddrinuse} -> 
			ems_logger:error("ems_ldap_listener can not listen ~s on port ~p because it is already in use on IP ~s by other process.", [ProtocolStr, Port, IpAddressStr])
	end,
	{ok, #state{listener_name = ListenerName, service = Service}}.
		
handle_cast(shutdown, State) ->
    {stop, normal, State}.
    
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
    
handle_info(timeout, State) ->  {noreply, State}.

handle_info(State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
 
code_change(_OldVsn, State, _Extra) -> {ok, State}.

