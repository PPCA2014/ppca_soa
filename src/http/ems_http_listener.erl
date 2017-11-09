%%********************************************************************
%% @title Module ems_http_listener
%% @version 1.0.0
%% @doc Listener module for HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_listener).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Server API
-export([start/3, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {http_max_content_length,
				http_header_default}).


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
 
init({IpAddress, 
	  _Service = #service{protocol = Protocol,
						  tcp_port = Port,
						  tcp_is_ssl = IsSsl,
						  tcp_max_connections = MaxConnections,
						  tcp_ssl_cacertfile = SslCaCertFile,
						  tcp_ssl_certfile = SslCertFile,
						  tcp_ssl_keyfile = SslKeyFile,
						  http_max_content_length = HttpMaxContentLength},
	  ListenerName}) ->
    State = #state{http_max_content_length = HttpMaxContentLength,
				   http_header_default = get_http_header_default()},
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/websocket", ems_websocket_handler, State},
			{'_', ems_http_handler, State}
		]}
	  ]),
	ProtocolStr = binary_to_list(Protocol),
	IpAddressStr = inet_parse:ntoa(IpAddress),
	case IsSsl of
		true -> 
			Ret = cowboy:start_tls(ListenerName, [  {ip, IpAddress},
													{port, Port},
													{max_connections, MaxConnections},
													{cacertfile, binary_to_list(SslCaCertFile)},
													{certfile, binary_to_list(SslCertFile)},
													{keyfile, binary_to_list(SslKeyFile)}
												  ], #{compress => true, 
													   env => #{dispatch => Dispatch}});
		false ->
			Ret = cowboy:start_clear(ListenerName, [{ip, IpAddress}, 
													{port, Port}, 
													{max_connections, MaxConnections}], 
										#{compress => true,
										  env => #{dispatch => Dispatch}
									})
	end,
	case Ret of
		{ok, _PidCowboy} -> 
			ems_logger:info("ems_http_listener listener ~s in ~s:~p.", [ProtocolStr, IpAddressStr, Port]);
		{error, eaddrinuse} -> 
			ems_logger:error("ems_http_listener can not listen ~s on port ~p because it is already in use on IP ~s by other process.", [ProtocolStr, Port, IpAddressStr])
	end,
	{ok, State}.
	
	
		
		
handle_cast(shutdown, State) ->
    {stop, normal, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
    
handle_info(timeout, State) ->  {noreply, State}.

handle_info(State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
 
code_change(_OldVsn, State, _Extra) -> {ok, State}.

get_http_header_default() ->
	#{
		<<"server">> => ?SERVER_NAME,
		<<"content-type">> => ?CONTENT_TYPE_JSON,
		<<"ems-node">> => ems_util:node_binary(),
		<<"cache-control">> => ?CACHE_CONTROL_NO_CACHE,
		<<"access-control-allow-origin">> => ?ACCESS_CONTROL_ALLOW_ORIGIN,
		<<"access-control-max-age">> => ?ACCESS_CONTROL_MAX_AGE,
		<<"access-control-allow-headers">> => ?ACCESS_CONTROL_ALLOW_HEADERS,
		<<"access-control-allow-methods">> => ?ACCESS_CONTROL_ALLOW_METHODS,
		<<"access-control-expose-headers">> => ?ACCESS_CONTROL_EXPOSE_HEADERS
	}.
