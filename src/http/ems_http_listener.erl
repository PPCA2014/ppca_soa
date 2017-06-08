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
 
init({_IpAddress, 
	  _Service = #service{tcp_port = Port,
						  tcp_is_ssl = IsSsl,
						  tcp_max_connections = MaxConnections,
						  tcp_ssl_cacertfile = SslCaCertFile,
						  tcp_ssl_certfile = SslCertFile,
						  tcp_ssl_keyfile = SslKeyFile},
	  ListenerName}) ->
	  Dispatch = cowboy_router:compile([
		{'_', [
			{"/websocket", ems_websocket_handler, []},
			{'_', ems_http_handler, []}
		]}
	]),
	
	case IsSsl of
		true -> 
			{ok, _} = cowboy:start_tls(ListenerName, [
				{port, Port},
				{max_connections, MaxConnections},
				{cacertfile, SslCaCertFile},
				{certfile, SslCertFile},
				{keyfile, SslKeyFile}
			], #{compress => true,
				 env => #{dispatch => Dispatch}});
		false ->
			{ok, _} = cowboy:start_clear(ListenerName, [{port, Port}, {max_connections, MaxConnections}], 
				#{compress => true,
				  env => #{dispatch => Dispatch}
			})
	end.
	
		
		
handle_cast(shutdown, State) ->
    {stop, normal, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
    
handle_info(timeout, State) ->  {noreply, State}.

handle_info(State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
 
code_change(_OldVsn, State, _Extra) -> {ok, State}.

