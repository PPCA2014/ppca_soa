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
-export([start/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {lsocket = undefined, 
				allowed_address = undefined}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(IpAddress, TcpConfig) -> 
    gen_server:start_link(?MODULE, {IpAddress, TcpConfig}, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init({IpAddress, #tcp_config{tcp_keepalive = KeepAlive, 
							 tcp_nodelay = NoDelay, 
							 tcp_allowed_address_t = AllowedAddress,
							 tcp_max_http_worker = MaxHttpWorker,
							 tcp_port = Port}}) ->
	Opts = [binary, 
			{packet, 0}, 
			{active, true},
			{buffer, 8000},
			{send_timeout_close, true},
			{send_timeout, ?TCP_SEND_TIMEOUT}, 
			{keepalive, KeepAlive}, 
			{nodelay, NoDelay},
			{backlog, ?TCP_BACKLOG},
			{ip, IpAddress},
			{reuseaddr, true},
			{delay_send, false}],
	case gen_tcp:listen(Port, Opts) of
		{ok, LSocket} ->
			NewState = #state{lsocket = LSocket, allowed_address = AllowedAddress},
			start_server_workers(MaxHttpWorker, LSocket, AllowedAddress),
			ems_logger:info("Listening http packets on ~s:~p.", [inet:ntoa(IpAddress), Port]),
			{ok, NewState};
		{error,eaddrnotavail} ->
			ems_logger:error("Network interface to the IP ~p not available, ignoring this interface...", [inet:ntoa(IpAddress)]),
			{ok, #state{}};    
		Error -> Error
     end.	

handle_cast(new_worker, State = #state{lsocket = LSocket,
									   allowed_address = Allowed_Address}) ->
    ems_http_worker:start_link({self(), LSocket, Allowed_Address}),
    {noreply, State};

handle_cast(shutdown, State=#state{lsocket = undefined}) ->
    {stop, normal, State};
    
handle_cast(shutdown, State=#state{lsocket = LSocket}) ->
    gen_tcp:close(LSocket),
    {stop, normal, State#state{lsocket = undefined}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
    
handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, #state{lsocket = undefined}) ->
    ok;
   
terminate(_Reason, #state{lsocket = LSocket}) ->
    gen_tcp:close(LSocket),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

start_server_workers(0,_,_) ->
    ok;

start_server_workers(Num, LSocket, Allowed_Address) ->
    ems_http_worker:start_link({self(), LSocket, Allowed_Address}),
    start_server_workers(Num-1, LSocket, Allowed_Address).
