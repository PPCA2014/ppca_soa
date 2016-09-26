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

% estado do servidor
-record(state, {lsocket = undefined, 
				listener_name,
				tcp_config}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(IpAddress, TcpConfig, ListenerName) -> 
    gen_server:start_link(?MODULE, {IpAddress, TcpConfig, ListenerName}, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init({IpAddress, 
	  TcpConfig = #tcp_config{tcp_keepalive = KeepAlive, 
							  tcp_nodelay = NoDelay, 
							  tcp_min_http_worker = MinHttpWorker,
							  tcp_port = Port,
							  tcp_send_timeout = SendTimeout,
							  tcp_backlog = Backlog,
							  tcp_delay_send = DelaySend,
							  tcp_buffer = Buffer},
	  ListenerName}) ->
	Opts = [binary, 
			{packet, 0}, 
			{active, true},
			{buffer, Buffer},
			{send_timeout_close, true},
			{send_timeout, SendTimeout}, 
			{keepalive, KeepAlive}, 
			{nodelay, NoDelay},
			{backlog, Backlog},
			{ip, IpAddress},
			{reuseaddr, true},
			{delay_send, DelaySend}],
	case gen_tcp:listen(Port, Opts) of
		{ok, LSocket} ->
			NewState = #state{lsocket = LSocket, 
							  listener_name = ListenerName,
							  tcp_config = TcpConfig},
			ems_db:init_sequence(ListenerName, 0),
			start_server_workers(MinHttpWorker, LSocket, TcpConfig, ListenerName),
			ems_logger:info("Listening http packets on ~s:~p.", [inet:ntoa(IpAddress), Port]),
			{ok, NewState};
		{error,eaddrnotavail} ->
			ems_logger:error("Network interface to the IP ~p not available, ignoring this interface...", [inet:ntoa(IpAddress)]),
			{ok, #state{}};    
		Error -> Error
     end.	

handle_cast(new_worker, State = #state{lsocket = LSocket,
									   listener_name = ListenerName,
									   tcp_config = TcpConfig = #tcp_config{tcp_min_http_worker = _MinHttpWorker}}) ->
	%io:format("Iniciando worker extra\n"),
	%start_server_workers(MinHttpWorker, LSocket, TcpConfig, ListenerName),
	ems_http_worker:start_link({self(), LSocket, TcpConfig, ListenerName}),
	flush(),
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


flush() ->
    receive
        _ -> flush()
    after 0 ->
        ok
    end.
    
start_server_workers(0,_,_,_) ->
    ok;
start_server_workers(Num, LSocket, TcpConfig, ListenerName) ->
    ems_http_worker:start_link({self(), LSocket, TcpConfig, ListenerName}),
    start_server_workers(Num-1, LSocket, TcpConfig, ListenerName).
