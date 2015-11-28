%%********************************************************************
%% @title Módulo msbus_server_listener
%% @version 1.0.0
%% @doc Módulo listener para o servidor HTTP
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_server_listener).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include("../include/msbus_http_messages.hrl").

%% Server API
-export([start/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {lsocket = undefined}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Port, IpAddress) -> 
    gen_server:start_link(?MODULE, {Port, IpAddress}, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init({Port, IpAddress}) ->
	process_flag(trap_exit, true),
	Conf = msbus_config:getConfig(),
	Opts = [binary, 
			{packet, 0}, 
			{active, true},
			{send_timeout, ?TCP_SEND_TIMEOUT}, 
			{keepalive, Conf#config.tcp_keepalive}, 
			{nodelay, Conf#config.tcp_nodelay},
			{backlog, ?TCP_BACKLOG},
			{ip, IpAddress},
			{reuseaddr, true},
			{delay_send, false}],
	case gen_tcp:listen(Port, Opts) of
      {ok, LSocket} ->
			%io:format("Start workers for listener ~p com IP ~p\n", [self(), IpAddress]),    
            start_server_workers(Conf#config.tcp_max_http_worker, LSocket),
            %io:format("Finish start workers for listener ~p com IP ~p\n", [self(), IpAddress]),    
            {ok, #state{lsocket = LSocket}, 0};
      {error, Reason} ->
           {error, Reason}
     end.	

handle_cast(shutdown, State=#state{lsocket = undefined}) ->
	%io:format("shutdown listener undefined LSocket\n"),
    {stop, normal, State};
    
handle_cast(shutdown, State=#state{lsocket = LSocket}) ->
    %io:format("shutdown listener e close LSsocket\n"),
    gen_tcp:close(LSocket),
    {stop, normal, State#state{lsocket = undefined}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
    
handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, #state{lsocket = undefined}) ->
    %io:format("terminate listener undefined LSocket\n"),
    ok;
   
terminate(_Reason, #state{lsocket = LSocket}) ->
	%io:format("terminate listener e close LSocket\n"),    
    gen_tcp:close(LSocket),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

start_server_workers(0,_) ->
    ok;

start_server_workers(Num, LSocket) ->
    msbus_server_worker:start({Num, LSocket}),
    start_server_workers(Num-1, LSocket).
