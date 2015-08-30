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
-record(state, {lsocket, socket}).

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
			{ip, IpAddress}],
	case gen_tcp:listen(Port, Opts) of
      {ok, LSocket} ->
            start_server_worker(Conf#config.tcp_max_http_worker, LSocket),
            {ok, #state{lsocket = LSocket}, 0};
      {error, Reason} ->
           {error, Reason}
     end.	
    
handle_cast(shutdown, State) ->
    gen_tcp:close(State#state.lsocket),
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
    
handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

start_server_worker(0,_) ->
    ok;

start_server_worker(Num, LSocket) ->
    msbus_server_worker:start({Num, LSocket}),
    start_server_worker(Num-1, LSocket).
