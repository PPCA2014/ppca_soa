%%********************************************************************
%% @title Módulo msbus_server_listener
%% @version 1.0.0
%% @doc Módulo principal do servidor HTTP
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_server_listener).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include("../include/msbus_http_messages.hrl").

%% Server API
-export([start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {lsocket, socket}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Port) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([Port]) ->
	Opts = [binary, 
			{packet, 0}, 
			{active, true},
			{send_timeout, ?TCP_SEND_TIMEOUT}, 
			{keepalive, ?TCP_KEEPALIVE}, 
			{nodelay, ?TCP_NODELAY}],
	case gen_tcp:listen(Port, Opts) of
      {ok, LSocket} ->
            start_servers(?TCP_MAX_HTTP_WORKER, LSocket),
            {ok, Port} = inet:port(LSocket),
            {ok, #state{lsocket = LSocket}, 0};
      {error, Reason} ->
           {error, Reason}
     end.	
    
handle_cast(shutdown, State) ->
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

start_servers(0,_) ->
    ok;

start_servers(Num, LSocket) ->
    msbus_server_worker:start(LSocket),
    start_servers(Num-1, LSocket).
