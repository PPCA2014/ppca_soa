%%********************************************************************
%% @title Módulo clusterware
%% @version 1.0.0
%% @doc Módulo responsável pelo gerenciamendo dos nós do barramento.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_clusterware).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_http_messages.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([start_esb/0, stop_esb/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-import(string, [tokens/2]).
-import(lists, [reverse/1, map/2, filter/2]).

% estado do servidor
-record(state, {listener=[]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================
 
start_esb() ->
	gen_server:cast(?SERVER, start_esb).

stop_esb() ->
	gen_server:cast(?SERVER, stop_esb).
	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(start_esb, State) ->
	do_start_esb(),
	{noreply, State};
    
handle_cast(stop_esb, State) ->
	do_stop_esb(),
	{noreply, State}.

handle_call(start_esb, _From, State) ->
	do_start_esb(),
	{reply, ok, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    msbus_logger:info("esb finalizado."),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

do_start_esb() ->
	start_no(no_001@puebla, 2301),
	start_no(no_002@puebla, 2302),
	msbus_logger:info("ok\n").
	
do_stop_esb() ->
	msbus_logger:info("ok\n").

start_no(Nome, Port) ->
	rpc:call(Nome, msbus_soa, start, [Port]).
	

