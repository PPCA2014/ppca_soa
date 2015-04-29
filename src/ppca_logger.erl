%% ---
%%  PPCA_SOA
%%  Publish and subscribe message queue
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---

-module(ppca_logger).
-behavior(gen_server).

-include("../include/ppca_config.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([error_msg/1, 
		 info_msg/1, 
		 warn_msg/1,
		 sync/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(lists, [map/2]).

-define(SERVER, ?MODULE).

-record(state, {checkpoint, rotacao_timeout, buffer = [], filename}).


%%====================================================================
%% Server API
%%====================================================================

start() ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    io:format("ppca_logger iniciado.~n", []),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Client API
%%====================================================================
 
error_msg(Msg) -> 
	gen_server:call(?SERVER, {write_msg, error, Msg}). 

warn_msg(Msg) -> 
	gen_server:call(?SERVER, {write_msg, warn, Msg}). 

info_msg(Msg) -> 
	gen_server:call(?SERVER, {write_msg, info, Msg}). 
	
sync() ->
	gen_server:call(?SERVER, sync_buffer). 		



%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	Filename = get_filename_logger(),
    RotacaoTimeout = get_rotacao_timeout_logger(),
    Checkpoint = get_checkpoint_timeout_logger(),
	NewState = #state{filename=Filename, 
					  checkpoint = Checkpoint, 
					  rotacao_timeout = RotacaoTimeout},
    erlang:send_after(RotacaoTimeout, self(), rotacao),
    {ok, NewState}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State}.
    
handle_call({write_msg, Tipo, Msg}, _From, State) ->
	NewState = write_msg(Tipo, Msg, State),
	{reply, ok, NewState};

handle_call(sync_buffer, _From, State) ->
	NewState = sync_buffer(State),
	{reply, ok, NewState}.

handle_info(sync, State) ->
   NewState = sync_buffer(State),
   {noreply, NewState};

handle_info(rotacao, State) ->
   NewState = sync_buffer(State),
   rotacao(State),
   {noreply, NewState}.
 
terminate(_Reason, _State) ->
    io:format("ppca_event_mq finalizado.~n"),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================
    
get_logger_conf() -> #logger{}.

get_filename_logger() -> 
	Conf = get_logger_conf(),
	Conf#logger.filename.

get_checkpoint_timeout_logger() ->
	Conf = get_logger_conf(),
	Conf#logger.checkpoint_timeout.

get_rotacao_timeout_logger() ->
	Conf = get_logger_conf(),
	Conf#logger.rotacao_timeout.
    
write_msg(error, Msg, State) ->
	Msg1 = lists:concat(["Erro  ", ppca_util:timestamp_str(), "  ", Msg]),
	erlang:send_after(State#state.checkpoint, self(), sync),
	State#state{buffer = [Msg1|State#state.buffer]};
	
write_msg(warn, Msg, State) ->
	Msg1 = lists:concat(["Warn  ", ppca_util:timestamp_str(), "  ", Msg]),
	erlang:send_after(State#state.checkpoint, self(), sync),
	State#state{buffer = [Msg1|State#state.buffer]};

write_msg(info, Msg, State) ->
	Msg1 = lists:concat(["Info  ", ppca_util:timestamp_str(), "  ", Msg]),
	erlang:send_after(State#state.checkpoint, self(), sync),
	State#state{buffer = [Msg1|State#state.buffer]}.

sync_buffer(State) ->
	file:write_file(State#state.filename, map(fun(L) -> L ++ "\n" end, lists:reverse(State#state.buffer)), [append]),
	State#state{buffer=[]}.

rotacao(_State) ->
	ok.
