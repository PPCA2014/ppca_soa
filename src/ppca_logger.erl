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

-record(state, {buffer = [], checkpoint = false}).


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
    {ok, #state{}}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State}.
    
handle_call({write_msg, Tipo, Msg}, _From, State) ->
	NewState = write_msg(Tipo, Msg, State),
	{reply, ok, NewState};

handle_call(sync_buffer, _From, State) ->
	NewState = sync_buffer(State),
	{reply, ok, NewState}.

handle_info(checkpoint, State) ->
   NewState = sync_buffer(State),
   {noreply, NewState};

handle_info(rotacao, State) ->
   NewState = rotacao(State),
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

get_new_filename_logger() ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("server_~p~p~p_~p~p~p.log", [Ano, Mes, Dia, Hora, Min, Seg])).
    
set_checkpoint_timeout(#state{checkpoint = false}) ->    
    Checkpoint = get_checkpoint_timeout_logger(),
	erlang:send_after(Checkpoint, self(), checkpoint);

set_checkpoint_timeout(_State) ->    
	ok.

set_rotacao_timeout() ->    
    Rotacao = get_rotacao_timeout_logger(),
	erlang:send_after(Rotacao, self(), rotacao).
    
write_msg(error, Msg, State) ->
	Msg1 = lists:concat(["Erro  ", ppca_util:timestamp_str(), "  ", Msg]),
	set_checkpoint_timeout(State),
	State#state{buffer = [Msg1|State#state.buffer], checkpoint = true};
	
write_msg(warn, Msg, State) ->
	Msg1 = lists:concat(["Warn  ", ppca_util:timestamp_str(), "  ", Msg]),
	set_checkpoint_timeout(State),
	State#state{buffer = [Msg1|State#state.buffer], checkpoint = true};

write_msg(info, Msg, State) ->
	Msg1 = lists:concat(["Info  ", ppca_util:timestamp_str(), "  ", Msg]),
	set_checkpoint_timeout(State),
	State#state{buffer = [Msg1|State#state.buffer], checkpoint = true}.

sync_buffer(State) ->
	FileName = get_filename_logger(),
	file:write_file(FileName, map(fun(L) -> L ++ "\n" end, lists:reverse(State#state.buffer)), [append]),
	#state{}.

rotacao(State) ->
	NewState = sync_buffer(State),	
	FileName = get_filename_logger(),
	NewFileName = get_new_filename_logger(),
	file:rename(FileName, NewFileName),
	set_rotacao_timeout(),
	NewState.

