%% ---
%%  ppca_logger
%%  Logger do barramento
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Alunos: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%          Eliene Vieira            (elienev@gmail.com)
%%---

-module(ppca_logger).

-behavior(gen_server). 

-include("../include/ppca_config.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([error_msg/1, 
		 error_msg/2, 
		 info_msg/1, 
		 info_msg/2, 
		 warn_msg/1,
		 warn_msg/2,
		 sync/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(lists, [map/2]).

-define(SERVER, ?MODULE).

%  Armazena o estado do ppca_logger. 
-record(state, {% buffer para lista de mensagens. As mensagens vão primeiro para um buffer. 
				buffer = [],         
			    % em intervalores configuravel ocorre um checkpoint para descarregar o buffer no arquivo de log
			    checkpoint = false   
 			   }). 


%%====================================================================
%% Server API
%%====================================================================

start() -> % cria o processo e devolve o pid
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    %io:format("ppca_logger iniciado.~n", []),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Client API
%%====================================================================
 
error_msg(Msg) -> 
	gen_server:call(?SERVER, {write_msg, error, Msg}). 

error_msg(Msg, Params) -> 
	gen_server:call(?SERVER, {write_msg, error, Msg, Params}). 

warn_msg(Msg) -> 
	gen_server:call(?SERVER, {write_msg, warn, Msg}). 

warn_msg(Msg, Params) -> 
	gen_server:call(?SERVER, {write_msg, warn, Msg, Params}). 

info_msg(Msg) -> 
	gen_server:call(?SERVER, {write_msg, info, Msg}).

info_msg(Msg, Params) -> 
	gen_server:call(?SERVER, {write_msg, info, Msg, Params}). 

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

handle_call({write_msg, Tipo, Msg, Params}, _From, State) ->
	NewState = write_msg(Tipo, Msg, Params, State),
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
    %io:format("ppca_logger finalizado.~n"),
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
    
write_msg(Tipo, <<Msg/binary>>, State) ->
	Msg1 = binary_to_list(Msg),
    write_msg(Tipo, Msg1, State);
    
write_msg(Tipo, Msg, State) ->
	io:format("~s~n", [Msg]),
	Msg1 = lists:concat([atom_to_list(Tipo), " ", ppca_util:timestamp_str(), "  ", Msg]),
	set_checkpoint_timeout(State),
	State#state{buffer = [Msg1|State#state.buffer], checkpoint = true}.
	
write_msg(Tipo, Msg, Params, State) ->
	Msg1 = io_lib:format(Msg, Params),
	write_msg(Tipo, Msg1, State).
	
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

