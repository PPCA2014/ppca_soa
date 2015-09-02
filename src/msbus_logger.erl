%%********************************************************************
%% @title Módulo msbus_logger
%% @version 1.0.0
%% @doc Módulo responsável pelo componente de logger.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(msbus_logger).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Client API
-export([error/1, error/2, info/1, info/2, warn/1, warn/2, sync/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do msbus_logger. 
-record(state, {% As mensagens vão primeiro para um buffer, posteriormente para o arquivo de log
				buffer = [],         
			    % checkpoint para descarregar o buffer no arquivo de log
			    checkpoint = false   
 			   }). 


%%====================================================================
%% Server API
%%====================================================================

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Client API
%%====================================================================
 
error(Msg) -> 
	gen_server:cast(?SERVER, {write_msg, error, Msg}). 

error(Msg, Params) -> 
	gen_server:cast(?SERVER, {write_msg, error, Msg, Params}). 

warn(Msg) -> 
	gen_server:cast(?SERVER, {write_msg, warn, Msg}). 

warn(Msg, Params) -> 
	gen_server:cast(?SERVER, {write_msg, warn, Msg, Params}). 

info(Msg) -> 
	gen_server:cast(?SERVER, {write_msg, info, Msg}).

info(Msg, Params) -> 
	gen_server:cast(?SERVER, {write_msg, info, Msg, Params}). 

sync() ->
	gen_server:cast(?SERVER, sync_buffer). 		



%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};
    
handle_cast({write_msg, Tipo, Msg}, State) ->
	NewState = write_msg(Tipo, Msg, State),
	{noreply, NewState};

handle_cast({write_msg, Tipo, Msg, Params}, State) ->
	NewState = write_msg(Tipo, Msg, Params, State),
	{noreply, NewState};

handle_cast(sync_buffer, State) ->
	NewState = sync_buffer(State),
	{noreply, NewState}.

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
   {noreply, NewState}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================
    
get_filename_logger() -> 
	Conf = msbus_config:getConfig(),
	{{Ano,Mes,Dia},{_Hora,_Min,_Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("~s/server_~p_~p_~p.log", [Conf#config.log_file_dest, Ano, Mes, Dia])).

get_checkpoint_timeout_logger() ->
	Conf = msbus_config:getConfig(),
	Conf#config.log_file_checkpoint.

set_checkpoint_timeout(#state{checkpoint = false}) ->    
    Checkpoint = get_checkpoint_timeout_logger(),
	erlang:send_after(Checkpoint, self(), checkpoint);

set_checkpoint_timeout(_State) ->    
	ok.

write_msg(Tipo, <<Msg/binary>>, State) ->
	Msg1 = binary_to_list(Msg),
    write_msg(Tipo, Msg1, State);
    
write_msg(Tipo, Msg, State) ->
	case erlang:is_list(Msg) of
		true -> io:format("~s~n", [Msg]);
		false -> io:format("~p~n", [Msg])
	end,
	Msg1 = lists:concat([string:to_upper(atom_to_list(Tipo)), " ", msbus_util:timestamp_str(), "  ", Msg]),
	set_checkpoint_timeout(State),
	State#state{buffer = [Msg1|State#state.buffer], checkpoint = true}.
	
write_msg(Tipo, Msg, Params, State) ->
	Msg1 = io_lib:format(Msg, Params),
	write_msg(Tipo, Msg1, State).
	
sync_buffer(State) ->
	FileName = get_filename_logger(),
	file:write_file(FileName, lists:map(fun(L) -> L ++ "\n" end, lists:reverse(State#state.buffer)), [append]),
	#state{}.
	
