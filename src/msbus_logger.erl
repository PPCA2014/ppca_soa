%%********************************************************************
%% @title Module msbus_logger
%% @version 1.0.0
%% @doc Module responsible for the logger component.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(msbus_logger).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Client API
-export([error/1, error/2, 
		 info/1, info/2, 
		 warn/1, warn/2, 
		 debug/1, debug/2,
		 sync/0, 
		 log_request/1,
		 modo_debug/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do msbus_logger. 
-record(state, {buffer = [],             		% The messages go first to a buffer subsequently to the log file        
			    buffer_tela = [],        		% The messages go first to a buffer subsequently to screen
			    flag_checkpoint = false,      	% checkpoint to unload the buffer to the log file
			    flag_checkpoint_tela = false, 	% checkpoint to unload the screen buffer
				log_file_dest,           		% path configuration where the logs will be written
				log_file_checkpoint,      		% timeout configuration to unload file buffer
				log_file_name,		      		% timeout configuration to unload file buffer
				debug							% It indicates whether it is in debug mode
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

debug(Msg) -> 
	gen_server:cast(?SERVER, {write_msg, debug, Msg}).

debug(Msg, Params) -> 
	gen_server:cast(?SERVER, {write_msg, debug, Msg, Params}). 

modo_debug(Flag) ->
	gen_server:cast(?SERVER, {debug, Flag}). 

sync() ->
	gen_server:call(?SERVER, sync_buffer). 		

log_request(Request) -> 
	gen_server:cast(?SERVER, {log_request, Request}). 


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
	Conf = msbus_config:getConfig(),
	LogFileDest = Conf#config.log_file_dest,
	Checkpoint = Conf#config.log_file_checkpoint,
	set_timeout_for_get_filename_logger(),
	%fprof:trace([start, {procs, [self()]}]),
    {ok, #state{log_file_dest = LogFileDest, 
                log_file_checkpoint = Checkpoint,
                log_file_name = get_filename_logger(LogFileDest),
                debug = Conf#config.modo_debug}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({write_msg, debug, Msg}, State=#state{debug = true}) ->
	NewState = write_msg(debug, Msg, State),
	{noreply, NewState};

handle_cast({write_msg, debug, Msg, Params}, State=#state{debug = true}) ->
	NewState = write_msg(debug, Msg, Params, State),
	{noreply, NewState};

handle_cast({write_msg, debug, _Msg}, State) ->
	{noreply, State};

handle_cast({write_msg, debug, _Msg, _Params}, State) ->
	{noreply, State};
    
handle_cast({write_msg, Tipo, Msg}, State) ->
	NewState = write_msg(Tipo, Msg, State),
	{noreply, NewState};

handle_cast({write_msg, Tipo, Msg, Params}, State) ->
	NewState = write_msg(Tipo, Msg, Params, State),
	{noreply, NewState};

handle_cast({log_request, Request}, State) ->
	do_log_request(Request, State),
	{noreply, State};

handle_cast({modo_debug, Flag}, State) ->
	{noreply, State#state{debug = Flag}};

handle_cast(sync_buffer, State) ->
	sync_buffer_tela(State),
	sync_buffer(State),
	{noreply, State#state{buffer = [], buffer_tela = [], flag_checkpoint = false, flag_checkpoint_tela = false}}.

handle_call({write_msg, Tipo, Msg}, _From, State) ->
	NewState = write_msg(Tipo, Msg, State),
	{reply, ok, NewState};

handle_call({write_msg, Tipo, Msg, Params}, _From, State) ->
	NewState = write_msg(Tipo, Msg, Params, State),
	{reply, ok, NewState};

handle_call(sync_buffer, _From, State) ->
	NewState = sync_buffer(State),
	{reply, ok, NewState}.

handle_info(checkpoint_tela, State) ->
   NewState = sync_buffer_tela(State),
   {noreply, NewState};

handle_info(checkpoint, State) ->
   NewState = sync_buffer(State),
   {noreply, NewState};

handle_info(checkpoint_get_filename_logger, State) ->
	io:format("ok"),
	NewLogFileName = get_filename_logger(State#state.log_file_dest),
	set_timeout_for_get_filename_logger(),
   {noreply, State#state{log_file_name = NewLogFileName}}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Internal functions
%%====================================================================
    
get_filename_logger(LogFileDest) -> 
	{{Ano,Mes,Dia},{Hora,Min,_Seg}} = calendar:local_time(),
	NodeName = msbus_util:get_node_name(),
	NomeArqLog = lists:flatten(io_lib:format("~s/~p/~p/~s/~s_~2..0w.~2..0w.~4..0w_~2..0w:~2..0w.log", [LogFileDest, node(), Ano, msbus_util:mes_extenso(Mes), NodeName, Dia, Mes, Ano, Hora, Min])),
	filelib:ensure_dir(NomeArqLog),
	NomeArqLog.

set_timeout_for_sync_buffer(#state{flag_checkpoint = false, log_file_checkpoint=Timeout}) ->    
	erlang:send_after(Timeout, self(), checkpoint);

set_timeout_for_sync_buffer(_State) ->    
	ok.

set_timeout_for_sync_tela(#state{flag_checkpoint_tela = false}) ->    
	erlang:send_after(1600, self(), checkpoint_tela);

set_timeout_for_sync_tela(_State) ->    
	ok.

set_timeout_for_get_filename_logger() ->    
	erlang:send_after(?LOG_ARCHIVE_CHECKPOINT, self(), checkpoint_get_filename_logger).

write_msg(Tipo, <<Msg/binary>>, State) ->
	Msg1 = binary_to_list(Msg),
    write_msg(Tipo, Msg1, State);
    
write_msg(Tipo, Msg, State) ->
	Msg1 = lists:concat([string:to_upper(atom_to_list(Tipo)), " ", msbus_util:timestamp_str(), "  ", Msg]),
	set_timeout_for_sync_buffer(State),
	set_timeout_for_sync_tela(State),
	State#state{buffer = [Msg1|State#state.buffer], buffer_tela = [Msg|State#state.buffer_tela], flag_checkpoint = true, flag_checkpoint_tela = true}.
	
write_msg(Tipo, Msg, Params, State) ->
	Msg1 = io_lib:format(Msg, Params),
	write_msg(Tipo, Msg1, State).
	
sync_buffer_tela(State = #state{buffer_tela = []}) -> State;

sync_buffer_tela(State) ->
	io:format("~s", [lists:map(fun(L) -> L ++ "\n" end, lists:reverse(State#state.buffer_tela))]),
	State#state{buffer_tela = [], flag_checkpoint_tela = false}.

sync_buffer(State = #state{buffer = []}) -> State;

sync_buffer(State) ->
	FileName = State#state.log_file_name,
	file:write_file(FileName, lists:map(fun(L) -> L ++ "\n" end, lists:reverse(State#state.buffer)), [append]),
	State#state{buffer = [], flag_checkpoint = false}.

do_log_request(Request = #request{protocolo = ldap}, _State) ->
	RID = Request#request.rid,
	Version = Request#request.versao_http,
	Metodo = Request#request.type,
	Url = Request#request.url,
	StatusSend = Request#request.status_send,
	Payload = Request#request.payload,
	Contract = Request#request.servico,
	Code = Request#request.code, 
	Reason = Request#request.reason, 
	Latencia = Request#request.latencia, 
	StatusSend = Request#request.status_send,
	Authorization = Request#request.authorization,
	case Contract of
		undefined -> Service = "";
		_ -> Service = Contract#servico.service
	end,
	Texto =  "~s ~s ~s {\n\tRID: ~p\n\tPayload: ~p\n\tService: ~s\n\tAuthorization: ~s\n\tStatus: ~p <<~s>> (~pms)\n\tSend: ~s\n}",
	Texto1 = io_lib:format(Texto, [Metodo, Url, Version, RID, Payload, Service, Authorization, Code, Reason, Latencia, StatusSend]),
	case Code of
		200 -> msbus_logger:info(Texto1);
		_ 	-> msbus_logger:error(Texto1)
	end;
	
	
do_log_request(Request = #request{protocolo = http}, _State) ->
	RID = Request#request.rid,
	Metodo = Request#request.type,
	Url = Request#request.url,
	Version = Request#request.versao_http,
	Accept = Request#request.accept,
	User_Agent = Request#request.user_agent,
	Payload = Request#request.payload,
	StatusSend = Request#request.status_send,
	Contract = Request#request.servico,
	Query = Request#request.querystring,
	Code = Request#request.code, 
	Reason = Request#request.reason, 
	Latencia = Request#request.latencia, 
	StatusSend = Request#request.status_send,
	Authorization = Request#request.authorization,
	case Contract of
		undefined -> Service = "";
		_ -> Service = Contract#servico.service
	end,
	case Payload of
		undefined ->
			Texto =  "~s ~s ~s {\n\tRID: ~p\n\tAccept: ~s:\n\tUser-Agent: ~s\n\tService: ~s\n\tQuery: ~p\n\tAuthorization: ~s\n\tStatus: ~p <<~s>> (~pms)\n\tSend: ~s\n}",
			Texto1 = io_lib:format(Texto, [Metodo, Url, Version, RID, Accept, User_Agent, Service, Query, Authorization, Code, Reason, Latencia, StatusSend]);
		_ ->
			Content_Type = Request#request.content_type,
			Texto =  "~s ~s ~s {\n\tRID: ~p\n\tAccept: ~s:\n\tUser-Agent: ~s\n\tContent-Type: ~s\n\tPayload: ~s\n\tService: ~s\n\tQuery: ~p\n\tAuthorization: ~s\n\tStatus: ~p <<~s>> (~pms)\n\tSend: ~s\n}",
			Texto1 = io_lib:format(Texto, [Metodo, Url, Version, RID, Accept, User_Agent, Content_Type, Payload, Service, Query, Authorization, Code, Reason, Latencia, StatusSend])
	end,
	case Code of
		200 -> msbus_logger:info(Texto1);
		_ 	-> msbus_logger:error(Texto1)
	end.
