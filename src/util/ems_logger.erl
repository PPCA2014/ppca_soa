%%********************************************************************
%% @title Module ems_logger
%% @version 1.0.0
%% @doc Module responsible for the logger component.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_logger).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).

%% Client API
-export([error/1, error/2, 
		 info/1, info/2, 
		 warn/1, warn/2, 
		 debug/1, debug/2, debug2/1, debug2/2, in_debug/0,
		 sync/0, 
		 log_request/1,
		 mode_debug/1,
		 set_level/1,
		 show_response/1,
		 format_warn/1, 
		 format_warn/2,
		 format_error/1, 
		 format_error/2,
		 format_debug/1, 
		 format_debug/2,
 		 format_alert/1, 
		 format_alert/2
]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


%  Armazena o estado do ems_logger. 
-record(state, {buffer = [],             				% The messages go first to a buffer subsequently to the log file        
			    buffer_tela = [],        				% The messages go first to a buffer subsequently to screen
			    flag_checkpoint_sync_buffer = false,    % checkpoint to unload the buffer to the log file
			    flag_checkpoint_tela = false, 			% checkpoint to unload the screen buffer
				log_file_checkpoint,      				% timeout archive log checkpoing
				log_file_name,		      				% log file name
				log_file_handle,						% IODevice of file
				log_file_max_size,						% Max file size in KB
				sync_buffer_error_count = 0,			% Attempts to unload buffer
				level = info,							% level of errors
				show_response = false					% show response of request
 			   }). 


%%====================================================================
%% Server API
%%====================================================================

start(Service) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Service, []).
 
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
	case in_debug() of
		true -> gen_server:cast(?SERVER, {write_msg, debug, Msg});
		_ -> ok
	end.

debug(Msg, Params) -> 
	case in_debug() of
		true -> gen_server:cast(?SERVER, {write_msg, debug, Msg, Params});
		_ -> ok
	end.

debug2(Msg) -> 
	case in_debug() of
		true -> 
			Msg2 = lists:concat(["\033[0;34mDEBUG ", ems_clock:local_time_str(), "  ", Msg, "\033[0m"]),
			io:format(Msg2);
		_ -> ok
	end.

debug2(Msg, Params) -> 
	case in_debug() of
		true -> 
			Msg2 = lists:concat(["\033[0;34mDEBUG ", ems_clock:local_time_str(), "  ", io_lib:format(Msg, Params), "\033[0m"]),
			io:format(Msg2);
		_ -> ok
	end.


in_debug() -> ets:lookup(debug_ets, debug) =:= [{debug, true}].

mode_debug(true)  -> ets:insert(debug_ets, {debug, true});
mode_debug(false) -> ets:insert(debug_ets, {debug, false}).

sync() ->
	gen_server:call(?SERVER, sync_buffer). 		

log_request(Request) -> 
	gen_server:cast(?SERVER, {log_request, Request}). 

set_level(Level) -> 
	gen_server:cast(?SERVER, {set_level, Level}). 

show_response(Value) -> 
	gen_server:cast(?SERVER, {show_response, Value}). 


% write direct messages to console
format_warn(Message) ->	io:format("\033[0;33m~s\033[0m", [Message]).
format_warn(Message, Params) ->	io:format("\033[0;33m~s\033[0m", [io_lib:format(Message, Params)]).

format_error(Message) -> io:format("\033[0;31m~s\033[0m", [Message]).
format_error(Message, Params) -> io:format("\033[0;31m~s\033[0m", [io_lib:format(Message, Params)]).

format_debug(Message) -> io:format("\033[0;34m~s\033[0m", [Message]).
format_debug(Message, Params) -> io:format("\033[0;34m~s\033[0m", [io_lib:format(Message, Params)]).

format_alert(Message) ->	io:format("\033[0;46m~s\033[0m", [Message]).
format_alert(Message, Params) ->	io:format("\033[0;46m~s\033[0m", [io_lib:format(Message, Params)]).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{properties = Props}) ->
	ems_logger:info("Loading ERLANGMS ~s...", [?SERVER_NAME]),
	Checkpoint = maps:get(<<"log_file_checkpoint">>, Props, ?LOG_FILE_CHECKPOINT),
	LogFileMaxSize = maps:get(<<"log_file_max_size">>, Props, ?LOG_FILE_MAX_SIZE),
	Conf = ems_config:getConfig(),
	Debug = Conf#config.ems_debug,
	mode_debug(Debug),
	State = checkpoint_arquive_log(#state{log_file_checkpoint = Checkpoint,
										  log_file_max_size = LogFileMaxSize,
										  log_file_handle = undefined}, false),
    {ok, State}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({write_msg, Tipo, Msg}, State) ->
	NewState = write_msg(Tipo, Msg, State),
	{noreply, NewState};

handle_cast({write_msg, Tipo, Msg, Params}, State) ->
	NewState = write_msg(Tipo, Msg, Params, State),
	{noreply, NewState};

handle_cast({log_request, Request}, State) ->
	do_log_request(Request, State),
	{noreply, State};

handle_cast({set_level, Level}, State) ->
	{noreply, State#state{level = Level}};

handle_cast({show_response, Value}, State) ->
	{noreply, State#state{show_response = Value}};

handle_cast(sync_buffer, State) ->
	State2 = sync_buffer_tela(State),
	State3 = sync_buffer(State2),
	{noreply, State3}.

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

handle_info(checkpoint_archive_log, State) ->
	Reply = checkpoint_arquive_log(State, false),
	{noreply, Reply}.

terminate(_Reason, #state{log_file_handle = undefined}) ->
    ok;

terminate(_Reason, #state{log_file_handle = IODevice}) ->
    file:close(IODevice),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Internal functions
%%====================================================================

-spec checkpoint_arquive_log(#state{}, boolean()) -> #state{} | {error, atom()}.
checkpoint_arquive_log(State = #state{log_file_handle = IODevice}, Immediate) ->
	case Immediate of
		true -> ems_logger:info("ems_logger immediate archive log file checkpoint.");
		false -> ems_logger:info("ems_logger archive log file checkpoint.")
	end,
	close_filename_device(IODevice),
	case open_filename_device() of
		{ok, LogFileName, IODevice2} ->
			ems_logger:info("ems_logger open the log file ~p for append.", [LogFileName]),
			State2 = State#state{log_file_name = LogFileName, 
								 log_file_handle = IODevice2,
								 sync_buffer_error_count = 0};
		{error, Reason} ->
			ems_logger:error("ems_logger archive log file checkpoint error: ~p.", [Reason]),
			State2 = State
	end,
	set_timeout_archive_log_checkpoint(),
	State2.

    
open_filename_device() -> 
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	MesAbrev = ems_util:mes_abreviado(Mes),
	NodeName = ems_util:get_node_name(),
	LogFileName = lists:flatten(io_lib:format("~s/~s/~p/~s/~s_~s_~2..0w~2..0w~4..0w_~2..0w~2..0w~2..0w.log", [?LOG_PATH, atom_to_list(node()), Ano, MesAbrev, NodeName, MesAbrev, Dia, Mes, Ano, Hora, Min, Seg])),
	open_filename_device(LogFileName).

open_filename_device(LogFileName) ->
	case filelib:ensure_dir(LogFileName) of
		ok ->
			case file:open(LogFileName, [append, {delayed_write, 256, 2}]) of
				{ok, IODevice} -> 
					{ok, LogFileName, IODevice};
				{error, enospc} = Error ->
					ems_logger:error("ems_logger does not have disk storage space to write to the log files."),
					Error;
				{error, Reason} = Error -> 
					ems_logger:error("ems_logger failed to open log file for append. Reason: ~p.", [Reason]),
					Error
			end;
		{error, Reason} = Error -> 
			ems_logger:error("ems_logger failed to create log file dir. Reason: ~p.", [Reason]),
			Error
	end.

close_filename_device(undefined) -> ok;
close_filename_device(IODevice) -> file:close(IODevice).

set_timeout_for_sync_buffer(#state{flag_checkpoint_sync_buffer = false, log_file_checkpoint=Timeout}) ->    
	%?DEBUG("ems_logger set_timeout_for_sync_buffer."),
	erlang:send_after(Timeout, self(), checkpoint);

set_timeout_for_sync_buffer(_State) ->    
	ok.

set_timeout_for_sync_tela(#state{flag_checkpoint_tela = false}) ->    
	%?DEBUG("ems_logger set_timeout_for_sync_tela."),
	erlang:send_after(2000, self(), checkpoint_tela);

set_timeout_for_sync_tela(_State) ->    
	ok.

set_timeout_archive_log_checkpoint() ->    
	%?DEBUG("ems_logger set_timeout_archive_log_checkpoint."),
	erlang:send_after(?LOG_ARCHIVE_CHECKPOINT, self(), checkpoint_archive_log).

write_msg(Tipo, Msg, State) when is_binary(Msg) ->
	Msg1 = binary_to_list(Msg),
    write_msg(Tipo, Msg1, State);
write_msg(Tipo, Msg, State = #state{level = Level})  ->
	case Tipo of
		info  -> Msg1 = lists:concat(["INFO ", ems_clock:local_time_str(), "  ", Msg]);
		error -> Msg1 = lists:concat(["\033[0;31mERROR ", ems_clock:local_time_str(), "  ", Msg, "\033[0m"]);
		warn  -> Msg1 = lists:concat(["\033[0;33mWARN ", ems_clock:local_time_str(), "  ", Msg, "\033[0m"]);
		debug -> Msg1 = lists:concat(["\033[0;34mDEBUG ", ems_clock:local_time_str(), "  ", Msg, "\033[0m"])
	end,
	UltMsg = erlang:get(ult_msg),
	case UltMsg == undefined orelse UltMsg =/= Msg1 of
		true ->
			erlang:put(ult_msg, Msg1),
			case Level == error andalso Tipo /= error of
				true ->
					set_timeout_for_sync_buffer(State),
					State#state{buffer = [Msg1|State#state.buffer], 
								flag_checkpoint_sync_buffer = true};
				false ->
					set_timeout_for_sync_buffer(State),
					set_timeout_for_sync_tela(State),
					State#state{buffer = [Msg1|State#state.buffer], 
								buffer_tela = [Msg1|State#state.buffer_tela], 
								flag_checkpoint_sync_buffer = true, 
								flag_checkpoint_tela = true}
			end;
		false -> 
			?DEBUG("ems_logger skip write_msg. Type: ~p, Level: ~p.", [Tipo, Level]),
			State
	end.
		
	
write_msg(Tipo, Msg, Params, State) ->
	Msg1 = io_lib:format(Msg, Params),
	write_msg(Tipo, Msg1, State).
sync_buffer_tela(State = #state{buffer_tela = []}) -> State;
sync_buffer_tela(State) ->
	Msg = [ [L | ["\n"]] || L <- lists:reverse(State#state.buffer_tela)],
	io:format(Msg),
	State#state{buffer_tela = [], flag_checkpoint_tela = false}.


sync_buffer(State = #state{buffer = []}) -> State;
sync_buffer(State = #state{sync_buffer_error_count = 10}) ->
	ems_logger:error("ems_logger tried to unload cache buffer 10 times without success. Clear log buffer cache."),
	State#state{buffer = [], flag_checkpoint_sync_buffer = false, sync_buffer_error_count = 0};
sync_buffer(State = #state{buffer = Buffer,
						   log_file_name = CurrentLogFileName,
						   log_file_max_size = LogFileMaxSize,
						   sync_buffer_error_count = SyncBufferErrorCount}) ->
	%?DEBUG("ems_logger sync_buffer to log file ~p. Buffer count: ~p, FileSize: ~p.", [CurrentLogFileName, string:len(Buffer), filelib:file_size(CurrentLogFileName)]),
	% check limit log file max size
	case filelib:file_size(CurrentLogFileName) > LogFileMaxSize of
		true -> 
			ems_logger:info("ems_logger is writing to a log file that has already exceeded the allowed limit."),
			State2 = checkpoint_arquive_log(State, true),
			State2#state{flag_checkpoint_sync_buffer = false, 
						 sync_buffer_error_count = 0};
		false ->
			% IoDevice is really the pid of the process that handles the file. 
			% Open again to verify that the file actually exists
			case open_filename_device(CurrentLogFileName) of
				{ok, LogFileName, IODevice} -> 
					Msg = [ [L | ["\n"]] || L <- lists:reverse(Buffer)],
					case file:write(IODevice, Msg) of
						ok -> 
							State#state{buffer = [], 
										flag_checkpoint_sync_buffer = false, 
										log_file_handle = IODevice, 
										log_file_name = LogFileName, 
										sync_buffer_error_count = 0};
						{error, enospc} -> 
							ems_logger:error("ems_logger does not have disk storage space to write to the log files. Clear log buffer cache."),
							State#state{buffer = [], 
										flag_checkpoint_sync_buffer = false, 
										log_file_handle = IODevice, 
										log_file_name = LogFileName,
										sync_buffer_error_count = 0};
						{error, ebadf} ->
							ems_logger:error("ems_logger does no have log file descriptor valid. Clear log buffer cache."),
							State#state{buffer = [], 
										flag_checkpoint_sync_buffer = false, 
										log_file_handle = IODevice, 
										log_file_name = LogFileName,
										sync_buffer_error_count = 0};
						{error, Reason} ->
							ems_logger:error("ems_logger was unable to unload the log buffer cache. Reason: ~p. Clear log buffer cache.", [Reason]),
							State#state{buffer = [], 
										flag_checkpoint_sync_buffer = false, 
										log_file_handle = IODevice, 
										log_file_name = LogFileName,
										sync_buffer_error_count = 0}
					end;
				{error, Reason} -> 
					ems_logger:error("ems_logger was unable to open log file ~p to unload the log buffer cache. Reason: ~p.", [CurrentLogFileName, Reason]),
					State2 = checkpoint_arquive_log(State#state{sync_buffer_error_count = SyncBufferErrorCount + 1}, true),
					State2
			end
	end.

	
do_log_request(#request{rid = RID,
						req_hash = ReqHash,
						type = Metodo,
						uri = Uri,
						version = Version,
						content_type = ContentType,
						accept = Accept,
						ip_bin = IpBin,
						payload_map = Payload,
						service = Service,
						params_url = Params,
						querystring_map = Query,
						code = Code,
						reason = Reason,
						latency = Latency,
						result_cache = ResultCache,
						result_cache_rid = ResultCacheRid,
						response_header = ResponseHeader,
						response_data = ResponseData,
						authorization = Authorization,
						cache_control = CacheControl,
						etag = Etag,
						if_modified_since = IfModifiedSince,
						if_none_match = IfNoneMatch,
						node_exec = Node,
						referer = Referer,
						user_agent = UserAgent,
						filename = FileName}, 
			  #state{show_response = ShowResponse}) ->
	Texto =  "~s ~s ~s {\n\tRID: ~p  (ReqHash: ~p)\n\tContent-Type: ~p\n\tAccept: ~p\n\tPeer: ~p\n\tReferer: ~p\n\tUser-Agent: ~p\n\tService: ~p\n\tParams: ~p\n\tQuery: ~p\n\tPayload: ~p\n\t~sResult-Cache: ~s\n\tCache-Control: ~s\n\tETag: ~p\n\tIf-Modified-Since: ~p\n\tIf-None-Match: ~p\n\tAuthorization: ~p\n\tNode: ~s\n\tFileName: ~s\n\tStatus: ~p <<~p>> (~pms)\n}",
	Texto1 = io_lib:format(Texto, [Metodo, 
								   Uri, 
								   Version, 
								   RID,
								   ReqHash,
								   ContentType, 
								   Accept,
								   IpBin, 
								   Referer,
								   UserAgent,
								   case Service of 
										undefined -> <<>>; 
										_ -> Service#service.service 
								   end,
								   Params,
								   Query, 
								   Payload, 
								   case ShowResponse of true -> io_lib:format("Header Response: ~p\n\tResponse: ~p\n\t", [ResponseHeader, ResponseData]); false -> <<>> end,
								   case Service =/= undefined of
										true ->
										   case ResultCache of 
												true -> io_lib:format("~sms  <<RID: ~s>>", [integer_to_list(Service#service.result_cache), integer_to_list(ResultCacheRid)]); 
												false -> integer_to_list(Service#service.result_cache) ++ "ms" 
											end;
										false -> "0ms"
								   end,
								   CacheControl,
								   Etag,
								   IfModifiedSince,
								   IfNoneMatch,
								   Authorization,
								   Node, 
								   FileName,
								   Code, 
								   Reason, 
								   Latency]),
	case Code >= 400 of
		true  -> ems_logger:error(Texto1);
		false -> ems_logger:info(Texto1)
	end.

