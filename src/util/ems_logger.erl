%%********************************************************************
%% @title Module ems_logger
%% @version 1.0.0
%% @doc Module responsible for the logger component.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_logger).

-behavior(gen_server). 

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

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
		 log_file_tail/0,
		 log_file_tail/1,
		 log_file_head/0,
		 log_file_head/1,
		 log_file_name/0,
		 format_info/1, 
		 format_info/2,
		 format_warn/1, 
		 format_warn/2,
		 format_error/1, 
		 format_error/2,
		 format_debug/1, 
		 format_debug/2,
 		 format_alert/1, 
		 format_alert/2,
		 checkpoint/0
]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


%  Armazena o estado do ems_logger. 
-record(state, {buffer = [],             				% The messages go first to a buffer subsequently to the log file        
			    buffer_tela = [],        				% The messages go first to a buffer subsequently to screen
			    flag_checkpoint_sync_buffer = false,    % checkpoint to unload the buffer to the log file
			    flag_checkpoint_screen = false, 			% checkpoint to unload the screen buffer
				log_file_checkpoint,      				% timeout archive log checkpoing
				log_file_name,		      				% log file name
				log_file_handle,						% IODevice of file
				log_file_max_size,						% Max file size in KB
				level = info,							% level of errors
				show_response = false,					% show response of request
				ult_msg,								% last print message
				ult_reqhash
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
			Msg2 = lists:concat(["\033[1;34mDEBUG ", ems_clock:local_time_str(), "  ", Msg, "\033[0m"]),
			io:format(Msg2);
		_ -> ok
	end.

debug2(Msg, Params) -> 
	case in_debug() of
		true -> 
			Msg2 = lists:concat(["\033[1;34mDEBUG ", ems_clock:local_time_str(), "  ", io_lib:format(Msg, Params), "\033[0m"]),
			io:format(Msg2);
		_ -> ok
	end.


in_debug() -> ets:lookup(debug_ets, debug) =:= [{debug, true}].

mode_debug(true)  -> 
	info("ems_logger debug mode enabled."),
	ets:insert(debug_ets, {debug, true});
mode_debug(false) -> 
	info("ems_logger debug mode disabled."),
	ets:insert(debug_ets, {debug, false}).

sync() ->
	info("ems_logger sync buffer."),
	gen_server:call(?SERVER, sync_buffer). 		

log_request(Request) -> 
	gen_server:cast(?SERVER, {log_request, Request}). 

set_level(Level) -> 
	info("ems_logger set level ~p.", [Level]),
	gen_server:cast(?SERVER, {set_level, Level}). 

show_response(Value) when Value == true -> 
	info("ems_logger set show response."),
	gen_server:cast(?SERVER, {show_response, Value});
show_response(_) -> 
	info("ems_logger unset show response."),
	gen_server:cast(?SERVER, {show_response, false}). 

log_file_head() ->
	gen_server:call(?SERVER, {log_file_head, 80}). 		

log_file_head(N) ->
	gen_server:call(?SERVER, {log_file_head, N}). 		

log_file_tail() ->
	gen_server:call(?SERVER, {log_file_tail, 80}). 		

log_file_tail(N) ->
	gen_server:call(?SERVER, {log_file_tail, N}). 		

log_file_name() ->
	gen_server:call(?SERVER, log_file_name). 		
	
checkpoint() -> 
	info("ems_logger archive log file checkpoint."),
	?SERVER ! checkpoint_archive_log.


% write direct messages to console
format_info(Message) ->	io:format(Message).
format_info(Message, Params) ->	io:format("~s", [io_lib:format(Message, Params)]).

format_warn(Message) ->	io:format("\033[0;33m~s\033[0m", [Message]).
format_warn(Message, Params) ->	io:format("\033[0;33m~s\033[0m", [io_lib:format(Message, Params)]).

format_error(Message) -> io:format("\033[0;31m~s\033[0m", [Message]).
format_error(Message, Params) -> io:format("\033[0;31m~s\033[0m", [io_lib:format(Message, Params)]).

format_debug(Message) -> io:format("\033[0;34m~s\033[0m", [Message]).
format_debug(Message, Params) -> io:format("\033[1;34m~s\033[0m", [io_lib:format(Message, Params)]).

format_alert(Message) ->	io:format("\033[0;46m~s\033[0m", [Message]).
format_alert(Message, Params) ->	io:format("\033[0;46m~s\033[0m", [io_lib:format(Message, Params)]).




%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{properties = Props}) ->
	info("Loading ESB ~s instance on Erlang/OTP ~s.", [?SERVER_NAME, erlang:system_info(otp_release)]),
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
	NewState = do_log_request(Request, State),
	{noreply, NewState};

handle_cast({set_level, Level}, State) ->
	{noreply, State#state{level = Level}};

handle_cast({show_response, Value}, State) ->
	{noreply, State#state{show_response = Value}};

handle_cast(sync_buffer, State) ->
	State2 = sync_buffer_screen(State),
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
	{reply, ok, NewState};

handle_call(log_file_name, _From, State = #state{log_file_name = FilenameLog}) ->
	{reply, FilenameLog, State};

handle_call({log_file_head, N}, _From, State) ->
	Result = log_file_head(State, N),
	{reply, Result, State};

handle_call({log_file_tail, N}, _From, State) ->
	Result = log_file_tail(State, N),
	{reply, Result, State}.

handle_info(checkpoint_tela, State) ->
   NewState = sync_buffer_screen(State),
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
checkpoint_arquive_log(State = #state{log_file_handle = CurrentIODevice, 
									  log_file_name = CurrentLogFilename}, Immediate) ->
	case Immediate of
		true -> 
			ems_db:inc_counter(ems_logger_immediate_archive_log_checkpoint),
			ems_logger:info("ems_logger immediate archive log file checkpoint.");
		false -> 
			ems_db:inc_counter(ems_logger_archive_log_checkpoint),
			ems_logger:info("ems_logger archive log file checkpoint.")
	end,
	close_filename_device(CurrentIODevice, CurrentLogFilename),
	case open_filename_device() of
		{ok, LogFilename, IODevice2} ->
			ems_logger:info("ems_logger open ~p.", [LogFilename]),
			State2 = State#state{log_file_name = LogFilename, 
								 log_file_handle = IODevice2};
		{error, Reason} ->
			ems_db:inc_counter(ems_logger_archive_log_error),
			ems_logger:error("ems_logger archive log file checkpoint exception: ~p.", [Reason]),
			State2 = State
	end,
	set_timeout_archive_log_checkpoint(),
	State2.

    
open_filename_device() -> 
	{{Ano,Mes,Dia},{Hora,Min,_}} = calendar:local_time(),
	MesAbrev = ems_util:mes_abreviado(Mes),
	LogFilename = lists:flatten(io_lib:format("~s/~p/~s/~s_~s_~2..0w~2..0w~4..0w_~2..0w~2..0w.log", [?LOG_PATH, Ano, MesAbrev, "emsbus", MesAbrev, Dia, Mes, Ano, Hora, Min])),
	open_filename_device(LogFilename).

open_filename_device(LogFilename) ->
	case filelib:ensure_dir(LogFilename) of
		ok ->
			case file:open(LogFilename, [append, {delayed_write, 256, 2}]) of
				{ok, IODevice} -> 
					{ok, LogFilename, IODevice};
				{error, enospc} = Error ->
					ems_db:inc_counter(ems_logger_open_file_enospc),
					ems_logger:error("ems_logger open_filename_device does not have disk storage space to write to the log files."),
					Error;
				{error, Reason} = Error -> 
					ems_db:inc_counter(ems_logger_open_file_error),
					ems_logger:error("ems_logger open_filename_device failed to open log file. Reason: ~p.", [Reason]),
					Error
			end;
		{error, Reason} = Error -> 
			ems_db:inc_counter(ems_logger_open_file_error),
			ems_logger:error("ems_logger open_filename_device failed to create log file dir. Reason: ~p.", [Reason]),
			Error
	end.

log_file_head(#state{log_file_name = LogFilename}, N) ->
	case ems_util:head_file(LogFilename, N) of
		{ok, List} -> {ok, List};
		{error, Reason} = Error -> 
			ems_logger:error("ems_logger log_file_head failed to open log file for read. Reason: ~p.", [Reason]),
			Error
	end.

log_file_tail(#state{log_file_name = LogFilename}, N) ->
	case ems_util:tail_file(LogFilename, N) of
		{ok, List} -> {ok, List};
		{error, Reason} = Error -> 
			ems_logger:error("ems_logger log_file_tail failed to open log file for read. Reason: ~p.", [Reason]),
			Error
	end.

close_filename_device(undefined, _) -> ok;
close_filename_device(IODevice, LogFilename) -> 
	?DEBUG("ems_logger close log file ~p.", [LogFilename]),
	file:close(IODevice).

set_timeout_for_sync_buffer(#state{flag_checkpoint_sync_buffer = false, log_file_checkpoint=Timeout}) ->    
	erlang:send_after(Timeout, self(), checkpoint);

set_timeout_for_sync_buffer(_State) ->    
	ok.

set_timeout_for_sync_tela(#state{flag_checkpoint_screen = false}) ->    
	erlang:send_after(2000, self(), checkpoint_tela);

set_timeout_for_sync_tela(_State) ->    
	ok.

set_timeout_archive_log_checkpoint() ->    
	erlang:send_after(?LOG_ARCHIVE_CHECKPOINT, self(), checkpoint_archive_log).

write_msg(Tipo, Msg, State = #state{level = Level, ult_msg = UltMsg})  ->
	%% test overflow duplicated messages
	case UltMsg == undefined orelse UltMsg =/= Msg of
		true ->
			case Tipo of
				info  -> 
					ems_db:inc_counter(ems_logger_write_info),
					Msg1 = iolist_to_binary([<<"INFO ">>, ems_clock:local_time_str(), <<"  ">>, Msg, <<"\n">>]);
				error -> 
					ems_db:inc_counter(ems_logger_write_error),
					Msg1 = iolist_to_binary([<<"\033[0;31mERROR ">>, ems_clock:local_time_str(), <<"  ">>, Msg, <<"\033[0m\n">>]);
				warn  -> 
					ems_db:inc_counter(ems_logger_write_warn),
					Msg1 = iolist_to_binary([<<"\033[0;33mWARN ">>, ems_clock:local_time_str(), <<"  ">>, Msg, <<"\033[0m\n">>]);
				debug -> 
					ems_db:inc_counter(ems_logger_write_debug),
					Msg1 = iolist_to_binary([<<"\033[1;34mDEBUG ">>, ems_clock:local_time_str(), <<"  ">>, Msg, <<"\033[0m\n">>])
			end,
			case (Level == error andalso Tipo /= error) andalso (Tipo /= debug) of
				true ->
					case length(State#state.buffer) == 200 of
						true -> 
							ems_db:inc_counter(ems_logger_immediate_sync_buffer),
							State2 = sync_buffer_screen(State),
							State2#state{buffer = [Msg1|State#state.buffer], 
										 flag_checkpoint_sync_buffer = true,
										 ult_msg = Msg};
						false -> 
							set_timeout_for_sync_buffer(State),
							State#state{buffer = [Msg1|State#state.buffer], 
									    flag_checkpoint_sync_buffer = true,
										ult_msg = Msg}
					end;
				false ->
					case length(State#state.buffer) == 200 of
						true -> 
							ems_db:inc_counter(ems_logger_immediate_sync_buffer),
							State2 = sync_buffer_screen(State),
							State3 = sync_buffer(State2),
							State3#state{buffer = [Msg1|State#state.buffer], 
										 buffer_tela = [Msg1|State#state.buffer_tela], 
										 flag_checkpoint_sync_buffer = true, 
										 flag_checkpoint_screen = true,
										 ult_msg = Msg};
						false ->
							set_timeout_for_sync_buffer(State),
							set_timeout_for_sync_tela(State),
							State#state{buffer = [Msg1|State#state.buffer], 
										buffer_tela = [Msg1|State#state.buffer_tela], 
										flag_checkpoint_sync_buffer = true, 
										flag_checkpoint_screen = true,
										ult_msg = Msg}
					end
			end;
		false -> 
			ems_db:inc_counter(ems_logger_write_dup),
			State
	end.
	
write_msg(Tipo, Msg, Params, State) ->
	Msg1 = io_lib:format(Msg, Params),
	write_msg(Tipo, Msg1, State).
	
	
sync_buffer_screen(State = #state{buffer_tela = []}) -> State;
sync_buffer_screen(State) ->
	ems_db:inc_counter(ems_logger_sync_buffer_screen),
	Msg = lists:reverse(State#state.buffer_tela),
	try
		io:format(Msg)
	catch
		_:_ -> ok
	end,
	State#state{buffer_tela = [], flag_checkpoint_screen = false, ult_msg = undefined, ult_reqhash = undefined}.


sync_buffer(State = #state{buffer = Buffer,
						   log_file_name = CurrentLogFilename,
						   log_file_max_size = LogFileMaxSize,
						   log_file_handle = CurrentIODevice}) ->
	ems_db:inc_counter(ems_logger_sync_buffer),
	FileSize = filelib:file_size(CurrentLogFilename),
	case FileSize > LogFileMaxSize of 	% check limit log file max size
		true -> 
			ems_db:inc_counter(ems_logger_sync_buffer_file_size_exceeded),
			ems_logger:info("ems_logger is writing to a log file that has already exceeded the allowed limit."),
			State2 = checkpoint_arquive_log(State, true);
		false ->
			case FileSize == 0 of % Check file deleted
				true ->
					close_filename_device(CurrentIODevice, CurrentLogFilename),
					{ok, LogFilename, IODevice} = open_filename_device(),
					State2 = State#state{buffer = [], 
										 log_file_handle = IODevice,
										 log_file_name = LogFilename,
										 flag_checkpoint_sync_buffer = false};
				false ->
					State2 = State#state{buffer = [], 
										 flag_checkpoint_sync_buffer = false}
			end
	end,
	Msg = lists:reverse(Buffer),
	case file:write(State2#state.log_file_handle, Msg) of
		ok -> ok;
		{error, enospc} -> 
			ems_db:inc_counter(ems_logger_sync_buffer_enospc),
			ems_logger:error("ems_logger does not have disk storage space to write to the log files.");
		{error, ebadf} ->
			ems_db:inc_counter(ems_logger_sync_buffer_ebadf),
			ems_logger:error("ems_logger does no have log file descriptor valid.");
		{error, Reason} ->
			ems_db:inc_counter(ems_logger_sync_buffer_error),
			ems_logger:error("ems_logger was unable to unload the log buffer cache. Reason: ~p.", [Reason])
	end,
	State2.

	
do_log_request(#request{rid = RID,
						req_hash = ReqHash,
						type = Type,
						uri = Uri,
						version = Version,
						content_type_in = ContentTypeIn,
						content_type = ContentType,
						content_length = ContentLength,
						accept = Accept,
						ip_bin = IpBin,
						payload = Payload,
						service = Service,
						params_url = Params,
						querystring_map = Query,
						code = Code,
						reason = Reason,
						latency = Latency,
						result_cache = ResultCache,
						result_cache_rid = ResultCacheRid,
						response_data = ResponseData,
						authorization = Authorization,
						cache_control = CacheControl,
						etag = Etag,
						if_modified_since = IfModifiedSince,
						if_none_match = IfNoneMatch,
						node_exec = Node,
						referer = Referer,
						user_agent = UserAgent,
						user_agent_version = UserAgentVersion,
						filename = Filename,
						client = Client,
						user = User,
						scope = Scope,
						oauth2_grant_type = GrantType,
						oauth2_access_token = AccessToken,
						oauth2_refresh_token = RefreshToken
			  }, 
			  State = #state{show_response = ShowResponse, ult_reqhash = UltReqHash}) ->
	try
		case UltReqHash == undefined orelse UltReqHash =/= ReqHash of
			true ->
				Texto1 = 
					  iolist_to_binary([
					   Type, <<" ">>,
					   Uri, <<" ">>,
					   atom_to_binary(Version, utf8), <<" ">>,
					   <<" {\n\tRID: ">>,  integer_to_binary(RID),
					   <<"  (ReqHash: ">>, integer_to_binary(ReqHash), <<")">>, 
					   <<"\n\tAccept: ">>, Accept,
					   <<"\n\tContent-Type in: ">>, case ContentTypeIn of
														undefined -> <<>>;
														_ -> ContentTypeIn
													end, 
						<<"\n\tContent-Type out: ">>,  case ContentType of
															undefined -> <<>>;
															_ -> ContentType 
													   end,
						<<"\n\tPeer: ">>, IpBin, <<"  Referer: ">>, case Referer of
																		undefined -> <<>>;
																		_ -> Referer
																	end,
						<<"\n\tUser-Agent: ">>, ems_util:user_agent_atom_to_binary(UserAgent), <<"  Version: ">>, UserAgentVersion,	
						<<"\n\tService: ">>, case Service of 
												undefined -> <<>>; 
												_ -> Service#service.service 
											 end,
						<<"\n\tParams: ">>, ems_util:print_int_map(Params), 
						<<"\n\tQuery: ">>, ems_util:print_str_map(Query), 
						<<"\n\tPayload: ">>, case ContentLength < 90 of
												true -> Payload;
												false -> [binary:part(Payload, 0, 89), <<"...">>]
											 end,
						 case ShowResponse andalso byte_size(ResponseData) < 90 of 
							true -> [<<"\n\tResponse: ">>, ResponseData]; 
							false -> <<>> 
						end,
						case Service =/= undefined of
							true ->
							   case Service#service.result_cache > 0 of
									true ->
									   ResultCacheSec = trunc(Service#service.result_cache / 1000),
									   case ResultCacheSec > 0 of 
											true  -> ResultCacheMin = trunc(ResultCacheSec / 60);
											false -> ResultCacheMin = 0
									   end,
									   case ResultCacheMin > 0 of
											true -> 
											   case ResultCache of 
													true ->  [<<"\n\tResult-Cache: ">>, integer_to_list(Service#service.result_cache), <<"ms (">>, integer_to_binary(ResultCacheMin), <<"min)  <<RID: ">>, integer_to_binary(ResultCacheRid), <<">>">>];
													false -> [<<"\n\tResult-Cache: ">>, integer_to_list(Service#service.result_cache), <<"ms (">>, integer_to_binary(ResultCacheMin), <<"min)">>] 
												end;
											false ->
											   case ResultCacheSec > 0 of
													true -> 
													   case ResultCache of 
															true ->  [<<"\n\tResult-Cache: ">>, integer_to_list(Service#service.result_cache), <<"ms (">>, integer_to_binary(ResultCacheSec), <<"sec)  <<RID: ">>, integer_to_binary(ResultCacheRid), <<">>">>];
															false -> [<<"\n\tResult-Cache: ">>, integer_to_list(Service#service.result_cache), <<"ms (">>, integer_to_binary(ResultCacheSec), <<"sec)">>] 
														end;
													false ->
													   case ResultCache of 
															true ->  [<<"\n\tResult-Cache: ">>, integer_to_list(Service#service.result_cache), <<"ms <<RID: ">>, integer_to_binary(ResultCacheRid), <<">>">>];
															false -> [<<"\n\tResult-Cache: ">>, integer_to_list(Service#service.result_cache), <<"ms">>]
														end
												end
										end;
									false -> <<>>
								end;
							false -> <<>>
						end,
					   <<"\n\tCache-Control: ">>, case CacheControl of
													undefined -> <<>>;
													_ -> CacheControl
											  end,  
						<<"  ETag: ">>, case Etag of
										undefined -> <<>>;
										_ -> Etag
									end,
						<<"\n\tIf-Modified-Since: ">>, case IfModifiedSince of
														undefined -> <<>>;
														_ -> IfModifiedSince
												   end,
					   <<"  If-None-Match: ">>, case IfNoneMatch of
												undefined -> <<>>;
												_ -> IfNoneMatch
										   end,
					   <<"\n\tAuthorization mode: ">>, case Service of 
														undefined -> <<>>; 
														_ -> 
															case Service#service.authorization of
																basic -> <<"basic, oauth2">>;
																oauth2 -> <<"oauth2">>;
																_ -> <<"public">>
															end
												   end,
					   <<"\n\tAuthorization header: <<">>, case Authorization of
															undefined -> <<>>;
															_ -> Authorization
														 end, <<">>">>,
					   case GrantType of
									undefined -> <<>>;
									_ -> [<<"\n\tOAuth2 grant type: ">>, GrantType]
					   end,
					   case AccessToken of
							undefined -> <<>>;
							_ ->  [<<"\n\tOAuth2 access token: ">>, AccessToken]
					   end,
					   case RefreshToken of
							undefined -> <<>>;
							_ ->  [<<"\n\tOAuth2 refresh token: ">>, RefreshToken]
					   end,
					   case Scope of
							undefined -> <<>>;
							_ -> [<<"\n\tOAuth2 scope: ">>, Scope]
					   end,
					  <<"\n\tClient: ">>, case Client of
											public -> <<"public">>;
											undefined -> <<>>;
											_ -> [integer_to_binary(Client#client.id), <<" ">>, Client#client.name]
									   end,
					   <<"\n\tUser: ">>, case User of
										public -> <<"public">>;
										undefined -> <<>>;
										_ ->  [integer_to_binary(User#user.id), <<" ">>,  User#user.login]
									 end,
					   <<"\n\tNode: ">>, case Node of
										undefined -> <<>>;
										_ -> Node
									 end,
					   <<"\n\tFilename: ">>, case Filename of
											undefined -> <<>>;
											_ -> Filename
										 end,
					   <<"\n\tStatus: ">>, integer_to_binary(Code), 
					   <<" <<">>, case is_atom(Reason) of
										true -> atom_to_binary(Reason, utf8);
										false -> <<"error">>
								  end, <<">> (">>, integer_to_binary(Latency), <<"ms)\n}">>]),
					   
				NewState = case Code >= 400 of
								true  -> write_msg(error, Texto1, State#state{ult_reqhash = ReqHash});
								false -> write_msg(info, Texto1, State#state{ult_reqhash = ReqHash})
							end,
				NewState;
			false -> 
				ems_db:inc_counter(ems_logger_write_dup),
				State
		end
	catch 
		_:_ -> 
			ems_db:inc_counter(ems_logger_log_request_error),
			State
	end.
	

