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
		 debug/1, debug/2,
		 sync/0, 
		 log_request/1,
		 mode_debug/1,
		 set_level/1,
		 show_response/1,
		 format_warn/1, 
		 format_warn/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do ems_logger. 
-record(state, {buffer = [],             		% The messages go first to a buffer subsequently to the log file        
			    buffer_tela = [],        		% The messages go first to a buffer subsequently to screen
			    flag_checkpoint = false,      	% checkpoint to unload the buffer to the log file
			    flag_checkpoint_tela = false, 	% checkpoint to unload the screen buffer
				log_file_checkpoint,      		% timeout configuration to unload file buffer
				log_file_name,		      		% timeout configuration to unload file buffer
				log_file_handle,				% IODevice of file
				debug = false,					% It indicates whether it is in debug mode
				level = info,					% level of errors
				show_response = false			% show response of request
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
	gen_server:cast(?SERVER, {write_msg, debug, Msg}).

debug(Msg, Params) -> 
	gen_server:cast(?SERVER, {write_msg, debug, Msg, Params}). 

mode_debug(Flag) ->
	gen_server:cast(?SERVER, {debug, Flag}). 

sync() ->
	gen_server:call(?SERVER, sync_buffer). 		

log_request(Request) -> 
	gen_server:cast(?SERVER, {log_request, Request}). 

set_level(Level) -> 
	gen_server:cast(?SERVER, {set_level, Level}). 

show_response(Value) -> 
	gen_server:cast(?SERVER, {show_response, Value}). 


format_warn(Message) ->	io:format("\033[0;33m~s\033[0m", [Message]).
format_warn(Message, Params) ->	io:format("\033[0;33m~s\033[0m", [io_lib:format(Message, Params)]).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{properties = Props}) ->
	Checkpoint = maps:get(<<"log_file_checkpoint">>, Props, ?LOG_FILE_CHECKPOINT),
	Debug = maps:get(<<"debug">>, Props, false),
	case get_filename_device() of
		{ok, NomeArqLog, IODevice} ->
			info("Loading server ~s...", [?SERVER_NAME]),
			set_timeout_for_get_filename_device(),
			{ok, #state{log_file_checkpoint = Checkpoint,
						log_file_name = NomeArqLog,
						log_file_handle = IODevice,
						debug = Debug}};
		{error, Reason} -> 
			io:format("Processo ems_logger falhou ao iniciar: ~p.", [Reason]),
			{stop, Reason}
	end.
    
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

handle_cast({set_level, Level}, State) ->
	{noreply, State#state{level = Level}};

handle_cast({show_response, Value}, State) ->
	{noreply, State#state{show_response = Value}};

handle_cast({ems_debug, Flag}, State) ->
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

handle_info(checkpoint_get_filename_device, State = #state{log_file_handle = IODevice, 
														   log_file_name = CurLogFileName}) ->
	file:close(IODevice),
	case get_filename_device() of
		{ok, NewLogFileName, IODevice2} ->
			set_timeout_for_get_filename_device(),
			{noreply, State#state{log_file_name = NewLogFileName, log_file_handle = IODevice2}};
		_Error ->
			ems_logger:error("Não foi possível substituir arquivo de log ~p. Ele poderá crescer indefinidamente!", [CurLogFileName]),
			{noreply, State}
	end.

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
    
get_filename_device() -> 
	{{Ano,Mes,Dia},{Hora,Min,_Seg}} = calendar:local_time(),
	NodeName = ems_util:get_node_name(),
	NomeArqLog = lists:flatten(io_lib:format("~s/~s/~p/~s/~s_~2..0w~2..0w~4..0w_~2..0w~2..0w.log", [?LOG_PATH, atom_to_list(node()), Ano, ems_util:mes_extenso(Mes), NodeName, Dia, Mes, Ano, Hora, Min])),
	case filelib:ensure_dir(NomeArqLog) of
		ok ->
			case file:open(NomeArqLog, [append]) of
				{ok, IODevice} -> {ok, NomeArqLog, IODevice};
				Error -> Error
			end;
		{error, Reason} = Error -> 
			io:format("Falhou ao abrir arquivo de log ~p: ~p.", [NomeArqLog, Reason]),
			Error
	end.

set_timeout_for_sync_buffer(#state{flag_checkpoint = false, log_file_checkpoint=Timeout}) ->    
	erlang:send_after(Timeout, self(), checkpoint);

set_timeout_for_sync_buffer(_State) ->    
	ok.

set_timeout_for_sync_tela(#state{flag_checkpoint_tela = false}) ->    
	erlang:send_after(2000, self(), checkpoint_tela);

set_timeout_for_sync_tela(_State) ->    
	ok.

set_timeout_for_get_filename_device() ->    
	erlang:send_after(?LOG_ARCHIVE_CHECKPOINT, self(), checkpoint_get_filename_device).

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
								flag_checkpoint = true};
				false ->
					set_timeout_for_sync_buffer(State),
					set_timeout_for_sync_tela(State),
					State#state{buffer = [Msg1|State#state.buffer], 
								buffer_tela = [Msg1|State#state.buffer_tela], 
								flag_checkpoint = true, 
								flag_checkpoint_tela = true}
			end;
		false -> State
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

sync_buffer(State = #state{log_file_handle = IODevice, 
						   buffer = Buffer}) ->
	Msg = [ [L | ["\n"]] || L <- lists:reverse(Buffer)],
	file:write(IODevice, Msg),
	State#state{buffer = [], flag_checkpoint = false}.

do_log_request(#request{protocol = ldap, 
						type = Metodo,
						url = Url,
						version = Version,
						payload = Payload,
						service = Service,
						code = Code,
						reason = Reason,
						latency = Latency,
						authorization = Authorization,
						node_exec = Node}, _State) ->
	ServiceImpl = case Service of
		undefined -> "";
		_ -> Service#service.service
	end,
	Texto =  "~s ~s ~s {\n\tPayload: ~p\n\tService: ~s\n\tAuthorization: ~s\n\tNode: ~s\n\tStatus: ~p <<~p>> (~pms)\n}",
	Texto1 = io_lib:format(Texto, [Metodo, Url, Version, Payload, ServiceImpl, Authorization, Node, Code, Reason, Latency]),
	case Code of
		200 -> ems_logger:info(Texto1);
		_ 	-> ems_logger:error(Texto1)
	end;
	
	
do_log_request(#request{rid = RID,
						req_hash = ReqHash,
						protocol = http, 
						type = Metodo,
						url = Url,
						version = Version,
						accept = Accept,
						host = Host,
						payload_map = Payload,
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
						user = User,
						cache_control = CacheControl,
						etag = Etag,
						if_modified_since = IfModifiedSince,
						if_none_match = IfNoneMatch,
						node_exec = Node
						}, 
			  #state{show_response = ShowResponse}) ->
	Texto =  "~s ~s ~s {\n\tRID: ~p  (ReqHash: ~p)\n\tAccept: ~p\n\tHost: ~p\n\tService: ~p\n\tParams: ~p\n\tQuery: ~p\n\tPayload: ~p\n\t~sResult-Cache: ~s\n\tCache-Control: ~p\n\tETag: ~p\n\tIf-Modified-Since: ~p\n\tIf-None-Match: ~p\n\tAuthorization: ~p   User: ~p\n\tNode: ~s\n\tStatus: ~p <<~p>> (~pms)\n}",
	Texto1 = io_lib:format(Texto, [Metodo, 
								   Url, 
								   Version, 
								   RID,
								   ReqHash,
								   Accept, 
								   Host, 
								   case Service of undefined -> <<>>; _ -> Service#service.service end,
								   Params,
								   Query, 
								   Payload, 
								   case ShowResponse of true -> io_lib:format("Response: ~p\n\t", [ResponseData]); false -> <<>> end,
								   case ResultCache of true -> io_lib:format("true  <<RID: ~s>>", [integer_to_list(ResultCacheRid)]); false -> "false" end,
								   CacheControl,
								   Etag,
								   IfModifiedSince,
								   IfNoneMatch,
								   Authorization,
								   User,
								   Node, 
								   Code, 
								   Reason, 
								   Latency]),
	case Code >= 400 of
		true  -> ems_logger:error(Texto1);
		false -> ems_logger:info(Texto1)
	end.

