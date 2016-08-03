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
-export([start/0, start_link/1, stop/0]).

%% Client API
-export([error/1, error/2, 
		 info/1, info/2, 
		 warn/1, warn/2, 
		 debug/1, debug/2,
		 sync/0, 
		 log_request/1,
		 mode_debug/1,
		 set_level/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do ems_logger. 
-record(state, {buffer = [],             		% The messages go first to a buffer subsequently to the log file        
			    buffer_tela = [],        		% The messages go first to a buffer subsequently to screen
			    flag_checkpoint = false,      	% checkpoint to unload the buffer to the log file
			    flag_checkpoint_tela = false, 	% checkpoint to unload the screen buffer
				log_file_dest,           		% path configuration where the logs will be written
				log_file_checkpoint,      		% timeout configuration to unload file buffer
				log_file_name,		      		% timeout configuration to unload file buffer
				log_file_handle,				% IODevice of file
				debug,							% It indicates whether it is in debug mode
				level = info					% level of errors
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

mode_debug(Flag) ->
	gen_server:cast(?SERVER, {debug, Flag}). 

sync() ->
	gen_server:call(?SERVER, sync_buffer). 		

log_request(Request) -> 
	gen_server:cast(?SERVER, {log_request, Request}). 

set_level(Level) -> 
	gen_server:cast(?SERVER, {set_level, Level}). 


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
	Conf = ems_config:getConfig(),
	LogFileDest = Conf#config.log_file_dest,
	Checkpoint = Conf#config.log_file_checkpoint,
	{NomeArqLog, IODevice} = get_filename_logger(LogFileDest),
	set_timeout_for_get_filename_logger(),
    {ok, #state{log_file_dest = LogFileDest, 
                log_file_checkpoint = Checkpoint,
                log_file_name = NomeArqLog,
                log_file_handle = IODevice,
                debug = Conf#config.ems_debug}}. 
    
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

handle_info(checkpoint_get_filename_logger, 
			State = #state{log_file_dest = LogFileDest,
						   log_file_handle = IODevice}) ->
	file:close(IODevice),
	{NewLogFileName, IODevice2} = get_filename_logger(LogFileDest),
	set_timeout_for_get_filename_logger(),
	{noreply, State#state{log_file_name = NewLogFileName, log_file_handle = IODevice2}}.

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
    
get_filename_logger(LogFileDest) -> 
	{{Ano,Mes,Dia},{Hora,Min,_Seg}} = calendar:local_time(),
	NodeName = ems_util:get_node_name(),
	NomeArqLog = lists:flatten(io_lib:format("~s/~s/~p/~s/~s_~2..0w~2..0w~4..0w_~2..0w~2..0w.log", [LogFileDest, atom_to_list(node()), Ano, ems_util:mes_extenso(Mes), NodeName, Dia, Mes, Ano, Hora, Min])),
	filelib:ensure_dir(NomeArqLog),
	{ok, IODevice} = file:open(NomeArqLog, [append]),
	{NomeArqLog, IODevice}.

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
    
write_msg(info, Msg, State = #state{level = error}) ->
	set_timeout_for_sync_tela(State),
	State#state{buffer_tela = [Msg|State#state.buffer_tela], flag_checkpoint_tela = true};

write_msg(Tipo, Msg, State)  ->
	Msg1 = lists:concat([string:to_upper(atom_to_list(Tipo)), " ", ems_util:timestamp_str(), "  ", Msg]),
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

sync_buffer(State = #state{log_file_handle = IODevice, 
						   buffer = Buffer}) ->
	file:write(IODevice, lists:map(fun(L) -> 
											L ++ "\n" 
									end, lists:reverse(Buffer))),
	State#state{buffer = [], flag_checkpoint = false}.

do_log_request(#request{protocol = ldap, 
						rid = RID,
						type = Metodo,
						url = Url,
						version = Version,
						payload = Payload,
						service = Service,
						code = Code,
						reason = Reason,
						latency = Latencia,
						authorization = Authorization,
						node_exec = Node}, _State) ->
	case Service of
		undefined -> ServiceImpl = "";
		_ -> ServiceImpl = Service#service.service
	end,
	Texto =  "~s ~s ~s {\n\tRID: ~p\n\tPayload: ~p\n\tService: ~s\n\tAuthorization: ~s\n\tNode: ~s\n\tStatus: ~p <<~s>> (~pms)\n}",
	Texto1 = io_lib:format(Texto, [Metodo, Url, Version, RID, Payload, ServiceImpl, Authorization, Node, Code, Reason, Latencia]),
	case Code of
		200 -> ems_logger:info(Texto1);
		_ 	-> ems_logger:error(Texto1)
	end;
	
	
do_log_request(#request{protocol = http, 
						rid = RID,
						type = Metodo,
						url = Url,
						version = Version,
						accept = Accept,
						user_agent = User_Agent,
						payload = Payload,
						service = Service,
						querystring_map = Query,
						content_type = Content_Type,
						code = Code,
						reason = Reason,
						latency = Latencia,
						authorization = Authorization,
						node_exec = Node}, _State) ->
		case Service of
			undefined -> ServiceImpl = "";
			_ -> ServiceImpl = Service#service.service
		end,
		case Payload of
			undefined ->
				Texto =  "~s ~s ~s {\n\tRID: ~p\n\tAccept: ~s:\n\tUser-Agent: ~s\n\tService: ~s\n\tQuery: ~p\n\tAuthorization: ~s\n\tNode: ~s\n\tStatus: ~p <<~s>> (~pms)\n}",
				Texto1 = io_lib:format(Texto, [Metodo, Url, Version, RID, Accept, User_Agent, 
											   ServiceImpl, Query, Authorization, 
											   Node, Code, Reason, 
											   Latencia]);
			_ ->
				Texto =  "~s ~s ~s {\n\tRID: ~p\n\tAccept: ~s:\n\tUser-Agent: ~s\n\tContent-Type: ~s\n\tPayload: ~s\n\tService: ~s\n\tQuery: ~p\n\tAuthorization: ~s\n\tNode: ~s\n\tStatus: ~p <<~s>> (~pms)\n}",
				Texto1 = io_lib:format(Texto, [Metodo, Url, Version, RID, Accept, User_Agent, 
											   Content_Type, Payload, ServiceImpl, Query, 
											   Authorization, Node, Code, Reason, 
											   Latencia])
		end,
		case Code of
			200 -> ems_logger:info(Texto1);
			_ 	-> ems_logger:error(Texto1)
		end.

