%%********************************************************************
%% @title Module ems_http_worker
%% @version 1.0.0
%% @doc Module responsible for processing HTTP requests.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_worker).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Server API
-export([start/1, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

%% Client API
-export([cast/1]).

% State of server
-record(state, {owner 	  = undefined,	 	 	%% http listener
				lsocket   = undefined,		 	%% socket of listener
				socket	  = undefined,		 	%% socket of request
				tcp_config = undefined,  		%% range of IP addresses that can access the server
				listener_name = undefined
			}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Args) -> 
    gen_server:start_link(?MODULE, Args, []).
    
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).
    
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================

%% @doc Send message to worker
cast(Msg) -> ems_pool:cast(ems_http_worker_pool, Msg).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Owner, LSocket, TcpConfig, ListenerName}) ->
	process_flag(trap_exit, true),	
    State = #state{owner = Owner,
				   lsocket = LSocket, 
				   tcp_config = TcpConfig,
				   listener_name = ListenerName},
    {ok, State, 0};



%% init for processes that will process the queue of outgoing requests
init(_) ->
    {ok, #state{}}.

handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
	process_response(Msg),
	{noreply, State, 0}.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(timeout, State=#state{socket = undefined}) ->
	accept_request(State);

handle_info(timeout, State=#state{socket = Socket}) ->
	%io:format("Timeout enquanto aguarda service http\n"),
	ems_socket:close(Socket),
	accept_request(State);

handle_info(timeout, State) ->
	%io:format("Timeout com state ~p\n", [State]),
	accept_request(State);

handle_info({tcp, Socket, RequestBin}, State) ->
	process_request(Socket, RequestBin),
	{noreply, State, 6000};

handle_info({ssl, Socket, RequestBin}, State) ->
	process_request({ssl, Socket}, RequestBin),
	{noreply, State, 6000};

handle_info({tcp_closed, _Socket}, State) ->
	io:format("process tcp closed end\n"),
	erlang:yield(),
	erlang:yield(),
	erlang:yield(),
	{noreply, State#state{socket = undefined}, 0};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    io:format("process exit end\n"),
    {noreply, State};

handle_info(Msg, State) ->
	{noreply, State, 6000}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, #state{socket = undefined}) ->
   io:format("terminate\n"),
   ok;

terminate(_Reason, #state{socket = Socket}) ->
	ems_socket:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

accept_request(State=#state{owner = Owner,
							lsocket = LSocket, 
							tcp_config = #tcp_config{tcp_allowed_address = AllowedAddress, 
												     tcp_max_http_worker = _MaxHttpWorker,
												     tcp_min_http_worker = MinHttpWorker,
												     tcp_accept_timeout = AcceptTimeout},
							listener_name = ListenerName}) ->
	ems_db:sequence(ListenerName),
	case ems_socket:accept(LSocket, AcceptTimeout) of
		{ok, Socket} -> 
			CurrentWorkerCount = ems_db:sequence(ListenerName, -1),
			case ems_socket:peername(Socket) of
				{ok, {Ip,_Port}} -> 
					case CurrentWorkerCount == 0 of
						true -> gen_server:cast(Owner, new_worker);
						_ -> ok
					end,
					case Ip of
						{127, 0, _,_} -> 
							{noreply, State#state{socket = Socket}};
						_ -> 
							case ems_http_util:match_ip_address(AllowedAddress, Ip) of
								true -> 
									{noreply, State#state{socket = Socket}};
								false -> 
									ems_socket:close(Socket),
									ems_logger:warn("Host ~s not authorized!", [ems_socket:ntoa(Ip)]),
									accept_request(State)
							end
					end;
				_ -> 
					ems_socket:close(Socket),
					accept_request(State)
			end;
		{error, closed} -> 
			% ListenSocket is closed
			ems_db:sequence(ListenerName, -1),
			ems_logger:info("Listener socket was closed."),
			{stop, normal, State};
		{error, timeout} ->
			% no connection is established within the specified time
			ems_db:sequence(ListenerName, -1),
			%io:format("timeout current: ~p  Min: ~p\n", [ems_db:current_sequence(ListenerName), MinHttpWorker]),
			case ems_db:current_sequence(ListenerName) < MinHttpWorker of
				true -> accept_request(State);
				_ ->
					%io:format("Liberando um worker por timeout!\n"),
					{stop, normal, State}
			end;
		{error, PosixError} ->
			ems_db:sequence(ListenerName, -1),
			PosixErrorDescription = ems_socket:posix_error_description(PosixError),
			ems_logger:error("~p in http worker.", [PosixErrorDescription]),
			erlang:yield(),
			erlang:yield(),
			erlang:yield(),
			accept_request(State)
	end.


process_request(Socket, RequestBin) ->
	case ems_http_util:encode_request(Socket, RequestBin, self()) of
		 {ok, Request} -> 
			% Send request to service
			ems_dispatcher:dispatch_request(Request),
			erlang:yield(),
			% Settings for tcp
			ems_socket:setopts(Socket,[{active,once}]),
			% TCP_LINGER2 for Linux
			ems_socket:setopts(Socket,[{raw,6,8,<<30:32/native>>}]),
			% TCP_DEFER_ACCEPT for Linux
			ems_socket:setopts(Socket,[{raw, 6,9, << 30:32/native >>}]);
		 {error, Reason} -> 
			Response = ems_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400(atom_to_list(Reason)), <<"application/json; charset=utf-8"/utf8>>),
			ems_socket:send_data(Socket, Response),
			ems_socket:close(Socket),
			ems_logger:error("Invalid request: ~p.", [Reason])
	end.


process_response({_MsgType, Request = #request{type = Method, socket = Socket}, Result}) ->
	ems_socket:setopts(Socket,[{active,once}]),
	% TCP_LINGER2 for Linux
	ems_socket:setopts(Socket,[{raw,6,8,<<30:32/native>>}]),
	% TCP_DEFER_ACCEPT for Linux
	ems_socket:setopts(Socket,[{raw, 6,9, << 30:32/native >>}]),
	case Result of
		{ok, <<Content/binary>>} -> 
			{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
			Response = ems_http_util:encode_response(HttpCodeBin, Content),
			send_response(HttpCode, ok, Request, Response);
		{ok, <<Content/binary>>, <<MimeType/binary>>} ->
			{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
			Response = ems_http_util:encode_response(HttpCodeBin, Content, MimeType),
			send_response(HttpCode, ok, Request, Response);
		{error, Reason} ->
			{HttpCode, HttpCodeBin} = get_http_code_verb(Method, false),
			Response = ems_http_util:encode_response(HttpCodeBin, {error, Reason}),
			send_response(HttpCode, {error, Reason}, Request, Response);
		Content when is_map(Content) -> 
			{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
			Response = ems_http_util:encode_response(HttpCodeBin, ems_util:json_encode(Content)),
			send_response(HttpCode, ok, Request, Response);
		Content = [H|_] when is_map(H) -> 
			{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
			Response = ems_http_util:encode_response(HttpCodeBin, ems_util:json_encode(Content)),
			send_response(HttpCode, ok, Request, Response);
		Content = [H|_] when is_tuple(H) -> 
			{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
			Response = ems_http_util:encode_response(HttpCodeBin, ems_schema:to_json(Content)),
			send_response(HttpCode, ok, Request, Response);
		Content -> 
			{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
			Response = ems_http_util:encode_response(HttpCodeBin, Content),
			send_response(HttpCode, ok, Request, Response)
	end.

get_http_code_verb("POST", true)  -> {201, <<"201">>};
get_http_code_verb("PUT", false)  -> {400, <<"400">>};
get_http_code_verb(_, true)  -> {200, <<"200">>};
get_http_code_verb(_, false)  -> {400, <<"400">>}.

send_response(Code, Reason, Request, Response) ->
	T2 = ems_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	StatusSend = ems_socket:send_data(Request#request.socket, Response),
	ems_socket:close(Request#request.socket),
	case  StatusSend of
		ok -> Status = req_send;
		_  -> Status = req_done
	end,
	Request2 = Request#request{latency = Latencia, code = Code, reason = Reason, status_send = StatusSend, status = Status},
	ems_request:finaliza_request(Request2),
	case StatusSend of
		ok -> ems_eventmgr:notifica_evento(close_request, Request2);
		_  -> ems_eventmgr:notifica_evento(send_error_request, Request2)
	end.

	
