%%********************************************************************
%% @title Module msbus_http_worker
%% @version 1.0.0
%% @doc Module responsible for processing HTTP requests.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(msbus_http_worker).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include("../include/msbus_http_messages.hrl").

%% Server API
-export([start/1, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

%% Client API
-export([cast/1]).

% estado do servidor
-record(state, {worker_id = undefined,  	 %% identifier worker
				lsocket   = undefined,		 %% socket oo listener
				socket	  = undefined,		 %% socket oo request
				allowed_address = undefined, %% range of IP addresses that can access the server
				open_requests = []
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
cast(Msg) -> msbus_pool:cast(msbus_http_worker_pool, Msg).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Worker_Id, LSocket, Allowed_Address}=Args) ->
    msbus_logger:debug("HTTP worker ~p init. Args: ~p.", [self(), Args]),
    State = #state{worker_id = Worker_Id, 
				   lsocket = LSocket, 
				   allowed_address = Allowed_Address,
				   open_requests=[]},
    {ok, State, 0};

%% init for processes that will process the queue of outgoing requests
init(Args) ->
    %fprof:trace([start, {procs, [self()]}]),
    msbus_logger:debug("HTTP worker ~p init. Args: ~p.", [self(), Args]),
    {ok, #state{}}.

handle_cast(shutdown, State) ->
    msbus_logger:debug("HTTP worker ~p shutdown in state ~p.", [self(), State]),
    {stop, normal, State};

%% It is not being used
handle_cast({Socket, RequestBin}, State) ->
	NewState = trata_request(Socket, RequestBin, State),
	{noreply, NewState, 0};
	
handle_cast({HttpCode, Request, Result}, State) ->
	Worker = self(),
	Socket = Request#request.socket,
	msbus_logger:debug("Init send response in ~p.", [Worker]),
	inet:setopts(Socket,[{active,once}]),
	% TCP_LINGER2 for Linux
	inet:setopts(Socket,[{raw,6,8,<<30:32/native>>}]),
	% TCP_DEFER_ACCEPT for Linux
	inet:setopts(Socket,[{raw, 6,9, << 30:32/native >>}]),
	case is_integer(HttpCode) of
		true -> Code = HttpCode;
		false -> Code = 200
	end,
	case Code of 
		200 -> Status = ok;
		201 -> Status = ok;
		400 -> Status = error;
		404 -> Status = error;
		_ ->
			case Code >= 400 of
				true -> Status = error;
				false -> Status = ok
			end
	end,
	CodeBin = integer_to_binary(Code), 
	case Result of
		<<>> -> 
			Response = msbus_http_util:encode_response(<<"200">>, <<>>),
			envia_response(Code, ok, Request, Response);
		{ok, Content} -> 
			Response = msbus_http_util:encode_response(CodeBin, Content),
			case Status of
				error -> envia_response(Code, Content, Request, Response);
				_-> envia_response(Code, ok, Request, Response)
			end;
		{ok, Content, MimeType} -> 
			Response = msbus_http_util:encode_response(CodeBin, Content, MimeType),
			envia_response(Code, Status, Request, Response);
		{error, _Reason} -> 
			envia_response(Request, Result, State);
		{error, _Reason, _Motivo} -> 
			envia_response(Request, Result, State);
		_ ->
			envia_response(Request, Result, State)
	end,
	msbus_logger:debug("Finish send response in ~p.", [Worker]),
	{noreply, State#state{socket=undefined}}.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(timeout, State=#state{lsocket = undefined}) ->
	{noreply, State};

handle_info(timeout, State=#state{lsocket = LSocket, allowed_address=Allowed_Address}) ->
    msbus_logger:debug("Listen for accept in http server worker ~p.", [State#state.worker_id]),
	case gen_tcp:accept(LSocket, ?TCP_ACCEPT_CONNECT_TIMEOUT) of
		{ok, Socket} -> 
			case inet:peername(Socket) of
				{ok, {Ip,_Port}} -> 
					case Ip of
						{127, 0, _,_} -> 
							{noreply, State#state{socket = Socket}};
						_ -> 
							%% It is in the range of IP addresses authorized to access the bus?
							case msbus_http_util:match_ip_address(Allowed_Address, Ip) of
								true -> 
									{noreply, State#state{socket = Socket}};
								false -> 
									msbus_logger:warn("Host ~s not authorized!", [inet:ntoa(Ip)]),
									gen_tcp:close(Socket),
									{noreply, State, 0}
							end
					end;
				_ -> 
					gen_tcp:close(Socket),
					{noreply, State, 0}
			end;
		{error, closed} -> 
			% ListenSocket is closed
			msbus_logger:info("Listener socket was closed."),
			{noreply, State#state{lsocket = undefined}}; %% para de fazer accept
		{error, timeout} ->
			% no connection is established within the specified time
			msbus_logger:info("Check pending connections to the server socket ~p.", [State#state.worker_id]),
			%close_timeout_connections(State),
			{noreply, State#state{open_requests = []}, 0};
		{error, system_limit} ->
			msbus_logger:error("No available ports in the Erlang emulator for worker ~p. System_limit: ~p.", [State#state.worker_id, system_limit]),
			msbus_util:sleep(3000),
			{noreply, State, 0};
		{error, PosixError} ->
			msbus_logger:error("Erro POSIX ~p in http worker ~p.", [PosixError, State#state.worker_id]),
			msbus_util:sleep(3000),
			{noreply, State, 0}
	end;

handle_info({tcp, Socket, RequestBin}, State) ->
	NewState = trata_request(Socket, RequestBin, State),
	{noreply, NewState, 0};

handle_info({tcp_closed, _Socket}, State) ->
	{noreply, State#state{socket = undefined}};

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, #state{socket = undefined}) ->
    ok;

terminate(Reason, #state{worker_id = Worker_id, socket = Socket}) ->
	msbus_logger:debug("Terminate server worker ~p. Reason: ~p.", [Worker_id, Reason]),
	gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

%%close_timeout_connections(#state{open_requests = Open_requests}) ->
%%	lists:foreach(fun(R) -> 
%%						case gen_tcp:controlling_process(R#request.socket, self()) of
%%							ok -> gen_tcp:close(R#request.socket);
%%							_ -> ok
%%						end
%%				  end, Open_requests).
	

%% @doc Trata o request
trata_request(Socket, RequestBin, State) -> 
	Worker = msbus_pool:checkout(msbus_http_worker_pool),
	case msbus_http_util:encode_request(Socket, RequestBin, Worker) of
		 {ok, Request} -> 
			case gen_tcp:controlling_process(Socket, Worker) of
				ok -> 
					inet:setopts(Socket,[{active,once}]),
					% TCP_LINGER2 for Linux
					inet:setopts(Socket,[{raw,6,8,<<30:32/native>>}]),
					% TCP_DEFER_ACCEPT for Linux
					inet:setopts(Socket,[{raw, 6,9, << 30:32/native >>}]),
					msbus_logger:debug("Dispatch new request: ~p.", [Request]),
					msbus_dispatcher:dispatch_request(Request),
					NewState = State#state{socket = undefined, 
										   open_requests = [Request | State#state.open_requests]};
				{error, closed} -> 
					NewState = State#state{socket=undefined};
				{error, not_owner} -> 
					msbus_logger:error("Server worker ~p is not owner of socket.", [Worker]),
					NewState = State#state{socket=undefined};
				{error, PosixError} ->
					gen_tcp:close(Socket),
					msbus_logger:error("Erro POSIX ~p in http worker ~p.", [PosixError, State#state.worker_id]),
					NewState = State#state{socket=undefined}
			end;
		 {error, Request, Reason} -> 
			envia_response(Request, {error, Reason}, State),
			NewState = State#state{socket = undefined};
		 {error, invalid_http_header} -> 
			gen_tcp:close(Socket),
			msbus_logger:error("Invalid HTTP request, close socket."),
			NewState = State#state{socket = undefined}
	end,
	msbus_pool:checkin(msbus_http_worker_pool, Worker),
	NewState.

envia_response(_Request, {async, false}, _State) -> 
	em_andamento;

envia_response(Request, {async, true}, _State) ->
	RID = msbus_http_util:rid_to_string(Request#request.rid),
	Ticket = iolist_to_binary([<<"{\"ticket\":\"">>, RID, "\"}"]),
	Response = msbus_http_util:encode_response(<<"200">>, Ticket),
	envia_response(200, ok, Request, Response);

envia_response(Request, {ok, Result}, _State) -> 
	case Request#request.servico#servico.async of
		true -> io:format("Ticket já foi entregue.\n");
		_ -> 
			Response = msbus_http_util:encode_response(<<"200">>, Result),
			envia_response(200, ok, Request, Response)
	end;

envia_response(Request, {ok, Result, MimeType}, _State) ->
	Response = msbus_http_util:encode_response(<<"200">>, Result, MimeType),
	envia_response(200, ok, Request, Response);

envia_response(Request, {error, notfound}, _State) ->
	Response = msbus_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404),
	envia_response(404, notfound, Request, Response);

envia_response(Request, {error, no_authorization}, _State) ->
	Response = msbus_http_util:encode_response(<<"401">>, ?HTTP_ERROR_401),
	envia_response(401, no_authorization, Request, Response);

envia_response(Request, {error, invalid_payload}, _State) ->
	Response = msbus_http_util:encode_response(<<"415">>, ?HTTP_ERROR_415),
	envia_response(415, invalid_payload, Request, Response);

envia_response(Request, {error, file_not_found}, _State) ->
	Response = msbus_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404_FILE_NOT_FOUND),
	envia_response(404, file_not_found, Request, Response);

envia_response(Request, {error, Reason}, _State) ->
	Response = msbus_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
	envia_response(400, Reason, Request, Response);

envia_response(Request, {error, servico_fora, ErroInterno}, _State) ->
	Response = msbus_http_util:encode_response(<<"503">>, ?HTTP_ERROR_503),
	Reason2 = io_lib:format("~p ~p", [servico_fora, ErroInterno]),
	envia_response(503, Reason2, Request, Response);

envia_response(Request, {error, Reason, ErroInterno}, State) ->
	Reason2 = io_lib:format("~p ~p", [Reason, ErroInterno]),
	envia_response(Request, {error, Reason2}, State);

envia_response(Request, Result, State) ->
	envia_response(Request, {ok, Result}, State).

envia_response(Code, Reason, Request, Response) ->
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	case  StatusSend of
		ok -> Status = req_entregue;
		_  -> Status = req_concluido
	end,
	Request2 = Request#request{latencia = Latencia, code = Code, reason = Reason, status_send = StatusSend, status = Status},
	msbus_request:finaliza_request(Request2),
	case StatusSend of
		ok -> msbus_eventmgr:notifica_evento(close_request, Request2);
		_  -> msbus_eventmgr:notifica_evento(send_error_request, Request2)
	end.
	
