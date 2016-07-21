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

% estado do servidor
-record(state, {owner 	  = undefined,	 	 %% http listener
				lsocket   = undefined,		 %% socket of listener
				socket	  = undefined,		 %% socket of request
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
cast(Msg) -> ems_pool:cast(ems_http_worker_pool, Msg).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Owner, LSocket, Allowed_Address}) ->
	process_flag(trap_exit, true),	
    State = #state{owner = Owner,
				   lsocket = LSocket, 
				   allowed_address = Allowed_Address},
    {ok, State, 0};



%% init for processes that will process the queue of outgoing requests
init(_) ->
    %fprof:trace([start, {procs, [self()]}]),
    {ok, #state{}}.

handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
	send_response(Msg),
	{stop, normal, State}.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(timeout, State=#state{lsocket = undefined}) ->
	{noreply, State};

handle_info(timeout, State) ->
	accept_request(timeout, State);

handle_info({tcp, Socket, RequestBin}, State) ->
	process_request(Socket, RequestBin),
	{noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
	{noreply, State#state{socket = undefined}};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, #state{socket = undefined}) ->
    ok;

terminate(_Reason, #state{socket = Socket}) ->
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
	


accept_request(timeout, State=#state{owner = Owner, 
									 lsocket = LSocket, 
									 allowed_address = Allowed_Address}) ->
	case gen_tcp:accept(LSocket, ?TCP_ACCEPT_CONNECT_TIMEOUT) of
		{ok, Socket} -> 
			% back to listen to the door as quickly as possible
			gen_server:cast(Owner, new_worker),
			
			%% It is in the range of IP addresses authorized to access the bus?
			case inet:peername(Socket) of
				{ok, {Ip,_Port}} -> 
					case Ip of
						{127, 0, _,_} -> 
							{noreply, State#state{socket = Socket}};
						_ -> 
							case ems_http_util:match_ip_address(Allowed_Address, Ip) of
								true -> 
									{noreply, State#state{socket = Socket}};
								false -> 
									gen_tcp:close(Socket),
									ems_logger:warn("Host ~s not authorized!", [inet:ntoa(Ip)]),
									{stop, normal, State}
							end
					end;
				_ -> 
					gen_tcp:close(Socket),
					{stop, normal, State}
			end;
		{error, closed} -> 
			% ListenSocket is closed
			ems_logger:info("Listener socket was closed."),
			{noreply, State#state{lsocket = undefined}}; %% stop accept
		{error, timeout} ->
			% no connection is established within the specified time
			%close_timeout_connections(State),
			{noreply, State#state{open_requests = []}};
		{error, PosixError} ->
			PosixErrorDescription = ems_tcp_util:posix_error_description(PosixError),
			ems_logger:error("~p in http worker.", [PosixErrorDescription]),
			{noreply, State}
	end.


process_request(Socket, RequestBin) ->
	case ems_http_util:encode_request(Socket, RequestBin, self()) of
		 {ok, Request} -> 
			inet:setopts(Socket,[{active,once}]),
			% TCP_LINGER2 for Linux
			inet:setopts(Socket,[{raw,6,8,<<30:32/native>>}]),
			% TCP_DEFER_ACCEPT for Linux
			inet:setopts(Socket,[{raw, 6,9, << 30:32/native >>}]),
			ems_logger:info("Dispatch new request: ~p.", [Request]),
			ems_dispatcher:dispatch_request(Request);
		 {error, Request, Reason} -> 
			envia_response(Request, {error, Reason});
		 {error, invalid_http_header} -> 
			gen_tcp:close(Socket),
			ems_logger:error("Invalid HTTP request, close socket.")
	end.


send_response({HttpCode, Request, Result}) ->
	Socket = Request#request.socket,
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
			Response = ems_http_util:encode_response(<<"200">>, <<>>),
			envia_response(Code, ok, Request, Response);
		{ok, Content} -> 
			Response = ems_http_util:encode_response(CodeBin, Content),
			case Status of
				error -> envia_response(Code, Content, Request, Response);
				_-> envia_response(Code, ok, Request, Response)
			end;
		{ok, Content, MimeType} -> 
			Response = ems_http_util:encode_response(CodeBin, Content, MimeType),
			envia_response(Code, Status, Request, Response);
		{error, _Reason} -> 
			envia_response(Request, Result);
		{error, _Reason, _Motivo} -> 
			envia_response(Request, Result);
		_ ->
			envia_response(Request, Result)
	end.


envia_response(_Request, {async, false}) -> 
	em_andamento;

envia_response(Request, {async, true}) ->
	RID = ems_http_util:rid_to_string(Request#request.rid),
	Ticket = iolist_to_binary([<<"{\"ticket\":\"">>, RID, "\"}"]),
	Response = ems_http_util:encode_response(<<"200">>, Ticket),
	envia_response(200, ok, Request, Response);

envia_response(Request, {ok, Result}) -> 
	case Request#request.service#service.async of
		true -> io:format("Ticket já foi entregue.\n");
		_ -> 
			Response = ems_http_util:encode_response(<<"200">>, Result),
			envia_response(200, ok, Request, Response)
	end;

envia_response(Request, {ok, Result, MimeType}) ->
	Response = ems_http_util:encode_response(<<"200">>, Result, MimeType),
	envia_response(200, ok, Request, Response);

envia_response(Request, {error, notfound}) ->
	Response = ems_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404),
	envia_response(404, notfound, Request, Response);

envia_response(Request, {error, no_authorization}) ->
	Response = ems_http_util:encode_response(<<"401">>, ?HTTP_ERROR_401),
	envia_response(401, no_authorization, Request, Response);

envia_response(Request, {error, invalid_payload}) ->
	Response = ems_http_util:encode_response(<<"415">>, ?HTTP_ERROR_415),
	envia_response(415, invalid_payload, Request, Response);

envia_response(Request, {error, file_not_found}) ->
	Response = ems_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404_FILE_NOT_FOUND),
	envia_response(404, file_not_found, Request, Response);

envia_response(Request, {error, Reason}) ->
	Response = ems_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
	envia_response(400, Reason, Request, Response);

envia_response(Request, {error, service_fora, ErroInterno}) ->
	Response = ems_http_util:encode_response(<<"503">>, ?HTTP_ERROR_503),
	Reason2 = io_lib:format("~p ~p", [service_fora, ErroInterno]),
	envia_response(503, Reason2, Request, Response);

envia_response(Request, {error, Reason, ErroInterno}) ->
	Reason2 = io_lib:format("~p ~p", [Reason, ErroInterno]),
	envia_response(Request, {error, Reason2});

envia_response(Request, Result) ->
	envia_response(Request, {ok, Result}).

envia_response(Code, Reason, Request, Response) ->
	T2 = ems_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	StatusSend = ems_tcp_util:send_data(Request#request.socket, Response),
	gen_tcp:close(Request#request.socket),
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
	
