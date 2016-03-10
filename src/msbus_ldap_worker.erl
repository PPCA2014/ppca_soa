%%********************************************************************
%% @title Module msbus_ldap_worker
%% @version 1.0.0
%% @doc Module responsible for processing LDAP requests.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(msbus_ldap_worker).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include("../include/msbus_http_messages.hrl").
-include("../include/LDAP.hrl").

%% Server API
-export([start/1, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

%% Client API
-export([cast/1]).

% estado do servidor
-record(state, {worker_id = undefined,  	 %% identifier worker
				lsocket   = undefined,		 %% socket ofo listener
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
cast(Msg) -> msbus_pool:cast(msbus_ldap_worker_pool, Msg).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Worker_Id, LSocket, Allowed_Address}) ->
    State = #state{worker_id = Worker_Id, 
				   lsocket = LSocket, 
				   allowed_address = Allowed_Address,
				   open_requests=[]},
    {ok, State, 0};

%% init for processes that will process the queue of outgoing requests
init(_Args) ->
    %fprof:trace([start, {procs, [self()]}]),
    {ok, #state{}}.

handle_cast(shutdown, State) ->
    msbus_logger:debug("LDAP worker ~p shutdown com state ~p.", [self(), State]),
    {stop, normal, State};

%% It is not being used
handle_cast({Socket, RequestBin}, State) ->
	NewState = trata_request(Socket, RequestBin, State),
	{noreply, NewState, 0};

%% Handle respose	
handle_cast({_StatusCode, Request, Result}, State) ->
	Socket = Request#request.socket,
	MessageID = Request#request.payload#'LDAPMessage'.messageID,
	inet:setopts(Socket,[{active,once}]),
	% TCP_LINGER2 for Linux
	inet:setopts(Socket,[{raw,6,8,<<30:32/native>>}]),
	% TCP_DEFER_ACCEPT for Linux
	inet:setopts(Socket,[{raw, 6,9, << 30:32/native >>}]),
	case Result of
		{ok, unbindRequest} ->
			msbus_tcp_util:send_data(Socket, [<<>>]),
			gen_tcp:close(Socket),
			finaliza_request(Request);
		{ok, Msg} -> 
			Response = lists:map(fun(M) -> msbus_ldap_util:encode_response(MessageID, M) end, Msg),
			msbus_tcp_util:send_data(Socket, Response)
	end,
	{noreply, State#state{socket=undefined}}.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(timeout, State=#state{lsocket = undefined}) ->
	{noreply, State};

handle_info({tcp, Socket, RequestBin}, State) ->
	NewState = trata_request(Socket, RequestBin, State),
	{noreply, NewState, 0};

handle_info({tcp_closed, _Socket}, State) ->
	{noreply, State#state{socket = undefined}};

%% Handle ldap requests
handle_info(timeout, State=#state{lsocket = LSocket, allowed_address=Allowed_Address}) ->
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
			msbus_logger:info("Socket do listener foi fechado."),
			{noreply, State#state{lsocket = undefined}}; %% para de fazer accept
		{error, timeout} ->
			% no connection is established within the specified time
			msbus_logger:info("Check pending connections to the ldap server socket ~p.", [State#state.worker_id]),
			%close_timeout_connections(State),
			{noreply, State#state{open_requests = []}, 0};
		{error, system_limit} ->
			msbus_logger:error("No available ports in the Erlang emulator for ldap worker ~p. System_limit: ~p.", [State#state.worker_id, system_limit]),
			msbus_util:sleep(3000),
			{noreply, State, 0};
		{error, PosixError} ->
			msbus_logger:error("Erro POSIX ~p in ldap worker ~p.", [PosixError, State#state.worker_id]),
			msbus_util:sleep(3000),
			{noreply, State, 0}
	end;

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, #state{socket = undefined}) ->
    ok;

terminate(Reason, #state{worker_id = Worker_id, socket = Socket}) ->
	msbus_logger:debug("Terminate ldap worker ~p. Reason: ~p.", [Worker_id, Reason]),
	gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

	%% @doc Treats ldap request
trata_request(Socket, RequestBin, State) -> 
	Worker = msbus_pool:checkout(msbus_ldap_worker_pool),
	case msbus_ldap_util:encode_request(Socket, RequestBin, Worker) of
		 {ok, Request} -> 
			case gen_tcp:controlling_process(Socket, Worker) of
				ok -> 
					inet:setopts(Socket,[{active,once}]),
					% TCP_LINGER2 for Linux
					inet:setopts(Socket,[{raw,6,8,<<30:32/native>>}]),
					% TCP_DEFER_ACCEPT for Linux
					inet:setopts(Socket,[{raw, 6,9, << 30:32/native >>}]),
					msbus_dispatcher:dispatch_request(Request),
					NewState = State#state{socket = undefined, 
										   open_requests = [Request | State#state.open_requests]};
				{error, closed} -> 
					NewState = State#state{socket=undefined};
				{error, not_owner} -> 
					msbus_logger:error("ldap worker ~p is not owner of socket.", [Worker]),
					NewState = State#state{socket=undefined};
				{error, PosixError} ->
					gen_tcp:close(Socket),
					msbus_logger:error("Erro POSIX ~p in ldap worker ~p.", [PosixError, State#state.worker_id]),
					NewState = State#state{socket=undefined}
			end;
		 {error, Reason} -> 
			gen_tcp:close(Socket),
			msbus_logger:error("Invalid LDAP request. Reason: ~p.", [Reason]),
			NewState = State#state{socket = undefined}
	end,
	msbus_pool:checkin(msbus_ldap_worker_pool, Worker),
	NewState.

finaliza_request(Request) ->
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	Request2 = Request#request{latencia = Latencia, code = 200, reason = ok, status_send = ok, status = req_concluido},
	msbus_request:finaliza_request(Request2),
	msbus_eventmgr:notifica_evento(close_request, Request2).
