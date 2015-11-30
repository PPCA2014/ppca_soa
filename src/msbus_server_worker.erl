%%********************************************************************
%% @title Módulo msbus_server_worker
%% @version 1.0.0
%% @doc Módulo responsável pelo processamento das requisições HTTP.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(msbus_server_worker).

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
-record(state, {worker_id = undefined,  %% identificador do worker
				lsocket   = undefined,	%% socket do listener
				socket	  = undefined,	%% socket da requisição do cliente
				allowed_address = undefined %% faixa de IPs permitidos que o servidor aceita
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

%% @doc Obtém os serviços mais usados
cast(Msg) -> msbus_pool:cast(msbus_server_worker_pool, Msg).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Worker_Id, LSocket, Allowed_Address}=Args) ->
    msbus_logger:debug("Server worker ~p init. Args: ~p.", [self(), Args]),
    State = #state{worker_id = Worker_Id, 
				   lsocket=LSocket, 
				   allowed_address=Allowed_Address},
    {ok, State, 0};

%% init para processos que vão processar a fila de requisições de saída
init(Args) ->
    %fprof:trace([start, {procs, [self()]}]),
    msbus_logger:debug("Server worker ~p init. Args: ~p.", [self(), Args]),
    {ok, #state{}}.

handle_cast(shutdown, State) ->
    msbus_logger:debug("Server worker ~p shutdown com state ~p.", [self(), State]),
    {stop, normal, State};

handle_cast({Socket, RequestBin}, State) ->
	msbus_logger:debug("Init dispatch da requisição no client worker ~p.", [self()]),
	trata_request(Socket, RequestBin, State),
	msbus_logger:debug("Finish dispatch da requisição no client worker ~p.", [self()]),
	{noreply, State#state{socket = undefined}};
	
handle_cast({static_file, Request, Result}, State) ->
	msbus_logger:debug("Init envio do static file response no client worker ~p.", [self()]),
	envia_response(Request, Result, State),
	msbus_logger:debug("Finish envio do static file response no client worker ~p.", [self()]),
	{noreply, State#state{socket = undefined}};


handle_cast({servico, Request, Result}, State) ->
	msbus_logger:debug("Init envio do service response no client worker ~p.", [self()]),
	envia_response(Request, Result, State),
	msbus_logger:debug("Finish envio do service response no client worker ~p.", [self()]),
	{noreply, State#state{socket = undefined}}.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(timeout, State=#state{lsocket = undefined}) ->
	{noreply, State};

handle_info(timeout, State=#state{lsocket = LSocket, allowed_address=Allowed_Address}) ->
    msbus_logger:debug("Listen for accept em server worker ~p.", [State#state.worker_id]),
	case gen_tcp:accept(LSocket, ?TCP_ACCEPT_CONNECT_TIMEOUT) of
		{ok, Socket} -> 
			% connection is established
			msbus_logger:debug("Conexão estabelecida para server worker ~p.", [State#state.worker_id]),
			case inet:peername(Socket) of
				{ok, {Ip,_Port}} -> 
					case Ip of
						{127, 0, _,_} -> 
							{noreply, State#state{socket = Socket}};
						_ -> 
							%% Está na faixa de IPs autorizado a acessar o barramento?
							msbus_logger:debug("Check se ip autorizado: ~p", [Ip]),
							case msbus_http_util:match_ip_address(Allowed_Address, Ip) of
								true -> 
									{noreply, State#state{socket = Socket}};
								false -> 
									msbus_logger:warn("Host ~s não autorizado!", [inet:ntoa(Ip)]),
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
			msbus_logger:info("Socket do listener foi fechado para o server worker ~p.", [State#state.worker_id]),
			{noreply, State#state{lsocket = undefined, socket = undefined}};
		{error, timeout} ->
			% no connection is established within the specified time
			msbus_logger:info("Nenhuma conexão estabelecida durante ~p para o server socket ~p.", [timeout, State#state.worker_id]),
			{noreply, State, 0};
		{error, system_limit} ->
			msbus_logger:error("No available ports in the Erlang emulator are in use for server worker ~p. System_limit: ~p.", [State#state.worker_id, system_limit]),
			msbus_util:sleep(3000),
			{noreply, State, 0};
		{error, PosixError} ->
			msbus_logger:error("Erro POSIX ~p ao tentar aceitar conexões no server worker ~p.", [PosixError, State#state.worker_id]),
			msbus_util:sleep(3000),
			{noreply, State, 0}
	end;

handle_info({tcp, Socket, RequestBin}, State) ->
	try
		msbus_pool:transaction(msbus_server_worker_pool, 
			fun(Worker) ->
				msbus_logger:debug("Init gen_tcp:controlling_process server worker pool ~p.", [Worker]),
				case gen_tcp:controlling_process(Socket, Worker) of
					ok -> 
						inet:setopts(Socket,[{active,once}]),
						% TCP_LINGER2 for Linux
						inet:setopts(Socket,[{raw,6,8,<<30:32/native>>}]),
						% TCP_DEFER_ACCEPT for Linux
						inet:setopts(Socket,[{raw, 6,9, << 30:32/native >>}]),
						gen_server:cast(Worker, {Socket, RequestBin}),
						msbus_logger:debug("Finish gen_tcp:controlling_process server worker pool ~p.", [Worker]),
						{noreply, State#state{socket=undefined}, 0};
					{error, closed} -> 
						msbus_logger:error("Socket fechado durante gen_tcp:controlling_process em server worker ~p.", [State#state.worker_id]),
						{noreply, State#state{socket=undefined}, 0};
					{error, not_owner} -> 
						msbus_logger:error("Server worker ~p não é o dono do socket.", [State#state.worker_id]),
						{noreply, State#state{socket=undefined}, 0};
					{error, PosixError} ->
						gen_tcp:close(Socket),
						msbus_logger:error("Erro POSIX ~p em gen_tcp:controlling_process no server worker ~p.", [PosixError, State#state.worker_id]),
						{noreply, State#state{socket=undefined}, 0}
				end
			end)
	catch
		_Exception:Reason -> msbus_logger:error("Erro ao fazer o despacho da requisição. Reason: ~p.", [Reason])
	end;
	

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


%% @doc Processa o request
trata_request(Socket, RequestBin, State) -> 
	case msbus_http_util:encode_request(Socket, RequestBin) of
		 {ok, Request} -> 
			msbus_logger:debug("Request: ~p.", [Request]),
			msbus_dispatcher:dispatch_request(Request);
		 {error, Request, Reason} -> 
			msbus_logger:debug("Request: ~p.", [Request]),
			envia_response(Request, {error, Reason}, State)
	end.

envia_response(_Request, {async, false}, _State) -> 
	em_andamento;

envia_response(Request, {async, true}, _State) ->
	RID = msbus_http_util:rid_to_string(Request#request.rid),
	Ticket = iolist_to_binary([<<"{\"ticket\":\"">>, RID, "\"}"]),
	Response = msbus_http_util:encode_response(<<"200">>, Ticket),
	envia_response(200, ok, Request, Response);

envia_response(Request, {ok, Result}, _State) -> 
	try
		case Request#request.servico#servico.async of
			true -> io:format("Ticket já foi entregue\n");
			_ -> 
				Response = msbus_http_util:encode_response(<<"200">>, Result),
				envia_response(200, ok, Request, Response)
		end
	catch
		_:_ -> msbus_logger:error("deu erro nesse request: ~p\n", [Request])
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
	
