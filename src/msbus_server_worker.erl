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
				socket	  = undefined	%% socket da requisição do cliente
				}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start({Num, LSocket}) -> 
    gen_server:start_link(?MODULE, {Num, LSocket}, []).
    
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

init({Worker_Id, LSocket}) ->
	process_flag(trap_exit, true),
    State = #state{worker_id = Worker_Id, lsocket=LSocket},
    {ok, State, 0};

%% init para processos que vão processar a fila de requisições de saída
init(_Args) ->
    %fprof:trace([start, {procs, [self()]}]),
    {ok, #state{}}.
   
handle_cast(shutdown, State=#state{socket = undefined}) ->
    %io:format("shutdown worker undefined socket\n"),
    {stop, normal, State};

handle_cast(shutdown, State=#state{socket = Socket}) ->
    %io:format("shutdown worker com socket\n"),
    gen_tcp:close(Socket),
    {stop, normal, State#state{socket = undefined}};

handle_cast({Socket, RequestBin}, State) ->
	trata_request(Socket, RequestBin, State),
	{noreply, State#state{socket = undefined}};
	
handle_cast({static_file, Request, Result}, State) ->
    %io:format("envia response static file\n"),
    envia_response(Request, Result, State),
    {noreply, State#state{socket = undefined}};

handle_cast({servico, Request, Result}, State) ->
	%io:format("envia response servico\n"),
	envia_response(Request, Result, State),
	{noreply, State#state{socket = undefined}}.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(timeout, State=#state{lsocket = undefined}) ->
	{noreply, State};

handle_info(timeout, State=#state{lsocket = LSocket}) ->
    %io:format("Listen for accept server worker ~p com state ~p\n", [State#state.worker_id, State]),
	case gen_tcp:accept(LSocket, ?TCP_ACCEPT_CONNECT_TIMEOUT) of
		{ok, Socket} -> 
			% connection is established
			%io:format("Conexão estabelecida para o server worker ~p.\n", [State#state.worker_id]),
			NewState = State#state{socket = Socket}, 
			io:format("NewState is ~p.\n", [NewState]),
			{noreply, NewState};
		{error, closed} -> 
			% ListenSocket is closed
			io:format("Socket do listener foi fechado para o server worker ~p.\n", [State#state.worker_id]),
			{noreply, State#state{lsocket = undefined, socket = undefined}};
		{error, timeout} ->
			% no connection is established within the specified time
			io:format("Nenhuma conexão estabelecida durante ~p para o server socket ~p. Reiniciando o accept.\n", [timeout, State#state.worker_id]),
			{noreply, State, 0};
		{error, system_limit} ->
			io:format("No available ports in the Erlang emulator are in use for server worker ~p. System_limit: ~p\n", [State#state.worker_id, system_limit]),
			msbus_util:sleep(3000),
			{noreply, State};
		{error, PosixError} ->
			io:format("Erro POSIX ~p ao tentar aceitar conexões no server worker ~p.\n", [PosixError, State#state.worker_id]),
			msbus_util:sleep(3000),
			{noreply, State#state{socket = undefined}}
	end;

handle_info({tcp, Socket, RequestBin}, State) ->
	%io:format("init transaction com state ~p\n", [State]),
	msbus_pool:transaction(msbus_server_worker_pool, 
		fun(Worker) ->
			case gen_tcp:controlling_process(Socket, Worker) of
				ok -> 
					inet:setopts(Socket,[{active,once}]),
					%io:format("cast to server worker transaction com state ~p\n", [State]),
					gen_server:cast(Worker, {Socket, RequestBin});
				{error, closed} -> 
					io:format("Falhou gen_tcp:controlling_process pois socket foi fechado no server socket ~p.\n", [State#state.worker_id]),
					{noreply, State#state{socket=undefined}};
				{error, not_owner} -> 
					msbus_logger:error("Http worker ~p não é o dono do socket.\n", [State#state.worker_id]);
				{error, PosixError} ->
					gen_tcp:close(Socket),
					io:format("Erro POSIX ~p em gen_tcp:controlling_process no server worker ~p.\n", [PosixError, State#state.worker_id])
			end
		end),
	NewState = State#state{socket=undefined}, 
	%io:format("finish transaction com state ~p\n", [NewState]),
	{noreply, NewState, 0};

handle_info({tcp_closed, _Socket}, State) ->
	%io:format("tcp_closed of worker com state ~p\n", [State]),
	{noreply, State#state{socket = undefined}};

handle_info(_Msg, State) ->
	%io:format("MENSAGEM ~p DESCONHECIDA COM STATE ~p!\n", [Msg, State]),
   {noreply, State}.

handle_info(State) ->
	%io:format("WORKER STATE ~p!\n", [State]),
   {noreply, State}.

terminate(_Reason, #state{socket = undefined}) ->
   %io:format("Terminate server worker undefined socket. Reason: ~p\n", [Reason]),
    ok;

terminate(_Reason, #state{socket = Socket}) ->
	%io:format("Terminate server worker com socket. Reason: ~p\n", [Reason]),
	gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================


%% @doc Processa o request
trata_request(Socket, RequestBin, State) -> 
	%io:format("Server worker ~p trata request com state ~p\n", [State#state.worker_id, State]),
	case msbus_http_util:encode_request(Socket, RequestBin) of
		 {ok, Request} -> 
			msbus_dispatcher:dispatch_request(Request);
		 {error, Request, Reason} -> 
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
		_:_ -> io:format("deu erro nesse request: ~p\n", [Request])
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
	
