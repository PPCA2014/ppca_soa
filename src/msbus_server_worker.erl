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
-record(state, {lsocket,	%% socket do listener
				socket		%% socket da requisição
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

%% init para processos que vão processar a fila de requisições de entrada
init({_Num, LSocket}) ->
    State = #state{lsocket=LSocket},
    {ok, State, 0};

%% init para processos que vão processar a fila de requisições de saída
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
   
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({Socket, RequestBin}, State) ->
	do_processa_request(Socket, RequestBin, State),
	{noreply, State};
	
handle_cast({static_file, Request, Result}, State) ->
    do_processa_response(Request, Result, State),
    {noreply, State};

handle_cast({servico, Request, Result}, State) ->
	case do_processa_response(Request, Result, State) of
		em_andamento  -> {noreply, State};
		ok			  -> {noreply, State}
	end.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(timeout, State) ->
    conexao_accept(State);

handle_info({tcp, Socket, RequestBin}, State) ->
	msbus_pool:transaction(msbus_server_worker_pool, 
		fun(Worker) ->
			case gen_tcp:controlling_process(Socket, Worker) of
				ok -> gen_server:cast(Worker, {Socket, RequestBin});
				{error, closed} -> 
					{noreply, State#state{socket=undefined}};
				{error, not_owner} -> msbus_logger:error("ocorreu um erro ao invocar gen_tcp:controlling_process")
			end
		end),
	{noreply, State, 0};

handle_info({tcp_closed, _Socket}, State) ->
	{noreply, State#state{socket=undefined}};

handle_info(_Msg, State) ->
   {noreply, State#state{socket=undefined}}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, #state{lsocket=_LSocket}) ->
	%gen_tcp:close(LSocket),
    ok;

terminate(_Reason, _State) ->
	ok.
	
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

conexao_accept(State) ->
	case gen_tcp:accept(State#state.lsocket) of
		{ok, Socket}   -> {noreply, State#state{socket=Socket}};
		{error, _Reason} -> {noreply, State#state{socket=undefined}}
	end.
		

%% @doc Processa o request
do_processa_request(Socket, RequestBin, State) -> 
	case msbus_http_util:encode_request(Socket, RequestBin) of
		 {ok, Request} -> msbus_dispatcher:dispatch_request(Request);
		 {error, Request, Reason} -> do_processa_response(Request, {error, Reason}, State);
		 {error, Reason} -> 
			gen_tcp:close(Socket),
			msbus_logger:error("Erro processa request: ~p.", [Reason])
	end.	

do_processa_response(_Request, {async, false}, _State) -> 
	em_andamento;

do_processa_response(Request, {async, true}, _State) ->
	RID = msbus_http_util:rid_to_string(Request#request.rid),
	Ticket = iolist_to_binary([<<"{\"ticket\":\"">>, RID, "\"}"]),
	Response = msbus_http_util:encode_response(<<"200">>, Ticket),
	do_processa_response(200, ok, Request, Response);

do_processa_response(Request, {ok, Result}, _State) -> 
	case Request#request.servico#servico.async of
		true -> io:format("Ticket já foi entregue\n");
		_ -> 
			Response = msbus_http_util:encode_response(<<"200">>, Result),
			do_processa_response(200, ok, Request, Response)
	end;

do_processa_response(Request, {ok, Result, MimeType}, _State) ->
	Response = msbus_http_util:encode_response(<<"200">>, Result, MimeType),
	do_processa_response(200, ok, Request, Response);

do_processa_response(Request, {error, notfound}, _State) ->
	Response = msbus_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404),
	do_processa_response(404, notfound, Request, Response);

do_processa_response(Request, {error, no_authorization}, _State) ->
	Response = msbus_http_util:encode_response(<<"401">>, ?HTTP_ERROR_401),
	do_processa_response(401, no_authorization, Request, Response);

do_processa_response(Request, {error, invalid_payload}, _State) ->
	Response = msbus_http_util:encode_response(<<"415">>, ?HTTP_ERROR_415),
	do_processa_response(415, invalid_payload, Request, Response);

do_processa_response(Request, {error, file_not_found}, _State) ->
	Response = msbus_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404_FILE_NOT_FOUND),
	do_processa_response(404, file_not_found, Request, Response);

do_processa_response(Request, {error, Reason}, _State) ->
	Response = msbus_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
	do_processa_response(400, Reason, Request, Response);

do_processa_response(Request, {error, servico_fora, ErroInterno}, _State) ->
	Response = msbus_http_util:encode_response(<<"503">>, ?HTTP_ERROR_503),
	Reason2 = io_lib:format("~p ~p", [servico_fora, ErroInterno]),
	do_processa_response(503, Reason2, Request, Response);

do_processa_response(Request, {error, Reason, ErroInterno}, State) ->
	Reason2 = io_lib:format("~p ~p", [Reason, ErroInterno]),
	do_processa_response(Request, {error, Reason2}, State);

do_processa_response(Request, Result, State) ->
	do_processa_response(Request, {ok, Result}, State).

do_processa_response(Code, Reason, Request, Response) ->
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
	
