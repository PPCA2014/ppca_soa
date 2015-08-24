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
-record(state, {lsocket, socket}).

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

init({_Num, LSocket}) ->
    process_flag(trap_exit, true),
    State = #state{lsocket=LSocket},
    {ok, State, 0};

init(_Args) ->
    process_flag(trap_exit, true),
    msbus_eventmgr:registra_interesse(ok_request, fun(_Q, R) -> msbus_server_worker:cast(R) end),
    msbus_eventmgr:registra_interesse(erro_request, fun(_Q, R) -> msbus_server_worker:cast(R) end),
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
    io:format("new request ~p\n", [self()]),
	msbus_pool:transaction(msbus_server_worker_pool, 
		fun(Worker) ->
			ok = gen_tcp:controlling_process(Socket, Worker),
			gen_server:cast(Worker, {Socket, RequestBin})
		end),
    %do_processa_request(Socket, RequestBin, State),
    io:format("fim request ~p\n", [self()]),
	{noreply, State, 0};

handle_info({tcp_closed, _Socket}, State) ->
	io:format("tcp_closed ~p\n", [self()]),
	{noreply, State, 0};

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

conexao_accept(State) ->
    io:format("accept ~p\n", [self()]),
    case gen_tcp:accept(State#state.lsocket) of
		{ok, Socket}   -> {noreply, State#state{socket=Socket}};
		{error, Error} -> 
			msbus_logger:error("Falha na chamada gen_tcp:accept. Erro interno: ~p.", [Error]),
			{noreply, State}
	end.


%% @doc Processa o pedido do request
do_processa_request(Socket, RequestBin, State) -> 
	case msbus_http_util:encode_request(Socket, RequestBin) of
		 {ok, Request} -> msbus_dispatcher:dispatch_request(Request, self());
		 {error, Request, Reason} -> do_processa_response(Request, {error, Reason}, State);
		 {error, Reason} -> 
			gen_tcp:close(Socket),
			msbus_logger:error("Erro ~p.", [Reason])
	end.	

do_processa_response(_Request, {async, false}, _State) -> 

	io:format("em andamento\n\n\n"),
em_andamento;

do_processa_response(Request, {async, true}, _State) ->
	io:format("async async\n\n"),
	RID = msbus_http_util:rid_to_string(Request#request.rid),
	Ticket = iolist_to_binary([<<"{\"ticket\":\"">>, RID, "\"}"]),
	Response = msbus_http_util:encode_response(<<"200">>, Ticket),
	do_processa_response("OK", <<"200">>, Request, Response),
	ok;

do_processa_response(Request, {ok, Result}, _State) -> 
	case Request#request.servico#servico.async of
		true -> io:format("Ticket já foi entregue\n");
		_ -> 
			Response = msbus_http_util:encode_response(<<"200">>, Result),
			do_processa_response("OK", <<"200">>, Request, Response)
	end,
	ok;

do_processa_response(Request, {ok, Result, MimeType}, _State) ->
	Response = msbus_http_util:encode_response(<<"200">>, Result, MimeType),
	do_processa_response("OK", <<"200">>, Request, Response),
	ok;

do_processa_response(Request, {error, notfound}, _State) ->
	Response = msbus_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404),
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	log_status_requisicao(<<"404">>, Request, notfound, Latencia, StatusSend),
	ok;

do_processa_response(Request, {error, invalid_payload}, _State) ->
	Response = msbus_http_util:encode_response(<<"415">>, ?HTTP_ERROR_415),
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	log_status_requisicao(<<"503">>, Request, invalid_payload, Latencia, StatusSend),
	ok;

do_processa_response(Request, {error, file_not_found}, _State) ->
	Response = msbus_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404_FILE_NOT_FOUND),
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	log_status_requisicao(<<"503">>, Request, file_not_found, Latencia, StatusSend),
	ok;

do_processa_response(Request, {error, Reason}, _State) ->
	Response = msbus_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	log_status_requisicao(<<"400">>, Request, Reason, Latencia, StatusSend),
	ok;

do_processa_response(Request, {error, Reason, ErroInterno}, State) ->
	Reason2 = io_lib:format("~p ~p", [Reason, ErroInterno]),
	do_processa_response(Request, {error, Reason2}, State);

do_processa_response(Request, Result, State) ->
	do_processa_response(Request, {ok, Result}, State),
	ok.

do_processa_response(Code, Status, Request, Response) ->
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	Request2 = Request#request{latencia = Latencia, status = Code},
	msbus_request:update_request(Request2),
	case StatusSend of
		ok -> log_status_requisicao(Code, Request2, Status, Latencia, StatusSend);
		_Error -> log_status_requisicao(Code, Request2, Status, Latencia, StatusSend)
	end,
	ok.
	
%% @doc Imprime o status da requisição no log
log_status_requisicao(Code, Request, Reason, Latencia, StatusSend) when erlang:is_record(Request, request) ->
	Metodo = Request#request.type,
	Url = Request#request.url,
	HTTP_Version = Request#request.versao_http,
	Accept = Request#request.accept,
	User_Agent = Request#request.user_agent,
	Payload = Request#request.payload,
	StatusSend2 = format_send_error(StatusSend),
	case Payload of
		undefined ->
			Texto =  "~s ~s ~s {\n\tAccept: ~s:\n\tUser-Agent: ~s\n\tStatus: ~s <<~s>> (~pms)\n\t~s\n}",
			msbus_logger:info(Texto, [Metodo, Url, HTTP_Version, Accept, User_Agent, Code, Reason, Latencia, StatusSend2]);
		_ ->
			Content_Type = Request#request.content_type,
			Texto =  "~s ~s ~s {\n\tAccept: ~s:\n\tUser-Agent: ~s\n\tContent-Type: ~s\n\tPayload: ~s\n\tStatus: ~s <<~s>> (~pms)\n\t~s\n}",
			msbus_logger:info(Texto, [Metodo, Url, HTTP_Version, Accept, User_Agent, Content_Type, Payload, Code, Reason, Latencia, StatusSend2])
	end;
	
log_status_requisicao(Code, Request, Reason, Latencia, StatusSend) ->	
	StatusSend2 = format_send_error(StatusSend),
	Texto =  "~s {\n\tStatus: ~s ~s (~pms)\n\t~s\n}", 
	msbus_logger:info(Texto, [Request, Code, Reason, Latencia, StatusSend2]).
	
	format_send_error(StatusSend) ->
	case StatusSend of
		ok -> "";
		_Error -> io_lib:format("Erro tcp_send: ~p", [StatusSend]) 
	end.
	
