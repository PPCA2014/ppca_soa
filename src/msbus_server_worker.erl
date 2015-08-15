%%********************************************************************
%% @title Módulo msbus_server_worker
%% @version 1.0.0
%% @doc Módulo responsável pelo processamento das requisições HTTP.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_server_worker).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include("../include/msbus_http_messages.hrl").

%% Server API
-export([start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {lsocket, socket}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(LSocket) -> 
    gen_server:start_link(?MODULE, LSocket, []).
    
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(LSocket) ->
   % process_flag(trap_exit, true),
    State = #state{lsocket=LSocket},
    {ok, State, 0}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({static_file, Request, Result}, State) ->
    do_processa_response(Request, Result, State),
    {noreply, State, 0};

handle_cast({servico, Request, Result}, State) ->
	case do_processa_response(Request, Result, State) of
		em_andamento  -> {noreply, State};
		ok			  -> {noreply, State, 0}
	end.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(timeout, State=#state{lsocket=LSocket}) ->
    case gen_tcp:accept(LSocket) of
		{ok, Socket}   -> {noreply, State#state{socket=Socket}};
		{error, Error} -> 
			msbus_logger:error("Falha na chamada gen_tcp:accept. Erro interno: ~p.", [Error]),
			{noreply, State}
	end;

handle_info({tcp, Socket, RequestBin}, State) ->
    do_processa_request(Socket, RequestBin, State),
	{noreply, State, 0};

handle_info({tcp_closed, _Socket}, State) ->
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

%% @doc Processa o pedido do request
do_processa_request(Socket, RequestBin, State) -> 
	case msbus_http_util:encode_request(Socket, RequestBin) of
		 {ok, Request} -> msbus_dispatcher:dispatch_request(Request);
		 {error, Request, Reason} -> do_processa_response(Request, {error, Reason}, State);
		 {error, Reason} -> 
			gen_tcp:close(Socket),
			msbus_logger:error("Erro ~p.", [Reason])
	end.	

do_processa_response(_Request, {async, <<"false">>}, _State) -> em_andamento;

do_processa_response(Request, {async, <<"true">>}, _State) ->
	RID = msbus_http_util:rid_to_string(Request#request.rid),
	Ticket = iolist_to_binary([<<"{\"ticket\":\"">>, RID, "\"}"]),
	Response = msbus_http_util:encode_response(<<"200">>, Ticket),
	do_processa_response("OK", <<"200">>, Request, Response),
	ok;

do_processa_response(Request, {ok, Result}, _State) -> 

	Async = maps:get(<<"async">>, Request#request.servico),
	case Async of
		<<"true">> -> io:format("Ticket já foi entregue\n");
		_ -> 
			Response = msbus_http_util:encode_response(<<"200">>, Result),
			do_processa_response("OK", <<"200">>, Request, Response)
	end,
	ok;

do_processa_response(Request, {ok, Result, MimeType}, _State) when erlang:is_record(Request, request) ->
	Response = msbus_http_util:encode_response(<<"200">>, Result, MimeType),
	do_processa_response("OK", <<"200">>, Request, Response),
	ok;

do_processa_response(Request, {error, notfound}, _State) when erlang:is_record(Request, request) ->
	Response = msbus_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404),
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	log_status_requisicao(<<"404">>, Request, notfound, Latencia, StatusSend),
	ok;

do_processa_response(Request, {error, invalid_payload}, _State) when erlang:is_record(Request, request) ->
	Response = msbus_http_util:encode_response(<<"415">>, ?HTTP_ERROR_415),
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	log_status_requisicao(<<"503">>, Request, invalid_payload, Latencia, StatusSend),
	ok;

do_processa_response(Request, {error, file_not_found}, _State) when erlang:is_record(Request, request) ->
	Response = msbus_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404_FILE_NOT_FOUND),
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	log_status_requisicao(<<"503">>, Request, file_not_found, Latencia, StatusSend),
	ok;

do_processa_response(Request, {error, Reason}, _State) when erlang:is_record(Request, request) ->
	Response = msbus_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	log_status_requisicao(<<"400">>, Request, Reason, Latencia, StatusSend),
	ok;

do_processa_response(Request, {error, Reason, _ErroInterno}, _State) when erlang:is_record(Request, request) ->
	Response = msbus_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	log_status_requisicao(<<"400">>, Request, Reason, Latencia, StatusSend),
	ok;

do_processa_response(Request, Result, State) when erlang:is_record(Request, request) ->
	do_processa_response(Request, {ok, Result}, State),
	ok.

do_processa_response(Code, Status, Request, Response) ->
	StatusSend = msbus_http_util:send_request(Request#request.socket, Response),
	T2 = msbus_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	Request2 = Request#request{latencia = Latencia, status = Code},
	msbus_health:registra_request(Request2),
	case StatusSend of
		ok -> ok;
		_Error -> log_status_requisicao(Code, Request2, Status, Latencia, StatusSend)
	end,
	ok.
	
%% @doc Imprime o status da requisição no log
log_status_requisicao(Code, Request, Status, Latencia, StatusSend) when erlang:is_record(Request, request) ->
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
			msbus_logger:info(Texto, [Metodo, Url, HTTP_Version, Accept, User_Agent, Code, Status, Latencia, StatusSend2]);
		_ ->
			Content_Type = Request#request.content_type,
			Texto =  "~s ~s ~s {\n\tAccept: ~s:\n\tUser-Agent: ~s\n\tContent-Type: ~s\n\tPayload: ~s\n\tStatus: ~s <<~s>> (~pms)\n\t~s\n}",
			msbus_logger:info(Texto, [Metodo, Url, HTTP_Version, Accept, User_Agent, Content_Type, Payload, Code, Status, Latencia, StatusSend2])
	end;
	
log_status_requisicao(Code, Request, Status, Latencia, StatusSend) ->	
	StatusSend2 = format_send_error(StatusSend),
	Texto =  "~s {\n\tStatus: ~s ~s (~pms)\n\t~s\n}", 
	msbus_logger:info(Texto, [Request, Code, Status, Latencia, StatusSend2]).
	
format_send_error(StatusSend) ->
	case StatusSend of
		ok -> "";
		_Error -> io_lib:format("Erro tcp_send: ~p", [StatusSend]) 
	end.
	
