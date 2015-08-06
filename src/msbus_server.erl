%%********************************************************************
%% @title Servidor HTTP
%% @version 1.0.0
%% @doc Módulo responsável pelo processamento das requisições HTTP.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_server).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include("../include/msbus_http_messages.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([start_listen/2, stop_listen/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {listener=[]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================
 
start_listen(Port, From) ->
	gen_server:call(?SERVER, {start_listen, Port, From}).

stop_listen(Port, From) ->
	gen_server:call(?SERVER, {stop_listen, Port, From}).
	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	case do_start_listen(?CONF_PORT, #state{}) of
		{ok, NewState} -> {ok, NewState};
		{{error, _Reason}, NewState} -> {error, NewState}
	end.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({start_listen, Port, From}, State) ->
	{Reply, NewState} = do_start_listen(Port, State),
	From ! Reply, 
	{noreply, NewState};
    
handle_cast({stop_listen, Port, From}, State) ->
	{Reply, NewState} = do_stop_listen(Port, State),
	From ! Reply, 
	{noreply, NewState}.

handle_call({start_listen, Port}, _From, State) ->
	{Reply, NewState} = do_start_listen(Port, State),
	{reply, Reply, NewState}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

do_start_listen(Port, State) ->
	case start_server(Port) of
		{ok, Listen} ->
			NewState = State#state{listener=[{Listen, Port}|State#state.listener]},
			msbus_logger:info("Escutando na porta ~p.", [Port]),
			Reply = {ok, NewState};
		{error, Reason} ->
			Reply = {{error, Reason}, State}
	end,
	Reply.

do_stop_listen(Port, State) ->
	case [ S || {S,P} <- State#state.listener, P == Port] of
		[Listen|_] ->
			stop_server(Listen),
			NewState = State#state{listener=lists:delete({Listen, Port}, State#state.listener)},
			msbus_logger:info("Parou de escutar na porta ~p.", [Port]),
			Reply = {ok, NewState};
		_ -> 
			Reply = {{error, enolisten}, State}
	end,
	Reply.

start_server(Port) ->
	case gen_tcp:listen(Port, [binary, {packet, 0}, 
									   {reuseaddr, true},
									   {active, true}]) of
		{ok, Listen} ->
		    spawn(fun() -> aceita_conexoes(Listen) end),
			{ok, Listen};
		{error, Reason} -> 
			{error, Reason} 
	end.	

stop_server(Listen) ->
	gen_tcp:close(Listen).

aceita_conexoes(Listen) ->
 	case gen_tcp:accept(Listen) of
		{ok, Socket} -> processa_conexao(Listen, Socket);
		{error, closed} -> ok
	end.

processa_conexao(Listen, Socket) -> 
  spawn(fun() -> aceita_conexoes(Listen) end),
  inet:setopts(Socket, [{packet,0},binary, {nodelay,true},{active, true}]),
  Timestamp = calendar:local_time(),
  T1 = msbus_util:get_milliseconds(),
  case get_request(Socket, []) of
     {ok, Request} ->
		case msbus_dispatcher:dispatch_request(Request) of
			{ok, Code, Request1, Response} ->
				gen_tcp:send(Socket, [Response]),
				T2 = msbus_util:get_milliseconds(),
				Latencia = T2 - T1,
				Request2 = Request1#request{timestamp = Timestamp,
									 	    latencia = Latencia,
										    status = 200},
				msbus_health:registra_request(Request2),
				log_status_requisicao(Code, Request2, "OK", Latencia);
			{error, Code, Request1, Response, ErroInterno} ->
				gen_tcp:send(Socket, [Response]),
				T2 = msbus_util:get_milliseconds(),
				Latencia = T2 - T1,
				log_status_requisicao(Code, Request1, atom_to_list(ErroInterno), Latencia)
		end;
     {error, tcp_closed} -> ok;
     {error, Request, invalid_payload} -> 
		Response = msbus_http_util:encode_response(<<"415">>, ?HTTP_ERROR_415),
		gen_tcp:send(Socket, [Response]),
		T2 = msbus_util:get_milliseconds(),
		Latencia = T2 - T1,
		log_status_requisicao(<<"415">>, Request, invalid_payload, Latencia);
     {error, Request, Reason} -> 
		%% Foi possível ler o cabecalho (ou parte dele)
		Response = msbus_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
		gen_tcp:send(Socket, [Response]),
		T2 = msbus_util:get_milliseconds(),
		Latencia = T2 - T1,
		log_status_requisicao(<<"400">>, Request, Reason, Latencia);
     {error, Reason} -> 
		%% Requisição inválida.
		Response = msbus_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
		gen_tcp:send(Socket, [Response]),
		msbus_logger:error("Requisicao inválida: ~s.", [Reason])
  end.	

get_request(Socket, L) ->
	try
		get_request_socket(Socket, L)
	catch
		_Exception:_Reason ->  {error, invalid_request} 
	end.

get_request_socket(Socket, L) ->
    receive
		{tcp, Socket, Bin} -> 
			L1 = L ++ binary_to_list(Bin),
			%% is_fim_header verifica quando o cabeçalho está completo
			case is_fim_header(L1, []) of
				more ->
					%% o cabeçalho está incompleto e precisa mais dados
					get_request(Socket, L1);
				{Header, Payload} ->
					%% cabeçalho completo, podemos fazer o parser do header e do payload
					
					%% Faz o parser do cabeçalho e obtém o request
					case msbus_http_util:get_http_header(Header) of
						{ok, Request} ->
							case possui_payload(Request) of
								false ->
									% Requisições GET e DELETE
									{ok, Request};
								true ->
									% Requisições POST e PUT
									case get_request_payload(Socket, Request#request.content_length, Payload) of
										{ok , PayloadText, PayloadMap} ->
											Request1 = Request#request{payload = PayloadText, payload_map = PayloadMap},
											{ok, Request1};
										{error, Reason} -> {error, Request, Reason}
									end
							end;
						{error, Request, Reason} -> {error, Request, Reason};
						{error, Reason} -> {error, Reason}
					end
			end;
		{tcp_closed, Socket} ->
		    {error, tcp_closed};
		_Any  ->
		    get_request(Socket, L)
    end.

%% @doc Retorna boolean indicando se possui payload
possui_payload(Request) when Request#request.type =:= "GET"; 
							 Request#request.type =:= "DELETE" -> false;
possui_payload(Request) when Request#request.type =:= "POST"; 
							 Request#request.type =:= "PUT" -> 
	Request#request.content_length > 0.

get_request_payload(Socket, Content_Length, L) when length(L) /= Content_Length ->
    receive
		{tcp, Socket, Bin} -> 
			L1 = L ++ binary_to_list(Bin),
			get_request_payload(Socket, Content_Length, L1);
		{tcp_closed, Socket} ->
		    {error, tcp_closed};
		_Any  ->
		    get_request_payload(Socket, Content_Length, L)
    end;
    
get_request_payload(_Socket, _Content_Length, L) ->    
	Payload = list_to_binary(L),
	case decode_payload(Payload) of
		{ok, PayloadMap} -> {ok, L, PayloadMap};
		Error -> Error
	end. 

%% @doc Decodifica o payload e transforma em um tipo Erlang
decode_payload(<<>>) ->
	{ok, #{}};

%% @doc Decodifica o payload e transforma em um tipo Erlang
decode_payload(Payload) ->
	case msbus_util:json_decode_as_map(Payload) of
		{ok, PayloadMap} -> {ok, PayloadMap};
		{error, _Reason} -> {error, invalid_payload}
	end.

is_fim_header("\r\n\r\n" ++ T, L) -> {lists:reverse(L), T};
is_fim_header([H|T], L)           -> is_fim_header(T, [H|L]);
is_fim_header([], _)              -> more.

%% @doc Imprime o status da requisição no log
log_status_requisicao(Code, Request, Status, Latencia) when erlang:is_record(Request, request) ->
	Metodo = Request#request.type,
	Url = Request#request.url,
	HTTP_Version = Request#request.versao_http,
	Accept = Request#request.accept,
	User_Agent = Request#request.user_agent,
	Payload = Request#request.payload,
	case Payload of
		undefined ->
			Texto =  "~s ~s ~s {\n\tAccept: ~s:\n\tUser-Agent: ~s\n\tStatus: ~s <<~s>> (~pms)\n}",
			msbus_logger:info(Texto, [Metodo, Url, HTTP_Version, Accept, User_Agent, Code, Status, Latencia]);
		_ ->
			Content_Type = Request#request.content_type,
			Texto =  "~s ~s ~s {\n\tAccept: ~s:\n\tUser-Agent: ~s\n\tContent-Type: ~s\n\tPayload: ~s\n\tStatus: ~s <<~s>> (~pms)\n}",
			msbus_logger:info(Texto, [Metodo, Url, HTTP_Version, Accept, User_Agent, Content_Type, Payload, Code, Status, Latencia])
	end;
	
log_status_requisicao(Code, Request, Status, Latencia) ->	
	Texto =  "~s {\n\tStatus: ~p ~s (~sms)\n}", 
	msbus_logger:info(Texto, [Code, Request, Status, Latencia]),
	msbus_logger:info(Request). 
	

	
