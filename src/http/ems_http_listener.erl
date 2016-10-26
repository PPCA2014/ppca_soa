%%********************************************************************
%% @title Module ems_http_listener
%% @version 1.0.0
%% @doc Listener module for HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_listener).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Server API
-export([start/3, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {lsocket = undefined, 
				listener_name,
				tcp_config}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(IpAddress, TcpConfig, ListenerName) -> 
    gen_server:start_link(?MODULE, {IpAddress, TcpConfig, ListenerName}, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init({IpAddress, 
	  TcpConfig = #tcp_config{tcp_port = Port,
							  tcp_is_ssl = IsSsl},
	  ListenerName}) ->
	  mochiweb_http:start([{port, Port}, {loop, fun(Req) -> process_request(Req) end}]).
		
handle_cast(shutdown, State=#state{lsocket = undefined}) ->
    {stop, normal, State};
    
handle_cast(shutdown, State=#state{lsocket = LSocket}) ->
    {stop, normal, State#state{lsocket = undefined}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
    
handle_info(timeout, State) ->  {noreply, State}.

handle_info(State) -> {noreply, State}.

terminate(Reason, _State) -> ok.
 
code_change(_OldVsn, State, _Extra) -> {ok, State}.

process_request(Req) ->
	case Req:get(method) of
		'OPTIONS' ->
			case erlang:get(result_cache_options) of
				undefined ->
					{HttpCode, HttpCodeBin} = get_http_code_verb("OPTIONS", true),
					Response = ems_http_util:encode_response(HttpCodeBin, <<>>, <<"Cache-Control: max-age=290304000, public"/utf8>>),
					erlang:put(result_cache_options, Response),
					ems_socket:send_data(Req:get(socket), Response);
				Response -> ems_socket:send_data(Req:get(socket), Response)
			end;
		_ ->
			case ems_http_util:encode_request_mochiweb(Req, self()) of
				{ok, Request} -> 
					case ems_dispatcher:dispatch_request(Request) of
						{ok, #request{result_cache = true}, Response} -> 
							send_response(200, ok, Request, Response);
						{ok, Request2, Response} -> 
							process_response(Request2, Response);
						Error ->
							Response = ems_http_util:encode_response(<<"400">>, Error),
							send_response(400, Error, Request, Response)
					end;
				{error, Reason} -> 
					Error = {error, Reason},
					Req:respond({400, [{"Content-Type", "application/json; charset=utf-8"}], [Error]})
			end
	end.

process_response(Request = #request{type = Method, 
								    service = #service{page_module = PageModule,
									 				   page_mime_type = PageMimeType}}, Result) ->
	case PageModule of
		null ->
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
			end;
		_ -> 
			case Result of
				{error, Reason} ->
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, false),
					Response = ems_http_util:encode_response(HttpCodeBin, {error, Reason}),
					send_response(HttpCode, {error, Reason}, Request, Response);
				_ ->
					Content = ems_page:render(PageModule, Result),
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
					Response = ems_http_util:encode_response(HttpCodeBin, Content, PageMimeType),
					send_response(HttpCode, ok, Request, Response)
			end
	end.

get_http_code_verb("POST", true)  -> {201, <<"201">>};
get_http_code_verb("PUT", false)  -> {400, <<"400">>};
get_http_code_verb(_, true)  -> {200, <<"200">>};
get_http_code_verb(_, false)  -> {400, <<"400">>}.

send_response(Code, Reason, Request = #request{rowid = Rowid, result_cache = ResultCache, socket = Socket}, Response) ->
	T2 = ems_util:get_milliseconds(),
	Latencia = T2 - Request#request.t1,
	StatusSend = ems_socket:send_data(Socket, Response),
	ems_socket:close(Socket),
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

	

