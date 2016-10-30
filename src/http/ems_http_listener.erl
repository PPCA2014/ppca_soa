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
	  %mochiweb_http:start([{port, Port}, {loop, fun(Req) -> process_request(Req) end}]).
	  Dispatch = cowboy_router:compile([
		{'_', [
			{'_', ems_http_handler, []},
			{"/ws", ems_websocket_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, 100, [{port, Port}], #{
		compress => true,
		env => #{dispatch => Dispatch}
	}).
		
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
			case ems_dispatcher_cache:lookup_options() of
				false ->
					io:format("no cache options\n"),
					Response = ems_http_util:encode_response(<<"200">>, <<>>, <<"Cache-Control: max-age=290304000, public"/utf8>>),
					ems_dispatcher_cache:add_options(Response),
					ems_socket:send_data(Req:get(socket), Response);
				{true, Response} -> ems_socket:send_data(Req:get(socket), Response)
			end;
		_ ->
			case ems_http_util:encode_request_mochiweb(Req, self()) of
				{ok, Request = #request{rowid = Rowid, t1 = Timestamp}} -> 
					case ems_dispatcher:dispatch_request(Request) of
						{ok, #request{result_cache = true}, Response} -> 
							send_response(200, ok, Request, Response);
						{ok, Request2, Response} -> 
							{HttpCode, Reason, Response2} = encode_response(Request2, Response),
							ems_dispatcher_cache:add(Rowid, Timestamp, Response2),
							send_response(HttpCode, Reason, Request2, Response2);
						Error ->
							Response = ems_http_util:encode_response(<<"400">>, Error),
							send_response(400, Error, Request, Response)
					end;
				{error, Reason} -> 
					Error = {error, Reason},
					Req:respond({400, [{"Content-Type", "application/json; charset=utf-8"}], [Error]})
			end
	end.

encode_response(Request = #request{type = Method, 
								    service = #service{page_module = PageModule,
									 				   page_mime_type = PageMimeType}}, Result) ->
	case PageModule of
		null ->
			case Result of
				{ok, <<Content/binary>>} -> 
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
					Response = ems_http_util:encode_response(HttpCodeBin, Content),
					{HttpCode, ok, Response};
				{ok, <<Content/binary>>, <<MimeType/binary>>} ->
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
					Response = ems_http_util:encode_response(HttpCodeBin, Content, MimeType),
					{HttpCode, ok, Response};
				{error, Reason} = Error ->
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, false),
					Response = ems_http_util:encode_response(HttpCodeBin, {error, Reason}),
					{HttpCode, Error, Response};
				Content when is_map(Content) -> 
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
					Response = ems_http_util:encode_response(HttpCodeBin, ems_util:json_encode(Content)),
					{HttpCode, ok, Response};
				Content = [H|_] when is_map(H) -> 
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
					Response = ems_http_util:encode_response(HttpCodeBin, ems_util:json_encode(Content)),
					{HttpCode, ok, Response};
				Content = [H|_] when is_tuple(H) -> 
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
					Response = ems_http_util:encode_response(HttpCodeBin, ems_schema:to_json(Content)),
					{HttpCode, ok, Response};
				Content -> 
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
					Response = ems_http_util:encode_response(HttpCodeBin, Content),
					{HttpCode, ok, Response}
			end;
		_ -> 
			case Result of
				{error, Reason} = Error ->
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, false),
					Response = ems_http_util:encode_response(HttpCodeBin, {error, Reason}),
					{HttpCode, Error, Response};
				_ ->
					Content = ems_page:render(PageModule, Result),
					{HttpCode, HttpCodeBin} = get_http_code_verb(Method, true),
					Response = ems_http_util:encode_response(HttpCodeBin, Content, PageMimeType),
					{HttpCode, ok, Response}
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

	

