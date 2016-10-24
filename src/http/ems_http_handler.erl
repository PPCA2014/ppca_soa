%%********************************************************************
%% @title Module ems_http_server
%% @version 1.0.0
%% @doc Main module HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_handler).

-export([init/2, init/3, terminate/3, handle_http/1]).

-record(state, {}).


init(_Type, Req, _Opts) -> 
	io:format("passei aqui\n"),
	{ok, Req, #state{}}.


init(Req0, State) ->
	io:format("Aqui no handler: ~p  and state: ~p \n\n\n", [Req0, State]),
	try
		case cowboy_req:has_body(Req0) of
			true ->
				io:format("tem payload\n"),
				{ok, Body, _} = cowboy_req:read_body(Req0),
				io:format("payload is ~p\n", [Body]);
			false ->
				io:format("não tem payload\n")
		end,
		%R = cowboy_req:reply(200, Req0),
		%io:format("result is ~p\n", [R]),
		{ok, Req0, State}   	
	catch
		Error -> 
			io:format("erru foi ~p\n", [Error]),
		{ok, Req0, State}   	
	end.
	
terminate(_Reason, Req, _State) ->  
	io:format("passei terminate\n"),
	cowboy_req:reply(200, Req),
	ok.    


handle_http(Req) ->
	io:format("requet is ~p\n", [Req]),
	Payload = Req:recv_body(),
	Method = Req:get(method),
	Path = Req:get(path),
	Opts = Req:get(opts),
	Peer = Req:get(peer),
	Socket = Req:get(socket),
	Version = Req:get(version),
	{ok, {Ip,_Port}} = mochiweb_socket:peername(Socket),
	io:format("payload is ~p\n", [Payload]),
	io:format("method is ~p\n", [Method]),
	io:format("path is ~p\n", [Path]),
	io:format("opts is ~p\n", [Opts]),
	io:format("peer is ~p\n", [Peer]),
	io:format("ip is ~p\n", [Ip]),
	io:format("version is ~p\n", [Version]),
    Req:respond({200, [{"Content-Type", "text/xml"}], ["ola mundo"]}).

