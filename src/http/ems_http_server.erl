%%********************************************************************
%% @title Module ems_http_server
%% @version 1.0.0
%% @doc Main module HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_server).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Server API
-export([start/1, stop/0]).

%% Client API
-export([start_listeners/2, stop_listener/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {listener=[],
				tcp_config
		}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Args) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================

start_listeners(Port, Listen_Address) ->
	gen_server:cast(?SERVER, {start_listeners, Port, Listen_Address}).
 
stop_listener(Port, IpAddress) ->
	gen_server:call(?SERVER, {stop_listener, Port, IpAddress}).
	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(Args) ->
 	ListenAddress = ems_util:binlist_to_list(maps:get(<<"tcp_listen_address">>, Args, [<<"127.0.0.1">>])),
 	AllowedAddress = ems_util:binlist_to_list(maps:get(<<"tcp_allowed_address">>, Args, [])),
	TcpConfig = #tcp_config{
		tcp_listen_address = ListenAddress,
		tcp_listen_address_t = parse_tcp_listen_address(ListenAddress),
		tcp_allowed_address = AllowedAddress,
		tcp_allowed_address_t = parse_allowed_address(AllowedAddress),
		tcp_port = parse_tcp_port(maps:get(<<"tcp_port">>, Args, 2301)),
		tcp_keepalive = parse_keepalive(maps:get(<<"tcp_keepalive">>, Args, true)),
		tcp_nodelay = ems_util:binary_to_bool(maps:get(<<"tcp_nodelay">>, Args, true)),
		tcp_max_http_worker = parse_max_http_worker(maps:get(<<"tcp_max_http_worker">>, Args, ?MAX_HTTP_WORKER))
 	},
 	State = #state{tcp_config = TcpConfig},
	case start_listeners(TcpConfig#tcp_config.tcp_listen_address_t, TcpConfig, State) of
		{ok, State2} ->
			{ok, State2};
		{error, _Reason, State2} -> 
			{stop, State2}
	end.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({start_listeners, Port, Listen_Address}, State) ->
	{_, NewState} = start_listeners(Listen_Address, Port, State),
	{noreply, NewState}.

handle_call({stop_listener, Port, IpAddress}, _From, State) ->
	{Reply, NewState} = do_stop_listener(Port, IpAddress, State),
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

start_listeners([], _TcpConfig, State) -> {ok, State};
start_listeners([H|T], TcpConfig, State) ->
	case do_start_listener(H, TcpConfig, State) of
		{ok, NewState} -> start_listeners(T, TcpConfig, NewState);
		{{error, Reason}, NewState} -> {error, Reason, NewState}
	end.

do_start_listener(IpAddress, TcpConfig = #tcp_config{tcp_port = Port}, State) ->
	case ems_http_listener:start(IpAddress, TcpConfig) of
		{ok, PidListener} ->
			NewState = State#state{listener=[{PidListener, Port, IpAddress}|State#state.listener]},
			{ok, NewState};
		{error, Reason} ->
			{{error, Reason}, State}
	end.

do_stop_listener(Port, IpAddress, State) ->
	case [ S || {S,P,I} <- State#state.listener, {P,I} == {Port, IpAddress}] of
		[PidListener|_] ->
			gen_server:call(PidListener, shutdown),
			NewState = State#state{listener=lists:delete({PidListener, Port, IpAddress}, State#state.listener)},
			ems_logger:info("Stopped listening at the address ~p:~p.", [inet:ntoa(IpAddress), Port]),
			{ok, NewState};
		_ -> 
			{{error, enolisten}, State}
	end.
	
parse_tcp_listen_address(ListenAddress) ->
	lists:map(fun(IP) -> 
					{ok, L2} = inet:parse_address(IP),
					L2 
			  end, ListenAddress).

parse_allowed_address(AllowedAddress) ->
	lists:map(fun(IP) -> 
					ems_http_util:mask_ipaddress_to_tuple(IP)
			  end, AllowedAddress).

parse_keepalive(Keepalive) ->
	ems_util:binary_to_bool(Keepalive).

parse_tcp_port(<<Port/binary>>) -> 
	parse_tcp_port(binary_to_list(Port));		
parse_tcp_port(Port) when is_list(Port) -> 
	parse_tcp_port(list_to_integer(Port));
parse_tcp_port(Port) when is_integer(Port) -> 
	case ems_consist:is_range_valido(Port, 1024, 5000) of
		true -> Port;
		false -> erlang:error("Parameter tcp_port invalid. Enter a value between 1024 and 5000.")
	end.
	
parse_max_http_worker(<<Value/binary>>) -> 
	parse_max_http_worker(binary_to_list(Value));
parse_max_http_worker(Value) -> 
	case ems_consist:is_range_valido(Value, 1, ?MAX_HTTP_WORKER_RANGE) of
		true -> Value;
		false -> erlang:error("Parameter tcp_max_http_worker invalid.")
	end.
	
	
