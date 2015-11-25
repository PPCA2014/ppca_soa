%%********************************************************************
%% @title Módulo msbus_config
%% @version 1.0.0
%% @doc Módulo para gerenciamento das configurações
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_config).

-behavior(gen_server). 

-include("../../include/msbus_config.hrl").

%% Server API
-export([start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-export([getConfig/0]).

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
 
getConfig() -> gen_server:call(?SERVER, get_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) -> le_config().
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.
    
handle_call(get_config, _From, State) ->
	{reply, State, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================


le_config() ->
	try
		{ok, Arq} = file:read_file(?CONF_FILE_PATH),
		{ok, Json} = msbus_util:json_decode_as_map(Arq),
		{ok, Hostname} = inet:gethostname(),
		Hostname2 = list_to_binary(Hostname),
		Config = #config{tcp_listen_address = parse_tcp_listen_address(maps:get(<<"tcp_listen_address">>, Json, [<<"127.0.0.1">>])),
						tcp_port        	= maps:get(<<"tcp_port">>, Json, 2301),
						tcp_keepalive   	= msbus_util:binary_to_bool(maps:get(<<"tcp_keepalive">>, Json, <<"true">>)),
						tcp_nodelay     	= msbus_util:binary_to_bool(maps:get(<<"tcp_nodelay">>, Json, <<"true">>)),
						tcp_max_http_worker = maps:get(<<"tcp_max_http_worker">>, Json, 12),
						log_file_dest 		= binary_to_list(maps:get(<<"log_file_dest">>, Json, <<"logs">>)),
						log_file_checkpoint	= maps:get(<<"log_file_checkpoint">>, Json, 6000),
						cat_host_alias		= maps:get(<<"cat_host_alias">>, Json, #{<<"local">> => Hostname2}),
						cat_host_search		= maps:get(<<"cat_host_search">>, Json, <<>>),							
						cat_node_search		= maps:get(<<"cat_node_search">>, Json, <<>>),
						msbus_hostname 		= Hostname2,
						msbus_host	 		= list_to_atom(Hostname)
				},
		valida_port(Config#config.tcp_port),
		valida_max_http_worker(Config#config.tcp_max_http_worker),
		{ok, Config}
	catch
		_Exception:Reason -> {stop, Reason} 
	end.

parse_tcp_listen_address(ListenAddress) ->
	lists:map(fun(L) -> 
					{ok, L2} = inet:parse_address(binary_to_list(L)),
					L2 
			  end, ListenAddress).

valida_port(Value) -> 
	case msbus_consiste:is_range_valido(Value, 1024, 5000) of
		true -> ok;
		false -> erlang:error(invalid_tcp_port)
	end.

valida_max_http_worker(Value) -> 
	case msbus_consiste:is_range_valido(Value, 1, 1000) of
		true -> ok;
		false -> erlang:error(invalid_tcp_max_http_worker)
	end.
	

