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

% Retorna o nome do arquivo de configuração
% Locais do arquivo: home do user (.erlangms/<<node>>.conf) ou na pasta conf do barramento
get_nome_arq_config() ->
	case init:get_argument(home) of
		{ok, [[Home]]} -> 
			NomeArqConfig = lists:concat([Home, "/.erlangms/", node(), ".conf"]),
			% O arquivo no home deve existir!
			case file:read_file(NomeArqConfig) of 
				{ok, _} -> NomeArqConfig;
				_ -> ?CONF_FILE_PATH
			end;
		error -> ?CONF_FILE_PATH
	end.

le_config() ->
	NomeArqConfig = get_nome_arq_config(),
	{ok, Hostname} = inet:gethostname(),
	Hostname2 = list_to_binary(Hostname),
	case file:read_file(NomeArqConfig) of
		{ok, ArqConfig} -> 
			% Lê as configurações do arquivo de configuração
			{ok, Json} = msbus_util:json_decode_as_map(ArqConfig),
			Listen_address = maps:get(<<"tcp_listen_address">>, Json, [<<"127.0.0.1">>]),
			Listen_address_t = parse_tcp_listen_address(Listen_address),
			Allowed_address = msbus_util:binlist_to_list(maps:get(<<"tcp_allowed_address">>, Json, [])),
			Allowed_address_t = parse_allowed_address(Allowed_address),
			Config = #config{tcp_listen_address 		= Listen_address,
							 tcp_listen_address_t 		= Listen_address_t,
							 tcp_port        			= parse_tcp_port(maps:get(<<"tcp_port">>, Json, 2301)),
							 tcp_keepalive   			= parse_keepalive(maps:get(<<"tcp_keepalive">>, Json, true)),
							 tcp_nodelay     			= msbus_util:binary_to_bool(maps:get(<<"tcp_nodelay">>, Json, true)),
							 tcp_max_http_worker 		= maps:get(<<"tcp_max_http_worker">>, Json, ?MAX_HTTP_WORKER),
							 tcp_allowed_address		= Allowed_address,
							 tcp_allowed_address_t 		= Allowed_address_t,
							 log_file_dest 				= binary_to_list(maps:get(<<"log_file_dest">>, Json, <<"logs">>)),
							 log_file_checkpoint		= maps:get(<<"log_file_checkpoint">>, Json, ?LOG_FILE_CHECKPOINT),
							 cat_host_alias				= maps:get(<<"cat_host_alias">>, Json, #{<<"local">> => Hostname2}),
							 cat_host_search			= maps:get(<<"cat_host_search">>, Json, <<>>),							
							 cat_node_search			= maps:get(<<"cat_node_search">>, Json, <<>>),
							 msbus_hostname 			= Hostname2,
							 msbus_host	 				= list_to_atom(Hostname),
							 config_file_dest			= NomeArqConfig,
							 modo_debug					= maps:get(<<"modo_debug">>, Json, false)
						},
			valida_port(Config#config.tcp_port),
			valida_max_http_worker(Config#config.tcp_max_http_worker);
		{error, _Reason} ->
			% Gera uma configuração default se não existir o arquivo de configuração
			Config = #config{tcp_listen_address 		= ["127.0.0.1"],
							 tcp_listen_address_t 		= [inet:parse_address("127.0.0.1")],
							 tcp_port        			= 2301,
							 tcp_keepalive   			= true,
							 tcp_nodelay     			= true,
							 tcp_max_http_worker 		= 12,
							 tcp_allowed_address		= [],
							 log_file_dest 				= <<"logs">>,
							 log_file_checkpoint		= ?LOG_FILE_CHECKPOINT,
							 cat_host_alias				= #{<<"local">> => Hostname2},
							 cat_host_search			= <<>>,							
							 cat_node_search			= <<>>,
							 msbus_hostname 			= Hostname2,
							 msbus_host	 				= list_to_atom(Hostname),
							 config_file_dest			= NomeArqConfig,
							 modo_debug					= false
						}
	 end,
	{ok, Config}.

parse_keepalive(Keepalive) ->
	io:format("valor eh ~p\n\n", [Keepalive]),
	io:format("valor eh ~p\n\n", [msbus_util:binary_to_bool(Keepalive)]),
	msbus_util:binary_to_bool(Keepalive).

parse_tcp_port(<<Port/binary>>) -> binary_to_list(Port);		
parse_tcp_port(Port) -> Port.	

parse_tcp_listen_address(ListenAddress) ->
	lists:map(fun(IP) -> 
					{ok, L2} = inet:parse_address(binary_to_list(IP)),
					L2 
			  end, ListenAddress).

parse_allowed_address(AllowedAddress) ->
	lists:map(fun(IP) -> 
					msbus_http_util:mask_ipaddress_to_tuple(IP)
			  end, AllowedAddress).

valida_port(Value) -> 
	case msbus_consiste:is_range_valido(Value, 1024, 5000) of
		true -> ok;
		false -> erlang:error(invalid_tcp_port)
	end.

valida_max_http_worker(Value) -> 
	case msbus_consiste:is_range_valido(Value, 1, ?MAX_HTTP_WORKER_RANGE) of
		true -> ok;
		false -> erlang:error(invalid_tcp_max_http_worker)
	end.
	

