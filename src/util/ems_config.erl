%%********************************************************************
%% @title Module ems_config
%% @version 1.0.0
%% @doc Module for configuration management
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_config).

-behavior(gen_server). 

-include("../../include/ems_config.hrl").

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
 
init([]) -> 
	try
		Config = le_config(),
		{ok, Config}
	catch _Exception: Reason ->
		{stop, Reason}
	end.

    
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

% Retorna o name do arquivo de configuração
% Locais do arquivo: home do user (.erlangms/node@hostname.conf) ou na pasta priv/conf do barramento
get_name_arq_config() ->
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

% Lê as configurações do arquivo de configuração
le_config() ->
	NomeArqConfig = get_name_arq_config(),
	case ems_util:read_file_as_map(NomeArqConfig) of
		{ok, Json} -> 
			try
				{ok, Hostname} = inet:gethostname(),
				Hostname2 = list_to_binary(Hostname),
				Listen_address = ems_util:binlist_to_list(maps:get(<<"tcp_listen_address">>, Json, [<<"127.0.0.1">>])),
				Listen_address_t = parse_tcp_listen_address(Listen_address),
				Allowed_address = ems_util:binlist_to_list(maps:get(<<"tcp_allowed_address">>, Json, [])),
				Allowed_address_t = parse_allowed_address(Allowed_address),
				#config{tcp_listen_address 		= Listen_address,
						 tcp_listen_address_t 		= Listen_address_t,
						 tcp_port        			= parse_tcp_port(maps:get(<<"tcp_port">>, Json, 2301)),
						 tcp_keepalive   			= parse_keepalive(maps:get(<<"tcp_keepalive">>, Json, true)),
						 tcp_nodelay     			= ems_util:binary_to_bool(maps:get(<<"tcp_nodelay">>, Json, true)),
						 tcp_max_http_worker 		= parse_max_http_worker(maps:get(<<"tcp_max_http_worker">>, Json, ?MAX_HTTP_WORKER)),
						 tcp_allowed_address		= Allowed_address,
						 tcp_allowed_address_t 		= Allowed_address_t,
						 log_file_dest 				= binary_to_list(maps:get(<<"log_file_dest">>, Json, <<"logs">>)),
						 log_file_checkpoint		= maps:get(<<"log_file_checkpoint">>, Json, ?LOG_FILE_CHECKPOINT),
						 cat_host_alias				= maps:get(<<"cat_host_alias">>, Json, #{<<"local">> => Hostname2}),
						 cat_host_search			= maps:get(<<"cat_host_search">>, Json, <<>>),							
						 cat_node_search			= maps:get(<<"cat_node_search">>, Json, <<>>),
						 ems_hostname 				= Hostname2,
						 ems_host	 				= list_to_atom(Hostname),
						 ems_file_dest				= NomeArqConfig,
						 ems_debug					= maps:get(<<"ems_debug">>, Json, false)
					}
			catch 
				_Exception:Reason ->
					io:format("Não foi possível processar o arquivo de configuração. Motivo: ~p.\n", [Reason]),
					io:format("Executando o barramento com configurações padrão...\n"),
					get_default_config()
			end;
		{error, enojsonformat} -> 
			io:format("Layout do arquivo de configuração não é um JSON válido!\n"),
			io:format("Executando o barramento com configurações padrão...\n"),
			get_default_config();
		{error, _Reason} -> 
			io:format("Arquivo de configuração inexistente ou inacessível!\n"),
			io:format("Executando o barramento com configurações padrão...\n"),
			get_default_config()
	end.

% Gera uma configuração default se não existir o arquivo de configuração
get_default_config() ->
	{ok, Hostname} = inet:gethostname(),
	Hostname2 = list_to_binary(Hostname),
	#config{tcp_listen_address 		= ["127.0.0.1"],
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
			 ems_hostname 				= Hostname2,
			 ems_host	 				= list_to_atom(Hostname),
			 ems_file_dest				= "",
			 ems_debug					= false
		}.


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

parse_tcp_listen_address(ListenAddress) ->
	lists:map(fun(IP) -> 
					{ok, L2} = inet:parse_address(IP),
					L2 
			  end, ListenAddress).

parse_allowed_address(AllowedAddress) ->
	lists:map(fun(IP) -> 
					ems_http_util:mask_ipaddress_to_tuple(IP)
			  end, AllowedAddress).

	

