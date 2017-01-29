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

-export([getConfig/0, get_name_arq_config/0]).

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
		?DEBUG("Configuration file: ~p", [Config]),
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

% Returns the name of the configuration file
% Locais do arquivo: home do user (.erlangms/node@hostname.conf) ou na pasta priv/conf do barramento
-spec get_name_arq_config() -> list() | {error, atom()}.
get_name_arq_config() ->
	case init:get_argument(home) of
		{ok, [[Home]]} -> 
			NomeArqConfig = lists:concat([Home, "/.erlangms/", node(), ".conf"]),
			?DEBUG("Checking if file ~p exists.", [NomeArqConfig]),
			case file:read_file(NomeArqConfig) of 
				{ok, _} -> NomeArqConfig;
				_ -> ?CONF_FILE_PATH
			end;
		error -> ?CONF_FILE_PATH
	end.

% Lê as configurações do arquivo de configuração
le_config() ->
	NomeArqConfig = get_name_arq_config(),
	io:format("\nLoading configuration file: ~p.\n", [NomeArqConfig]),
	case ems_util:read_file_as_map(NomeArqConfig) of
		{ok, Json} -> 
			try
				parse_config(Json, NomeArqConfig)
			catch 
				_Exception:Reason ->
					io:format("Fail to parse configuration file. Reason: ~p. Running with default settings...\n", [Reason]),
					get_default_config()
			end;
		_Error -> 
			io:format("Configuration file layout is not a valid JSON, running with default settings...\n"),
			get_default_config()
	end.

parse_cat_path_search(Json) ->
	CatPathSearch = maps:get(<<"catalog_path">>, Json, #{}),
	case maps:is_key(<<"ems-bus">>, CatPathSearch) of
		true -> maps:to_list(CatPathSearch);
		false -> [{<<"ems-bus">>, ?CATALOGO_ESB_PATH} | maps:to_list(CatPathSearch)]
	end.
	

parse_config(Json, NomeArqConfig) ->
	{ok, Hostname} = inet:gethostname(),
	Hostname2 = list_to_binary(Hostname),
	#config{ cat_host_alias				= maps:get(<<"host_alias">>, Json, #{<<"local">> => Hostname2}),
			 cat_host_search			= maps:get(<<"host_search">>, Json, <<>>),							
			 cat_node_search			= maps:get(<<"node_search">>, Json, <<>>),
			 cat_path_search			= parse_cat_path_search(Json),
			 cat_disable_services		= maps:get(<<"disable_services">>, Json, []),
			 ems_hostname 				= Hostname2,
			 ems_host	 				= list_to_atom(Hostname),
			 ems_file_dest				= NomeArqConfig,
			 ems_debug					= maps:get(<<"ems_debug">>, Json, false)
		}.

% It generates a default configuration if there is no configuration file
get_default_config() ->
	{ok, Hostname} = inet:gethostname(),
	Hostname2 = list_to_binary(Hostname),
	#config{ cat_host_alias				= #{<<"local">> => Hostname2},
			 cat_host_search			= <<>>,							
			 cat_node_search			= <<>>,
			 cat_path_search			= [{<<"ems-bus">>, ?CATALOGO_ESB_PATH}],
			 cat_disable_services		= [],
			 ems_hostname 				= Hostname2,
			 ems_host	 				= list_to_atom(Hostname),
			 ems_file_dest				= "",
			 ems_debug					= false
		}.


