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
		Config = load_config(),
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

% Returns the configuration file data
% Locais do arquivo: home do user (.erlangms/node@hostname.conf, .erlangms/emsbus.conf) ou na pasta priv/conf do barramento
-spec get_config_data() -> string() | {error, enofile_config}.
get_config_data() ->
	case init:get_argument(home) of
		{ok, [[HomePath]]} -> 
			FileName = lists:concat([HomePath, "/.erlangms/", node(), ".conf"]),
			case file:read_file(FileName) of 
				{ok, Arq} -> 
					?DEBUG("Checking if node file configuration ~p exist: Ok", [FileName]),
					{ok, Arq, FileName};
				_Error -> 
					?DEBUG("Checking if node file configuration ~p exist: No", [FileName]),
					FileName2 = lists:concat([HomePath, "/.erlangms/emsbus.conf"]),
					case file:read_file(FileName2) of 
						{ok, Arq2} -> 
							?DEBUG("Checking if file configuration ~p exist: Ok", [FileName2]),
							{ok, Arq2, FileName2};
						_Error -> 
							?DEBUG("Checking if file configuration ~p exist: No", [FileName2]),
							case file:read_file(?CONF_FILE_PATH) of 
								{ok, Arq3} -> 
									?DEBUG("Checking if global file configuration ~p exist: Ok", [?CONF_FILE_PATH]),
									{ok, Arq3, ?CONF_FILE_PATH};
								_Error -> 
									?DEBUG("Checking if global file configuration ~p exist: No", [?CONF_FILE_PATH]),
									{error, enofile_config}
							end
					end
			end;
		error ->
			case file:read_file(?CONF_FILE_PATH) of 
				{ok, Arq4} -> 
					?DEBUG("Checking if global file configuration ~p exist: Ok", [?CONF_FILE_PATH]),
					{ok, Arq4, ?CONF_FILE_PATH};
				{error, enoent} -> 
					?DEBUG("Checking if global file configuration ~p exist: No", [?CONF_FILE_PATH]),
					{error, enofile_config}
			end
	end.

% Load the configuration file
load_config() ->
	case get_config_data() of
		{ok, ConfigData, FileName} ->
			io:format("\nLoading configuration file: ~p.\n", [FileName]),
			case ems_util:json_decode_as_map(ConfigData) of
				{ok, Json} -> 
					try
						parse_config(Json, FileName)
					catch 
						_Exception:_Reason ->
							ems_logger:format_warn("Fail to parse invalid configuration file, running with default settings...\n"),
							get_default_config()
					end;
				_Error -> 
					ems_logger:format_warn("Configuration file layout is not a valid JSON format, running with default settings...\n"),
					get_default_config()
			end;
		{error, enofile_config} ->
			ems_logger:format_warn("No file configuration exist, running with default settings...\n"),
			get_default_config()
	end.

% parse path_search and return a list
-spec parse_cat_path_search(map()) -> list().
parse_cat_path_search(Json) ->
	CatPathSearch = maps:get(<<"catalog_path">>, Json, #{}),
	case maps:is_key(<<"ems-bus">>, CatPathSearch) of
		true -> maps:to_list(CatPathSearch);
		false -> [{<<"ems-bus">>, ?CATALOGO_ESB_PATH} | maps:to_list(CatPathSearch)]
	end.
	

-spec parse_static_file_path(map()) -> list().
parse_static_file_path(Json) ->
	StaticFilePath = maps:get(<<"static_file_path">>, Json, #{}),
	StaticFilePathList = maps:to_list(StaticFilePath),
	[{K, ems_util:remove_ult_backslash_url(binary_to_list(V))} || {K, V} <- StaticFilePathList].
	
	

parse_config(Json, NomeArqConfig) ->
	{ok, Hostname} = inet:gethostname(),
	Hostname2 = list_to_binary(Hostname),
	#config{ cat_host_alias				= maps:get(<<"host_alias">>, Json, #{<<"local">> => Hostname2}),
			 cat_host_search			= maps:get(<<"host_search">>, Json, <<>>),							
			 cat_node_search			= maps:get(<<"node_search">>, Json, <<>>),
			 cat_path_search			= parse_cat_path_search(Json),
			 static_file_path			= parse_static_file_path(Json),
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
			 static_file_path			= [],
			 ems_hostname 				= Hostname2,
			 ems_host	 				= list_to_atom(Hostname),
			 ems_file_dest				= "",
			 ems_debug					= false
		}.


