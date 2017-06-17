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
-include("../../include/ems_schema.hrl").

%% Server API
-export([start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-export([getConfig/0, getConfig/3, get_port_offset/1]).

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

-spec getConfig(binary(), binary(), any()) -> any().
getConfig(ParamName, ServiceName, Default) -> gen_server:call(?SERVER, {get_config, ParamName, ServiceName, Default}).

-spec get_port_offset(#service{}) -> non_neg_integer() | undefined.
get_port_offset(S = #service{tcp_port = Port, name = ServiceName}) ->
	Port2 = gen_server:call(?SERVER, {use_port_offset, ServiceName, Port}),
 	S#service{tcp_port = Port2}.



%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) -> 
	try
		ets:new(debug_ets, [set, named_table, public, {read_concurrency, true}, {write_concurrency, false}]),
		ets:insert(debug_ets, {debug, false}),
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
	{reply, State, State};

handle_call({get_config, ParamName, ServiceName, Default}, _From, State = #config{params = Params}) ->
	ParamName2 = iolist_to_binary([ServiceName, <<".">>, ParamName]),
	Result = maps:get(ParamName2, Params, Default),
	{reply, Result, State};

handle_call({use_port_offset, <<>>}, _From, State) ->
	{reply, undefined, State};
handle_call({use_port_offset, ServiceName, DefaultPort}, _From, State = #config{params = Params}) ->
	ParamName = iolist_to_binary([ServiceName, <<"_port_offset">>]),
	Port = maps:get(ParamName, Params, DefaultPort) ,
	Params2 = maps:put(ParamName, Port + 1, Params),
	State2 = State#config{params = Params2},
	{reply, Port, State2}.

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
					?DEBUG("ems_config checking if node file configuration ~p exist: Ok", [FileName]),
					{ok, Arq, FileName};
				_Error -> 
					?DEBUG("ems_config checking if node file configuration ~p exist: No", [FileName]),
					FileName2 = lists:concat([HomePath, "/.erlangms/emsbus.conf"]),
					case file:read_file(FileName2) of 
						{ok, Arq2} -> 
							?DEBUG("ems_config checking if file configuration ~p exist: Ok", [FileName2]),
							{ok, Arq2, FileName2};
						_Error -> 
							?DEBUG("ems_config checking if file configuration ~p exist: No", [FileName2]),
							case file:read_file(?CONF_FILE_PATH) of 
								{ok, Arq3} -> 
									?DEBUG("ems_config checking if global file configuration ~p exist: Ok", [?CONF_FILE_PATH]),
									{ok, Arq3, ?CONF_FILE_PATH};
								_Error -> 
									?DEBUG("ems_config checking if global file configuration ~p exist: No", [?CONF_FILE_PATH]),
									{error, enofile_config}
							end
					end
			end;
		error ->
			case file:read_file(?CONF_FILE_PATH) of 
				{ok, Arq4} -> 
					?DEBUG("ems_config checking if global file configuration ~p exist: Ok", [?CONF_FILE_PATH]),
					{ok, Arq4, ?CONF_FILE_PATH};
				{error, enoent} -> 
					?DEBUG("ems_config checking if global file configuration ~p exist: No", [?CONF_FILE_PATH]),
					{error, enofile_config}
			end
	end.

print_config_settings(Json = #config{ems_debug = true, config_file = FileName}) ->
	ems_logger:format_alert("\nems_config loading configuration file ~p...\n", [FileName]),
	ems_logger:format_debug("~p\n", [Json]);
print_config_settings(#config{ems_debug = false, config_file = FileName}) ->
	ems_logger:format_alert("\nems_config loading configuration file ~p...\n", [FileName]).

% Load the configuration file
load_config() ->
	case get_config_data() of
		{ok, ConfigData, FileName} ->
			case ems_util:json_decode_as_map(ConfigData) of
				{ok, Json} -> 
					try
						Result = parse_config(Json, FileName),
						print_config_settings(Result),
						Result
					catch 
						_Exception:Reason ->
							ems_logger:format_warn("\nems_config parse invalid configuration file ~p. Reason: ~p. Running with default settings.\n", [FileName, Reason]),
							get_default_config()
					end;
				_Error -> 
					ems_logger:format_warn("\nems_config parse invalid configuration file ~p. Running with default settings.\n", [FileName]),
					get_default_config()
			end;
		{error, enofile_config} ->
			ems_logger:format_warn("ems_config has no file configuration. Running with default settings.\n"),
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
	StaticFilePathList2 = [{<<"login_path">>, list_to_binary(?STATIC_FILE_PATH ++ "/login")} | StaticFilePathList],
	StaticFilePathList3 = [{<<"www_path">>, list_to_binary(?STATIC_FILE_PATH)} | StaticFilePathList2],
	[{K, ems_util:remove_ult_backslash_url(binary_to_list(V))} || {K, V} <- StaticFilePathList3].
	

parse_datasources([], _, Result) -> maps:from_list(Result);
parse_datasources([DsName|T], Datasources, Result) ->
	M = maps:get(DsName, Datasources),
	Ds = ems_db:create_datasource_from_map(M),
	parse_datasources(T, Datasources, [{DsName, Ds} | Result]).
								
	
parse_datasources(Json) ->
	Datasources = maps:get(<<"datasources">>, Json, #{}),
	parse_datasources(maps:keys(Datasources), Datasources, []).
	
	
parse_tcp_allowed_address(undefined) -> all;
parse_tcp_allowed_address([<<"*.*.*.*">>]) -> all;
parse_tcp_allowed_address(V) -> V.


parse_config(Json, NomeArqConfig) ->
	{ok, Hostname} = inet:gethostname(),
	Hostname2 = list_to_binary(Hostname),
	#config{ cat_host_alias				= maps:get(<<"host_alias">>, Json, #{<<"local">> => Hostname2}),
			 cat_host_search			= maps:get(<<"host_search">>, Json, <<>>),							
			 cat_node_search			= maps:get(<<"node_search">>, Json, <<>>),
			 cat_path_search			= parse_cat_path_search(Json),
			 static_file_path			= parse_static_file_path(Json),
			 cat_disable_services		= maps:get(<<"disable_services">>, Json, []),
			 cat_enable_services		= maps:get(<<"enable_services">>, Json, []),
			 ems_hostname 				= Hostname2,
			 ems_host	 				= list_to_atom(Hostname),
			 ems_file_dest				= NomeArqConfig,
			 ems_debug					= parse_bool(maps:get(<<"debug">>, Json, false)),
			 ems_result_cache			= maps:get(<<"result_cache">>, Json, ?TIMEOUT_DISPATCHER_CACHE),
			 ems_datasources			= parse_datasources(Json),
			 tcp_allowed_address		= parse_tcp_allowed_address(maps:get(<<"tcp_allowed_address">>, Json, all)),
			 tcp_listen_address			= maps:get(<<"tcp_listen_address">>, Json, [<<"0.0.0.0">>]),
			 authorization			    = ems_http_util:parse_authorization_type(maps:get(<<"authorization">>, Json, ?AUTHORIZATION_TYPE_DEFAULT)),
			 oauth2_with_check_constraint = parse_bool(maps:get(<<"oauth2_with_check_constraint">>, Json, false)),
			 config_file			    = NomeArqConfig,
			 params						= Json
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
			 cat_enable_services		= [],
			 static_file_path			= [],
			 ems_hostname 				= Hostname2,
			 ems_host	 				= list_to_atom(Hostname),
			 ems_file_dest				= "",
			 ems_debug					= false,
			 ems_result_cache			= ?TIMEOUT_DISPATCHER_CACHE,
			 ems_datasources			= #{},
			 tcp_allowed_address		= all,
			 tcp_listen_address			= [<<"0.0.0.0">>],
			 authorization				= <<"oauth2">>,
			 oauth2_with_check_constraint = false,
			 config_file			    = undefined,
			 params						= #{}
		}.

parse_bool(<<"true">>) -> true;
parse_bool(<<"false">>) -> false;
parse_bool(true) -> true;
parse_bool(false) -> false;
parse_bool(_) -> false.

