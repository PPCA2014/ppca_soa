%%********************************************************************
%% @title Module ems_catalog
%% @version 1.0.0
%% @doc Module responsible for catalog management services
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([new_from_map/2, 
		 get_metadata_json/1,
		 get_table/3]).
		 

-spec get_metadata_json(#service{}) -> binary().
get_metadata_json(#service{id = Id,
						  name = Name,
						  content_type = ContentType,
						  type = Type,
						  url = Url,
						  service = Service,
						  comment = Comment,
						  version = Version,
						  owner = Owner,
						  result_cache = ResultCache,
						  authorization = Authorization,
						  timeout = Timeout,
						  ctrl_path = CtrlPath,
						  ctrl_file = CtrlFile,
						  path = Path,
						  lang = Lang,
						  querystring = Querystring,
						  cache_control = CacheControl}) ->
	iolist_to_binary([<<"{"/utf8>>,
					   <<"\"id\""/utf8>>, <<":"/utf8>>, integer_to_binary(Id), <<","/utf8>>,
					   <<"\"name\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Name, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"content_type\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, ContentType, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"type\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Type, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"service\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Service, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"url\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Url, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"comment\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Comment, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"version\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Version, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"owner\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Owner, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"result_cache\""/utf8>>, <<":"/utf8>>, integer_to_binary(ResultCache), <<","/utf8>>,
					   <<"\"timeout\""/utf8>>, <<":"/utf8>>, integer_to_binary(Timeout), <<","/utf8>>,
					   <<"\"cache_control\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, CacheControl, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"ctrl_path\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, case CtrlPath of
																			undefined -> <<>>;
																			_ -> CtrlPath
																		 end, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"catalog_file\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, case CtrlFile of
																			undefined -> <<>>;
																			_ -> CtrlFile
																		 end, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"path\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, case Path of
																			undefined -> <<>>;
																			_ -> Path
																		 end, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"lang\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Lang, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"authorization\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, erlang:atom_to_binary(Authorization, utf8), <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"querystring\""/utf8>>, <<":"/utf8>>, ems_util:json_encode(Querystring),
				   <<"}"/utf8>>]).



new_service_re(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName, 
			   Type, Enable, Comment, Version, Owner, Async, Querystring, 
			   QtdQuerystringRequired, Host, HostName, ResultCache,
			   Authorization, Node, Lang, Datasource,
			   Debug, SchemaIn, SchemaOut, PoolSize, PoolMax, Properties,
			   Page, PageModule, Timeout, Middleware, CacheControl, 
			   ExpiresMinute, Public, ContentType, Path, Filename,
			   RedirectUrl, ListenAddress, ListenAddress_t, AllowedAddress, 
			   AllowedAddress_t, Port, MaxConnections,
			   IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
			   OAuth2WithCheckConstraint, OAuth2TokenEncrypt, OAuth2AllowClientCredentials,
			   Protocol,
			   CtrlPath, CtrlFile, CtrlModified, StartTimeout, CtrlHash,
			   ServiceExecMetricName, 
			   ServiceResultCacheHitMetricName, 
			   ServiceHostDeniedMetricName,	ServiceAuthDeniedMetricName, 
			   ServiceErrorMetricName, ServiceUnavailableMetricName,
			   ServiceTimeoutMetricName, AuthorizationPublicCheckCredential,
			   HttpMaxContentLength) ->
	PatternKey = ems_util:make_rowid_from_url(Url, Type),
	{ok, Id_re_compiled} = re:compile(PatternKey),
	#service{
				id = Id,
				rowid = Rowid,
				name = Name,
				url = Url,
				type = Type,
				service = Service,
			    module_name = ModuleName,
			    module_name_canonical = ModuleNameCanonical,
			    module = list_to_atom(ModuleName),
			    function_name = FunctionName,
			    function = list_to_atom(FunctionName),
			    use_re = true,
			    id_re_compiled = Id_re_compiled,
			    public = Public,
			    comment = Comment,
			    version = Version,
			    owner = Owner,
				async = Async,
			    querystring = Querystring,
			    qtd_querystring_req = QtdQuerystringRequired,
			    host = Host,
			    host_name = HostName,
			    result_cache = ResultCache,
			    authorization = Authorization,
			    node = Node,
			    page = Page,
			    page_module = PageModule,
			    datasource = Datasource,
			    debug = Debug,
			    lang = Lang,
			    schema_in = SchemaIn,
			    schema_out = SchemaOut,
			    pool_size = PoolSize,
			    pool_max = PoolMax,
			    timeout = Timeout,
			    middleware = Middleware,
			    properties = Properties,
			    cache_control = CacheControl,
			    expires = ExpiresMinute,
			    content_type = ContentType,
			    ctrl_path = CtrlPath,
			    ctrl_file = CtrlFile,
			    path = Path,
			    filename = Filename,
			    redirect_url = RedirectUrl,
			    enable = Enable,
				tcp_listen_address = ListenAddress,
				tcp_listen_address_t = ListenAddress_t,
				tcp_allowed_address = AllowedAddress,
				tcp_allowed_address_t = AllowedAddress_t,
				tcp_max_connections = MaxConnections,
				tcp_port = Port,
				tcp_is_ssl = IsSsl,
				tcp_ssl_cacertfile = SslCaCertFile,
				tcp_ssl_certfile = SslCertFile,
				tcp_ssl_keyfile = SslKeyFile,
				oauth2_with_check_constraint = OAuth2WithCheckConstraint,
				oauth2_token_encrypt = OAuth2TokenEncrypt,
				oauth2_allow_client_credentials = OAuth2AllowClientCredentials,
				protocol = Protocol,
				ctrl_modified = CtrlModified,
				ctrl_hash = CtrlHash,
				start_timeout = StartTimeout,
				service_exec_metric_name = ServiceExecMetricName,
				service_result_cache_hit_metric_name = ServiceResultCacheHitMetricName,
				service_host_denied_metric_name = ServiceHostDeniedMetricName,
				service_auth_denied_metric_name = ServiceAuthDeniedMetricName,
				service_error_metric_name = ServiceErrorMetricName,
				service_unavailable_metric_name = ServiceUnavailableMetricName,
				service_timeout_metric_name = ServiceTimeoutMetricName,
				authorization_public_check_credential = AuthorizationPublicCheckCredential,
				http_max_content_length = HttpMaxContentLength
			}.

new_service(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName,
			Type, Enable, Comment, Version, Owner, Async, Querystring, 
			QtdQuerystringRequired, Host, HostName, ResultCache,
			Authorization, Node, Lang, Datasource, Debug, SchemaIn, SchemaOut, 
			PoolSize, PoolMax, Properties, Page, PageModule, Timeout, Middleware, 
			CacheControl, ExpiresMinute, Public, ContentType, Path, Filename,
			RedirectUrl, ListenAddress, ListenAddress_t, AllowedAddress, AllowedAddress_t, 
			Port, MaxConnections, IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
			OAuth2WithCheckConstraint, OAuth2TokenEncrypt, OAuth2AllowClientCredentials,
			Protocol, 
			CtrlPath, CtrlFile, CtrlModified, StartTimeout, CtrlHash,
			ServiceExecMetricName, ServiceResultCacheHitMetricName, 
			ServiceHostDeniedMetricName, ServiceAuthDeniedMetricName, 
			ServiceErrorMetricName, ServiceUnavailableMetricName,
			ServiceTimeoutMetricName, AuthorizationPublicCheckCredential,
			HttpMaxContentLength) ->
	#service{
				id = Id,
				rowid = Rowid,
				name = Name,
				url = Url,
				type = Type,
				service = Service,
			    module_name = ModuleName,
			    module_name_canonical = ModuleNameCanonical,
			    module = list_to_atom(ModuleName),
			    function_name = FunctionName,
			    function = list_to_atom(FunctionName),
			    public = Public,
			    comment = Comment,
			    version = Version,
			    owner = Owner,
			    async = Async,
			    querystring = Querystring,
			    qtd_querystring_req = QtdQuerystringRequired,
			    host = Host,
			    host_name = HostName,
			    result_cache = ResultCache,
			    authorization = Authorization,
			    node = Node,
			    page = Page,
			    page_module = PageModule,
			    datasource = Datasource,
			    debug = Debug,
			    lang = Lang,
			    schema_in = SchemaIn,
			    schema_out = SchemaOut,
			    pool_size = PoolSize,
			    pool_max = PoolMax,
			    timeout = Timeout,
			    middleware = Middleware,
			    properties = Properties,
			    cache_control = CacheControl,
			    expires = ExpiresMinute,
			    content_type = ContentType,
			    ctrl_path = CtrlPath,
			    ctrl_file = CtrlFile,
			    path = Path,
			    filename = Filename,
			    redirect_url = RedirectUrl,
			    enable = Enable,
				tcp_listen_address = ListenAddress,
				tcp_listen_address_t = ListenAddress_t,
				tcp_allowed_address = AllowedAddress,
				tcp_allowed_address_t = AllowedAddress_t,
				tcp_max_connections = MaxConnections,
				tcp_port = Port,
				tcp_is_ssl = IsSsl,
				tcp_ssl_cacertfile = SslCaCertFile,
				tcp_ssl_certfile = SslCertFile,
				tcp_ssl_keyfile = SslKeyFile,
				oauth2_with_check_constraint = OAuth2WithCheckConstraint,
				oauth2_token_encrypt = OAuth2TokenEncrypt,
				oauth2_allow_client_credentials = OAuth2AllowClientCredentials,
				protocol = Protocol,
				ctrl_modified = CtrlModified,
				ctrl_hash = CtrlHash,
				start_timeout = StartTimeout,
				service_exec_metric_name = ServiceExecMetricName,
				service_result_cache_hit_metric_name = ServiceResultCacheHitMetricName,
				service_host_denied_metric_name = ServiceHostDeniedMetricName,
				service_auth_denied_metric_name = ServiceAuthDeniedMetricName,
				service_error_metric_name = ServiceErrorMetricName,
				service_unavailable_metric_name = ServiceUnavailableMetricName,
				service_timeout_metric_name = ServiceTimeoutMetricName,
				authorization_public_check_credential = AuthorizationPublicCheckCredential,
				http_max_content_length = HttpMaxContentLength
			}.

parse_middleware(null) -> undefined;
parse_middleware(undefined) -> undefined;
parse_middleware(Middleware) -> erlang:binary_to_atom(Middleware, utf8).

parse_ssl_path(FilenameCat, FilenameConfig, StaticFilePathDefault) -> 
	case FilenameConfig of
		undefined -> Filename = ems_util:parse_file_name_path(FilenameCat, StaticFilePathDefault, ?SSL_PATH);
		_ -> Filename = Filename = ems_util:parse_file_name_path(FilenameConfig, StaticFilePathDefault, ?SSL_PATH)
	end,
	case filelib:is_regular(Filename) of
		true -> list_to_binary(Filename);
		false -> 
			ems_logger:error("ems_catalog parse invalid ssl filename ~p.", [Filename]),
			erlang:error(einvalid_ssl_config_service)
	end.
	

compile_page_module(undefined, _, _) -> undefined;
compile_page_module(Page, Rowid, Conf) -> 
	ModuleNamePage =  "page" ++ integer_to_list(Rowid),
	PageFile = ems_util:parse_file_name_path(Page, "", Conf#config.static_file_path),
	case ems_django:compile_file(binary_to_list(PageFile), ModuleNamePage) of
		{ok, PageModule} -> PageModule;
		_ -> erlang:error(einvalid_page_module_service)
	end.

	
-spec parse_datasource(map(), non_neg_integer(), #config{}) -> #service_datasource{} | undefined.
parse_datasource(undefined, _, _) -> undefined;
parse_datasource(M, Rowid, _) when erlang:is_map(M) -> ems_db:create_datasource_from_map(M, Rowid);
parse_datasource(DsName, _Rowid, Conf) -> maps:get(DsName, Conf#config.ems_datasources, undefined).

	
parse_node_service(undefined) -> <<>>;
parse_node_service(<<>>) -> <<>>;
parse_node_service(List) -> List.

%% @doc O host pode ser um alias definido no arquivo de configuração
parse_host_service(<<>>, _,_,_) -> {'', atom_to_list(node())};
parse_host_service(_Host, ModuleName, Node, Conf) ->
	ModuleNameCanonical = [case X of 46 -> 95; _ -> X end || X <- ModuleName], % Troca . por _
	ListHost = case net_adm:host_file() of
		{error, _Reason} -> [Conf#config.ems_host];
		Hosts -> Hosts
	end,
	case erlang:is_list(Node) of
		true  -> ListNode = Node;
		false -> ListNode = [Node]
	end,
	ListHost2 = [case string:tokens(atom_to_list(X), ".") of
					[N, _] -> N;
					[N] -> N
				 end || X <- ListHost],
	ListNode2 = lists:map(fun(X) -> binary_to_list(X) end, ListNode),
	ClusterName = [case X of
						[] -> ModuleNameCanonical ++ "@" ++ Y;
						_  -> ModuleNameCanonical ++ "_" ++ X ++ "@" ++ Y 
				   end || X <- ListNode2, Y <- ListHost2],
	ClusterNode = lists:map(fun(X) -> list_to_atom(X) end, ClusterName),
	{ClusterNode, ClusterName}.


-spec new_from_map(map(), #config{}) -> {ok, #service{}} | {error, atom()}.
new_from_map(Map, Conf = #config{cat_enable_services = EnableServices,
								 cat_disable_services = DisableServices,
								 cat_enable_services_owner = EnableServicesOwner,
								 cat_disable_services_owner = DisableServicesOwner,
								 ems_result_cache = ResultCacheDefault,
								 ems_hostname = HostNameDefault,
								 authorization = AuthorizationDefault,
								 oauth2_with_check_constraint = Oauth2WithCheckConstraintDefault,
								 static_file_path = StaticFilePathDefault,
								 tcp_listen_address_t = TcpListenAddressDefault,
								 tcp_allowed_address = TcpAllowedAddressDefault,
								 cat_node_search = CatNodeSearchDefault,
								 cat_host_search = CatHostSearchDefault,
								 ssl_cacertfile = SslCaCertFileDefault,
								 ssl_certfile = SslCertFileDefault,
								 ssl_keyfile = SslKeyFileDefault,
								 http_max_content_length = HttpMaxContentLengthDefault}) ->
	try
		Name = ems_util:parse_name_service(maps:get(<<"name">>, Map)),
		Owner = maps:get(<<"owner">>, Map, <<>>),
		% habilitar serviços
		Enable0 = ems_util:parse_bool(maps:get(<<"enable">>, Map, true)),
		case lists:member(Owner, EnableServicesOwner) of
			true -> Enable1 = true;
			false -> Enable1 = Enable0
		end,
		case lists:member(Name, EnableServices) of
			true -> Enable2 = true;
			false -> Enable2 = Enable1
		end,
		% desabilitar serviços
		case lists:member(Owner, DisableServicesOwner) of
			true -> Enable3 = false;
			false -> Enable3 = Enable2
		end,
		case lists:member(Name, DisableServices) of
			true -> Enable = false;
			false -> Enable = Enable3
		end,
		UseRE = ems_util:parse_bool(maps:get(<<"use_re">>, Map, false)),
		case UseRE of
			true -> Url2 = maps:get(<<"url">>, Map);
			false -> Url2 = ems_util:parse_url_service(maps:get(<<"url">>, Map))
		end,
		Type = ems_util:parse_type_service(maps:get(<<"type">>, Map, <<"GET">>)),
		ServiceImpl = maps:get(<<"service">>, Map),
		{ModuleName, ModuleNameCanonical, FunctionName} = ems_util:parse_service_service(ServiceImpl),
		Comment = ?UTF8_STRING(maps:get(<<"comment">>, Map, <<>>)),
		Version = maps:get(<<"version">>, Map, <<"1.0.0">>),
		Async = ems_util:parse_bool(maps:get(<<"async">>, Map, false)),
		Rowid = ems_util:make_rowid(Url2),
		Id = maps:get(<<"id">>, Map, Rowid), % catálogos internos vão usar rowid como chave primária
		Lang = ems_util:parse_lang(maps:get(<<"lang">>, Map, <<>>)),
		Ds = maps:get(<<"datasource">>, Map, undefined),
		Datasource = parse_datasource(Ds, Rowid, Conf),
		case Type of
			<<"GET">> -> ResultCache = ems_util:parse_result_cache(maps:get(<<"result_cache">>, Map, ResultCacheDefault));
			_ -> ResultCache = 0
		end,
		Authorization = ems_util:parse_authorization_type(maps:get(<<"authorization">>, Map, AuthorizationDefault)),
		OAuth2WithCheckConstraint = ems_util:parse_bool(maps:get(<<"oauth2_with_check_constraint">>, Map, Oauth2WithCheckConstraintDefault)),
		OAuth2TokenEncrypt = ems_util:parse_bool(maps:get(<<"oauth2_token_encrypt">>, Map, false)),
		OAuth2AllowClientCredentials = ems_util:parse_bool(maps:get(<<"oauth2_allow_client_credentials">>, Map, false)),
		AuthorizationPublicCheckCredential = ems_util:parse_bool(maps:get(<<"authorization_public_check_credential">>, Map, false)),
		Debug = ems_util:parse_bool(maps:get(<<"debug">>, Map, false)),
		SchemaIn = maps:get(<<"schema_in">>, Map, null),
		SchemaOut = maps:get(<<"schema_out">>, Map, null),
		PoolSize = ems_config:getConfig(<<"pool_size">>, Name, maps:get(<<"pool_size">>, Map, 1)),
		PoolMax0 = ems_config:getConfig(<<"pool_max">>, Name, maps:get(<<"pool_max">>, Map, 1)),
		% Ajusta o pool_max para o valor de pool_size se for menor
		case PoolMax0 < PoolSize of
			true -> PoolMax = PoolSize;
			false -> PoolMax = PoolMax0
		end,
		Timeout = ems_util:parse_timeout(maps:get(<<"timeout">>, Map, ?SERVICE_TIMEOUT), ?SERVICE_MAX_TIMEOUT),
		Middleware = parse_middleware(maps:get(<<"middleware">>, Map, undefined)),
		CacheControl = maps:get(<<"cache_control">>, Map, ?CACHE_CONTROL_1_SECOND),
		ExpiresMinute = maps:get(<<"expires_minute">>, Map, 1),
		Public = ems_util:parse_bool(maps:get(<<"public">>, Map, true)),
		ContentType = maps:get(<<"content_type">>, Map, ?CONTENT_TYPE_JSON),
		CtrlPath = maps:get(<<"ctrl_path">>, Map, <<>>),
		CtrlFile = maps:get(<<"ctrl_file">>, Map, <<>>),
		Path = ems_util:parse_file_name_path(maps:get(<<"path">>, Map, CtrlPath), StaticFilePathDefault, undefined),
		Filename = ems_util:parse_file_name_path(maps:get(<<"filename">>, Map, undefined), StaticFilePathDefault, undefined),
		RedirectUrl = maps:get(<<"redirect_url">>, Map, undefined),
		Protocol = maps:get(<<"protocol">>, Map, <<>>),
		ListenAddress = maps:get(<<"tcp_listen_address">>, Map, TcpListenAddressDefault),
		ListenAddress_t = ems_util:parse_tcp_listen_address(ListenAddress),
		AllowedAddress = ems_util:parse_allowed_address(maps:get(<<"tcp_allowed_address">>, Map, TcpAllowedAddressDefault)),
		AllowedAddress_t = ems_util:parse_allowed_address_t(AllowedAddress),
		MaxConnections = maps:get(<<"tcp_max_connections">>, Map, ?HTTP_MAX_CONNECTIONS),
		Port = ems_util:parse_tcp_port(ems_config:getConfig(<<"tcp_port">>, Name, maps:get(<<"tcp_port">>, Map, undefined))),
		Port = ems_util:parse_tcp_port(ems_config:getConfig(<<"tcp_port">>, Name, maps:get(<<"tcp_port">>, Map, undefined))),
		HttpMaxContentLength = ems_util:parse_range(maps:get(<<"http_max_content_length">>, Map, HttpMaxContentLengthDefault), 0, ?HTTP_MAX_CONTENT_LENGTH_BY_SERVICE),
		Ssl = maps:get(<<"tcp_ssl">>, Map, undefined),
		case Ssl of
			undefined ->
				IsSsl = false,
				SslCaCertFile = undefined,
				SslCertFile = undefined,
				SslKeyFile = undefined;
			_ ->
				IsSsl = true,
				SslCaCertFile = parse_ssl_path(maps:get(<<"cacertfile">>, Ssl, SslCaCertFileDefault), SslCaCertFileDefault, StaticFilePathDefault),
				SslCertFile = parse_ssl_path(maps:get(<<"certfile">>, Ssl, SslCertFileDefault), SslCertFileDefault, StaticFilePathDefault),
				SslKeyFile = parse_ssl_path(maps:get(<<"keyfile">>, Ssl, SslKeyFileDefault), SslKeyFileDefault, StaticFilePathDefault)
		end,
		case Lang of
			<<"erlang">> -> 
				Node = <<>>,
				Mapost = '',
				MapostName = HostNameDefault,
				ems_util:compile_modulo_erlang(Path, ModuleNameCanonical);
			_ ->	
				Node = parse_node_service(maps:get(<<"node">>, Map, CatNodeSearchDefault)),
				{Mapost, MapostName} = parse_host_service(maps:get(<<"host">>, Map, CatHostSearchDefault), ModuleName, Node, Conf)
		end,
		{Querystring, QtdQuerystringRequired} = ems_util:parse_querystring_def(maps:get(<<"querystring">>, Map, [])),
		Page = maps:get(<<"page">>, Map, undefined),
		PageModule = compile_page_module(Page, Rowid, Conf),
		CtrlModified = maps:get(<<"ctrl_modified">>, Map, undefined),
		CtrlHash = erlang:phash2(Map),
		StartTimeout = maps:get(<<"start_timeout">>, Map, undefined),
		ServiceExecMetricName = list_to_atom("service_" ++ integer_to_list(Rowid) ++ "_exec"),
		ServiceResultCacheHitMetricName = list_to_atom("service_" ++ integer_to_list(Rowid) ++ "_result_cache_hit"),
		ServiceHostDeniedMetricName = list_to_atom("service_" ++ integer_to_list(Rowid) ++ "_host_denied"),
		ServiceAuthDeniedMetricName = list_to_atom("service_" ++ integer_to_list(Rowid) ++ "_auth_denied"),
		ServiceErrorMetricName = list_to_atom("service_" ++ integer_to_list(Rowid) ++ "_error"),
		ServiceUnavailableMetricName = list_to_atom("service_" ++ integer_to_list(Rowid) ++ "_unavailable"),
		ServiceTimeoutMetricName = list_to_atom("service_" ++ integer_to_list(Rowid) ++ "_timeout"),
		case UseRE of
			true -> 
				case Type of
					<<"GET">> -> 
						Service = new_service_re(Rowid, Id, Name, Url2, 
												   ServiceImpl,
												   ModuleName, 
												   ModuleNameCanonical,
												   FunctionName, Type, Enable, Comment, 
												   Version, Owner, Async, 
												   Querystring, QtdQuerystringRequired,
												   Mapost, MapostName, ResultCache,
												   Authorization, Node, Lang,
												   Datasource, Debug, SchemaIn, SchemaOut, 
												   PoolSize, PoolMax, Map, Page, 
												   PageModule, Timeout, 
												   Middleware, CacheControl, ExpiresMinute, 
												   Public, ContentType, Path, Filename,
												   RedirectUrl, ListenAddress, ListenAddress_t, AllowedAddress, 
												   AllowedAddress_t, Port, MaxConnections,
												   IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
												   OAuth2WithCheckConstraint, OAuth2TokenEncrypt, OAuth2AllowClientCredentials,
												   Protocol,
												   CtrlPath, CtrlFile, CtrlModified, StartTimeout, CtrlHash,
												   ServiceExecMetricName, ServiceResultCacheHitMetricName, 
												   ServiceHostDeniedMetricName,	ServiceAuthDeniedMetricName, 
												   ServiceErrorMetricName, ServiceUnavailableMetricName, 
												   ServiceTimeoutMetricName, AuthorizationPublicCheckCredential,
												   HttpMaxContentLength),
						{ok, Service};
					_ -> 
						erlang:error(einvalid_re_service)
				end;
			false -> 
				Service = new_service(Rowid, Id, Name, Url2, 
										ServiceImpl,
										ModuleName,
										ModuleNameCanonical,
										FunctionName, Type, Enable, Comment,
										Version, Owner, Async, 
										Querystring, QtdQuerystringRequired,
										Mapost, MapostName, ResultCache,
										Authorization, Node, Lang,
										Datasource, Debug, SchemaIn, SchemaOut, 
										PoolSize, PoolMax, Map, Page, 
										PageModule, Timeout, 
										Middleware, CacheControl, 
										ExpiresMinute, Public, 
										ContentType, Path, Filename,
										RedirectUrl, ListenAddress, ListenAddress_t, AllowedAddress, 
										AllowedAddress_t, Port, MaxConnections,
										IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
										OAuth2WithCheckConstraint, OAuth2TokenEncrypt, OAuth2AllowClientCredentials,
										Protocol,
										CtrlPath, CtrlFile, CtrlModified, StartTimeout, CtrlHash,
										ServiceExecMetricName, ServiceResultCacheHitMetricName, 
										ServiceHostDeniedMetricName, ServiceAuthDeniedMetricName, 
										ServiceErrorMetricName, ServiceUnavailableMetricName, 
										ServiceTimeoutMetricName, AuthorizationPublicCheckCredential,
										HttpMaxContentLength),
				{ok, Service}
		end
	catch
		_Exception:Reason -> 
			ems_db:inc_counter(edata_loader_invalid_catalog),
			ems_logger:warn("ems_catalog parse invalid service specification: ~p\n\t~p.\n", [Reason, Map]),
			{error, Reason}
	end.

-spec get_table(#service{}, boolean(), fs | db) -> catalog_get_db | catalog_post_db | catalog_put_db | catalog_delete_db | catalog_options_db |
												   catalog_get_fs | catalog_post_fs | catalog_put_fs | catalog_delete_fs | catalog_options_fs.
get_table(<<"GET">>, false, db) -> catalog_get_db;
get_table(<<"GET">>, true, db) -> catalog_re_db;
get_table(<<"POST">>, _, db) -> catalog_post_db;
get_table(<<"PUT">>, _, db) -> catalog_put_db;
get_table(<<"DELETE">>, _, db) -> catalog_delete_db;
get_table(<<"OPTIONS">>, _, db) -> catalog_options_db;
get_table(<<"KERNEL">>, _, db) -> catalog_kernel_db;
get_table(<<"GET">>, false, fs) -> catalog_get_fs;
get_table(<<"GET">>, true, fs) -> catalog_re_fs;
get_table(<<"POST">>, _, fs) -> catalog_post_fs;
get_table(<<"PUT">>, _, fs) -> catalog_put_fs;
get_table(<<"DELETE">>, _, fs) -> catalog_delete_fs;
get_table(<<"OPTIONS">>, _, fs) -> catalog_options_fs;
get_table(<<"KERNEL">>, _, fs) -> catalog_kernel_fs.

