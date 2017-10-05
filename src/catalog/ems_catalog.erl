%%********************************************************************
%% @title Module ems_catalog
%% @version 1.0.0
%% @doc Module responsible for catalog management services
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([lookup/1, lookup/2, get_querystring/2, 
		 get_metadata_json/1, 
		 list_kernel_catalog/0, list_re_catalog/0,
		 new_service/54, new_service_re/54]).


lookup(Request = #request{type = Type, rowid = Rowid, params_url = ParamsMap}) ->	
	case Type of
		"GET" -> EtsLookup = ets_get;
		"POST" -> EtsLookup = ets_post;
		"PUT" -> EtsLookup = ets_put;
		"DELETE" -> EtsLookup = ets_delete;
		"OPTIONS" -> EtsLookup = ets_options;
		"HEAD" -> EtsLookup = ets_get;
		"INFO" -> EtsLookup = ets_get
	end,
	case ets:lookup(EtsLookup, Rowid) of
		[] -> % is regular expression??
			case lookup_re(Request, list_re_catalog()) of
				{error, enoent} = Error -> Error;
				{Service, ParamsMapRE} -> 
					Querystring = processa_querystring(Service, Request),
					{Service, ParamsMapRE, Querystring}
			end;
		[{_Rowid, Service}] -> 
			Querystring = processa_querystring(Service, Request),
			{Service, ParamsMap, Querystring}
	end.

lookup(Method, Uri) ->
	case ems_util:encode_request(Method, Uri) of
		{ok, Request} -> lookup(Request);
		Error -> Error
	end.

list_kernel_catalog() ->
	case ets:lookup(ets_ems_catalog, cat) of
		[] -> {error, enoent};
		[{cat, {_, _, CatKernel}}] -> CatKernel
	end.

list_re_catalog() ->
	case ets:lookup(ets_ems_catalog, cat) of
		[] -> {error, enoent};
		[{cat, {_, CatRE, _}}] -> CatRE
	end.
    
    
%%====================================================================
%% Funções internas
%%====================================================================


get_querystring(<<QueryName/binary>>, Servico) ->	
	[Query] = [Q || Q <- maps:get(<<"querystring">>, Servico, <<>>), Q#service.comment == QueryName],
	Query.

processa_querystring(Service, Request) ->
	%% Querystrings do módulo ems_static_file_service e ems_options_service não são processados.
	QuerystringUser = Request#request.querystring_map,
	case Service#service.module of
		ems_static_file_service -> QuerystringUser;
		ems_options_service -> QuerystringUser;
		_ ->
			QuerystringServico = Service#service.querystring,
			case QuerystringUser =:= #{} of
				true -> 
					case QuerystringServico =:= [] of
						true -> QuerystringUser;
						false -> valida_querystring(QuerystringServico, QuerystringUser, [])
					end;
				false -> 
					case QuerystringServico =:= [] of
						true -> #{};
						false -> valida_querystring(QuerystringServico, QuerystringUser, [])
					end
			end
	end.

valida_querystring([], _QuerystringUser, QuerystringList) -> maps:from_list(QuerystringList);
valida_querystring([H|T], QuerystringUser, QuerystringList) ->
	%% Verifica se encontra a query na querystring do usuário
	NomeQuery = maps:get(<<"name">>, H),
	case maps:find(NomeQuery, QuerystringUser) of
		{ok, Value} -> 
			valida_querystring(T, QuerystringUser, [{NomeQuery, Value} | QuerystringList]);
		error ->
			%% se o usuário não informou a querystring, verifica se tem valor default na definição do serviço
			case maps:get(<<"default">>, H, enoent) of
				enoent -> [];
				Value -> valida_querystring(T, QuerystringUser, [{NomeQuery, Value} | QuerystringList])
			end
	end.

lookup_re(_, []) -> {error, enoent};
lookup_re(Request = #request{type = Type, url = Url}, [H|T]) ->
	try
		RE = H#service.id_re_compiled,
		PatternKey = ems_util:make_rowid_from_url(Url, Type),
		case re:run(PatternKey, RE, [{capture,all_names,binary}]) of
			match -> {H, #{}};
			{match, Params} -> 
				{namelist, ParamNames} = re:inspect(RE, namelist),
				ParamsMap = maps:from_list(lists:zip(ParamNames, Params)),
				{H, ParamsMap};
			nomatch -> lookup_re(Request, T);
			{error, _ErrType} -> {error, enoent}
		end
	catch 
		_Exception:_Reason -> {error, enoent}
	end.


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
						  catalog_path = CatalogPath,
						  catalog_file = CatalogFile,
						  path = Path,
						  lang = Lang,
						  querystring = Querystring,
						  cache_control = CacheControl}) ->
	iolist_to_binary([<<"{"/utf8>>,
					   <<"\"id\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Id, <<"\""/utf8>>, <<","/utf8>>,
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
					   <<"\"catalog_path\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, case CatalogPath of
																			undefined -> <<>>;
																			_ -> CatalogPath
																		 end, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"catalog_file\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, case CatalogFile of
																			undefined -> <<>>;
																			_ -> CatalogFile
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
			   ExpiresMinute, Public, ContentType, Path, RedirectUrl,
			   ListenAddress, ListenAddress_t, AllowedAddress, 
			   AllowedAddress_t, Port, MaxConnections,
			   IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
			   OAuth2WithCheckConstraint, OAuth2TokenEncrypt, Protocol,
			   CatalogPath, CatalogFile) ->
	PatternKey = ems_util:make_rowid_from_url(Url, Type),
	{ok, Id_re_compiled} = re:compile(PatternKey),
	#service{
				rowid = Rowid,
				id = Id,
				name = Name,
				url = Url,
				type = Type,
				service = Service,
			    module_name = ModuleName,
			    module_name_canonical = ModuleNameCanonical,
			    module = list_to_atom(ModuleName),
			    function_name = FunctionName,
			    function = list_to_atom(FunctionName),
			    id_re_compiled = Id_re_compiled,
			    public = Public,
			    comment = Comment,
			    version = Version,
			    owner = Owner,
				async = ems_util:parse_bool(Async),
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
			    catalog_path = CatalogPath,
			    catalog_file = CatalogFile,
			    path = Path,
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
				protocol = Protocol
			}.

new_service(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName,
			Type, Enable, Comment, Version, Owner, Async, Querystring, 
			QtdQuerystringRequired, Host, HostName, ResultCache,
			Authorization, Node, Lang, Datasource, Debug, SchemaIn, SchemaOut, 
			PoolSize, PoolMax, Properties, Page, PageModule, Timeout, Middleware, 
			CacheControl, ExpiresMinute, Public, ContentType, Path, RedirectUrl,
			ListenAddress, ListenAddress_t, AllowedAddress, AllowedAddress_t, 
			Port, MaxConnections,
			IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
			OAuth2WithCheckConstraint, OAuth2TokenEncrypt, Protocol,
			CatalogPath, CatalogFile) ->
	#service{
				rowid = Rowid,
				id = Id,
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
			    catalog_path = CatalogPath,
			    catalog_file = CatalogFile,
			    path = Path,
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
				protocol = Protocol
			}.

