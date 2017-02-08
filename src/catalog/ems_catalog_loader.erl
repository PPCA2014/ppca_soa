%%********************************************************************
%% @title Module ems_catalog_loader
%% @version 1.0.0
%% @doc Module responsible for parse e load catalog services
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_loader).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Client
-export([init_catalog/0,
		 list_kernel_catalog/0]).

 
%%====================================================================
%% Client API
%%====================================================================
 

init_catalog() ->
	ets:new(ets_ems_catalog, [set, named_table, public]),
	case get_catalog() of
		{ok, Cat1, Cat2, Cat3, CatK} -> 
			ets:insert(ets_ems_catalog, {cat, {Cat1, Cat2, Cat3, CatK}}),
			ok;
		{error, Reason} ->
			{stop, Reason}
	end.
	
list_kernel_catalog() ->
	case ets:lookup(ets_ems_catalog, cat) of
		[] -> {stop, nocatalog};
		[{cat, {_, _, _, CatK}}] -> CatK
	end.


%% @doc Get catalog of services
get_catalog() -> 
	Conf = ems_config:getConfig(),
	?DEBUG("Scan catalogs..."),
	ListCatalog = scan_catalogs(Conf#config.cat_path_search, Conf, []),
	?DEBUG("Parse catalogs..."),
	case parse_catalog(ListCatalog, [], [], [], [], 1, Conf) of
		{Cat2, Cat3, Cat4, CatK} -> {ok, Cat4, Cat2, Cat3, CatK};
		Error -> Error
	end.
	
	
-spec scan_catalogs(list(tuple()), #config{}, list()) -> list().
scan_catalogs([], _, Result) -> Result;
scan_catalogs([{CatName, FileName}|Rest], Conf, Result) ->
	case parse_filename_catalog(FileName, ?CATALOGO_PATH) of
		{ok, FileName2} ->
			io:format("Loading catalog ~p from ~p.\n", [binary_to_list(CatName), FileName2]),
			Result2 = scan_catalog(FileName2, Conf, Result),
			scan_catalogs(Rest, Conf, Result2);
		{error, FileName2} ->
			ems_logger:format_warn("Invalid filename catalog ~p. Ignoring this catalog.\n", [FileName2])
	end.
		
		
-spec scan_catalog(FileName :: string(), Conf :: #config{}, Result :: list(map())) -> list(map()) | {error, atom()}.
scan_catalog(FileName, Conf, Result) ->
	CurrentDir = filename:dirname(FileName),
	case ems_util:read_file_as_map(FileName) of
		{ok, CatList} when is_list(CatList) -> 
			scan_catalog_entry(CatList, Conf, CurrentDir, Result);
		{ok, CatMap} -> 
			scan_catalog_entry([CatMap], Conf, CurrentDir, Result);
		{error, enoent} ->
			ems_logger:format_warn("Catalog ~p does not exist, ignoring this catalog.\n", [FileName]),
			Result;
		_ -> 
			ems_logger:format_warn("Catalog ~p with invalid json format, ignoring this catalog.\n", [FileName]),
			Result
	end.
	
-spec scan_catalog_entry(list(), Conf :: #config{}, string(), list()) -> list().
scan_catalog_entry([], _, _, Result) -> 
	Result;
scan_catalog_entry([Cat|CatTail], Conf, CurrentDir, Result) ->
	case maps:is_key(<<"file">>, Cat) of
		true -> 
			case parse_filename_catalog(maps:get(<<"file">>, Cat), CurrentDir) of
				{ok, FileName} ->
					?DEBUG("Scan catalog ~p.", [FileName]),
					Result2 = scan_catalog(FileName, Conf, Result),
					scan_catalog_entry(CatTail, Conf, CurrentDir, Result2);			
				{error, FileName} ->
					?DEBUG("Fail scan catalog ~p: ~p.", [FileName, Cat]),
					ems_logger:format_warn("Invalid filename catalog ~p. Ignoring this catalog.\n", [FileName]),
					scan_catalog_entry(CatTail, Conf, CurrentDir, Result)
			end;
		false -> 
			scan_catalog_entry(CatTail, Conf, CurrentDir, [Cat | Result])
	end.

-spec parse_filename_catalog(map(), string()) -> {ok, string()} | {error, string()}.
parse_filename_catalog(FileName, CurrentDir) when is_binary(FileName) ->
	parse_filename_catalog(binary_to_list(FileName), CurrentDir);
parse_filename_catalog(FileName, CurrentDir) ->
	Ch = string:substr(FileName, 1, 1),
	Ch2 = string:substr(FileName, 2, 1),
	case Ch =:= "/" orelse (ems_util:is_letter(Ch) andalso Ch2 =:= ":")   of
		true -> {ok, FileName};  
		false ->
			case Ch == "~" of
				true -> 
					case init:get_argument(home) of
						{ok, [[HomePath]]} ->
							{ok, ems_util:replace(FileName, "~", HomePath)};
						_Error -> {error, FileName}
					end;
				_ -> 
					case Ch == "." of
						true -> {ok, CurrentDir ++ "/" ++ string:substr(FileName, 3)};
						false -> {ok, CurrentDir ++ "/" ++ FileName}
					end
			end
	end.


%% @doc Indica se o name da querystring é valido
is_name_querystring_valido(Name) ->
	case re:run(Name, "^[_a-zA-Z][_a-zA-Z0-9]{0,29}$") of
		nomatch -> false;
		_ -> true
	end.

%% @doc Indica se o name do pseudo param é valido
%is_pseudo_name_param_valido(Name) ->
%	case re:run(Name, "^[a-z0-9]{0,29}$") of
%		nomatch -> false;
%		_ -> true
%	end.

%% @doc Indica se o name da querystring é valido
is_name_service_valido(Name) ->
	case re:run(Name, "^[/_a-zA-Z][.:/_a-zA-Z0-9]{0,300}$") of
		nomatch -> false;
		_ -> true
	end.

%% @doc Indica se o tipo dos dados são é valido
is_type_valido(<<"int">>) 	 -> true;
is_type_valido(<<"string">>) -> true;
is_type_valido(<<"year">>) 	 -> true;
is_type_valido(<<"date">>) 	 -> true;
is_type_valido(<<"bool">>)   -> true;
is_type_valido(Enum) when length(Enum) > 0 -> true;
is_type_valido(_) 	 		 -> false.

%% @doc Indica se o tamanho é válido
is_valid_length(Value, MaxLength) -> length(binary_to_list(Value)) =< MaxLength.

%% @doc Valida o name do serviço
valida_name_service(Name) ->
	case is_name_service_valido(Name) of
		true -> ok;
		false -> erlang:error(invalid_name_service)
	end.

%% @doc Valida o name da querystring
valida_name_querystring(Name) ->
	case is_name_querystring_valido(Name) of
		true -> ok;
		false -> erlang:error(invalid_name_querystring)
	end.

%% @doc Valida o name do pseudo param
%valida_pseudo_name_param(Name) ->
%	case is_pseudo_name_param_valido(Name) of
%		true -> ok;
%		false -> erlang:error(invalid_pseudo_name_param)
%	end.

%% @doc Valida o tipo de dado da querystring
valida_type_querystring(Type) ->
	case is_type_valido(Type) of
		true -> ok;
		false -> erlang:error(invalid_type_querystring)
	end.

%% @doc Valida o método do serviço 
valida_type_service(Type) ->
	case ems_http_util:is_metodo_suportado(Type) orelse Type =:= <<"KERNEL">> of
		true -> ok;
		false -> erlang:error(invalid_type_service)
	end.

valida_url_service(<<"/">>) -> ok;
valida_url_service(Url) ->
	case ems_http_util:is_url_valido(Url) andalso is_valid_length(Url, 300) of
		true -> ok;
		false -> erlang:error(invalid_url_service)
	end.
	
valida_bool(<<"true">>) -> ok;
valida_bool(<<"false">>) -> ok;
valida_bool(true) -> ok;
valida_bool(false) -> ok;
valida_bool(_) -> erlang:error(invalid_bool).

valida_lang(<<"java">>) -> ok;
valida_lang(<<"erlang">>) -> ok;
valida_lang(<<"net">>) -> ok;
valida_lang(_) -> erlang:error(invalid_lang_service).

valida_authorization(<<"Basic">>) -> ok;
valida_authorization(<<>>) -> ok;
valida_authorization(_) -> erlang:error(invalid_authorization).

valida_length(Value, MaxLength) ->
	case is_valid_length(Value, MaxLength) of
		true -> ok;
		false -> erlang:error(invalid_length)
	end.

valida_web_service(_, _, _, _, false) -> ok;
valida_web_service(Cat, ServiceImpl, ModuleName, FunctionName, true) ->
	Module = list_to_atom(ModuleName),
	Function = list_to_atom(FunctionName),
	case proplists:lookup(Function, apply(Module, module_info, [exports])) of
		{Function, 1} -> ok;
		{Function, 2} -> ok;
		_ -> throw({enoent, ServiceImpl, Cat})
	end.


% Process the path "~" and "." wildcards and variable path. Return path
-spec parse_path_catalog(string(), list(tuple())) -> string().
parse_path_catalog(Path, StaticFilePathList) when is_binary(Path) ->
	parse_path_catalog(binary_to_list(Path), StaticFilePathList);
parse_path_catalog(Path, StaticFilePathList) ->
	Ch = string:substr(Path, 1, 1),
	Ch2 = string:substr(Path, 2, 1),
	case Ch =:= "/" orelse (ems_util:is_letter(Ch) andalso Ch2 =:= ":")   of
		true -> Path;  
		false ->
			case Ch == "~" of
				true -> 
					case init:get_argument(home) of
						{ok, [[HomePath]]} -> ems_util:replace(Path, "~", HomePath);
						_Error -> throw({error, einvalid_path_catalog})
					end;
				_ -> 
					case Ch == "." of
						true -> ?STATIC_FILE_PATH ++ "/" ++ string:substr(Path, 3);
						false -> 
							% Renders variables {{ var }} and invokes them again parse_path_catalog to process the path "~" and "." wildcards
							parse_path_catalog(ems_util:replace_all_vars(Path, StaticFilePathList), StaticFilePathList)
					end
			end
	end.
			

	

%% @doc Retorna uma mapa das querystrings e a quantidade de queries obrigatórias
parse_querystring([]) -> {[], 0};
parse_querystring(Querystring) -> parse_querystring_def(Querystring, [], 0).
	
%% @doc Retorna uma mapa das querystrings e a quantidade de queries obrigatórias
parse_querystring_def([], Querystring, QtdRequired) -> 	
	{Querystring, QtdRequired};
parse_querystring_def([H|T], Querystring, QtdRequired) -> 
	Name = maps:get(<<"name">>, H),
	Type = maps:get(<<"type">>, H, <<"string">>),
	Default = maps:get(<<"default">>, H, <<>>),
	Comment = maps:get(<<"comment">>, H, <<>>),
	Required = maps:get(<<"required">>, H, <<"false">>),
	valida_name_querystring(Name),
	valida_type_querystring(Type),
	valida_length(Default, 150),
	valida_length(Comment, 1000),
	valida_bool(Required),
	case Required of
		<<"true">>  -> QtdRequired2 = QtdRequired + 1;
		<<"false">> -> QtdRequired2 = QtdRequired;
		_ -> QtdRequired2 = QtdRequired,
			 erlang:error(invalid_required_querystring)
	end,
	Q = #{<<"name">>     => Name,
		  <<"type">>     => Type,
		  <<"default">>  => Default,
		  <<"comment">>  => Comment,
		  <<"required">> => Required},
	parse_querystring_def(T, [Q | Querystring], QtdRequired2).


%% @doc Converte um pseudo parâmetro para sua expressão regular
%pseudoparam_to_re(":id")   -> "(?<id>[0-9]{1,9})";
%pseudoparam_to_re(":top")  -> "(?<top>[0-9]{1,4})";
%pseudoparam_to_re(_)  -> erlang:error(invalid_pseudo_param).
%pseudoparam_to_re(":id", Nome)  -> io_lib:format("(?<id_~s>[0-9]{1,9})", [Nome]);
%pseudoparam_to_re(":top", Nome) -> io_lib:format("(?<top_~s>[0-9]{1,4})", [Nome]);
%pseudoparam_to_re(_, _)  -> erlang:error(invalid_pseudo_param).

%% @doc Faz o parser da URL convertendo os pseudo parâmetros em expressão regular
%parse_url_service(<<Url/binary>>) ->
%	Url1 = string:tokens(binary_to_list(Url), "/"),
%	parse_url_service(Url1, []).

%parse_url_service([], []) -> <<"/">>;
%parse_url_service([], Url) -> list_to_binary(["/" | string:join(lists:reverse(Url), "/")]);
%parse_url_service([H|T], Url) when hd(H) /= $: -> parse_url_service(T, [H | Url]);
%parse_url_service([H|T], Url) ->
%	case string:chr(H, $_) > 0 of
%		true ->
%			[Pseudo, Nome] = string:tokens(H, "_"),
%			valida_pseudo_name_param(Nome),
%			P = pseudoparam_to_re(Pseudo, Nome),
%			parse_url_service(T, [P | Url]);
%		false ->
%			Pseudo = H,
%			P = pseudoparam_to_re(Pseudo),
%			parse_url_service(T, [P | Url])
%	end.

make_ets_catalog([]) -> ok;
make_ets_catalog([H = {_Rowid, #service{type = Type}}|T]) -> 
	case Type of
		<<"GET">> -> ets:insert(ets_get, H);
		<<"POST">> -> ets:insert(ets_post, H);
		<<"PUT">> -> ets:insert(ets_put, H);
		<<"DELETE">> -> ets:insert(ets_delete, H);
		<<"OPTIONS">> -> ets:insert(ets_options, H)
	end,
	make_ets_catalog(T). 	


%% @doc Faz o parser dos contratos de serviços no catálogo de serviços
parse_catalog([], Cat2, Cat3, Cat4, CatK, _Id, _Conf) ->
	ets:new(ets_get, [ordered_set, named_table, public, {read_concurrency, true}]),
	ets:new(ets_post, [ordered_set, named_table, public, {read_concurrency, true}]),
	ets:new(ets_put, [ordered_set, named_table, public, {read_concurrency, true}]),
	ets:new(ets_delete, [ordered_set, named_table, public, {read_concurrency, true}]),
	ets:new(ets_options, [ordered_set, named_table, public, {read_concurrency, true}]),
	make_ets_catalog(Cat2),
	EtsCat2 = ems_util:list_to_ets(Cat2, ets_cat2, [ordered_set, 
													  public, 
													  {read_concurrency, true}]),
	{EtsCat2, lists:reverse(Cat3), Cat4, CatK};
	
parse_catalog([H|T], Cat2, Cat3, Cat4, CatK, Id, Conf) ->
	try
		Enable = maps:get(<<"enable">>, H, true),
		case Enable of 
			true ->
				Name = maps:get(<<"name">>, H),
				Url2 = maps:get(<<"url">>, H),
				Type = maps:get(<<"type">>, H, <<"GET">>),
				valida_url_service(Url2),
				ServiceImpl = maps:get(<<"service">>, H),
				{ModuleName, ModuleNameCanonical, FunctionName} = parse_service_service(ServiceImpl),
				Comment = maps:get(<<"comment">>, H, <<>>),
				Version = maps:get(<<"version">>, H, <<"1.0.0">>),
				Owner = maps:get(<<"owner">>, H, <<>>),
				Async = maps:get(<<"async">>, H, false),
				Rowid = ems_util:make_rowid(Url2),
				Lang = maps:get(<<"lang">>, H, <<>>),
				Datasource = parse_datasource(maps:get(<<"datasource">>, H, undefined), Rowid),
				Result_Cache = maps:get(<<"result_cache">>, H, 0),
				Authorization = maps:get(<<"authorization">>, H, <<>>),
				Debug = ems_util:binary_to_bool(maps:get(<<"debug">>, H, false)),
				UseRE = maps:get(<<"use_re">>, H, false),
				SchemaIn = parse_schema(maps:get(<<"schema_in">>, H, null)),
				SchemaOut = parse_schema(maps:get(<<"schema_out">>, H, null)),
				PoolSize = parse_schema(maps:get(<<"pool_size">>, H, 1)),
				PoolMax = parse_schema(maps:get(<<"pool_max">>, H, 1)),
				Timeout = maps:get(<<"timeout">>, H, ?SERVICE_TIMEOUT),
				Middleware = parse_middleware(maps:get(<<"middleware">>, H, undefined)),
				Cache_Control = maps:get(<<"cache_control">>, H, ?DEFAULT_CACHE_CONTROL),
				ExpiresMinute = maps:get(<<"expires_minute">>, H, 60),
				Public = maps:get(<<"public">>, H, true),
				ContentType = maps:get(<<"content_type">>, H, ?CONTENT_TYPE_JSON),
				Path = parse_path_catalog(maps:get(<<"path">>, H, ?STATIC_FILE_PATH), Conf#config.static_file_path),
				RedirectUrl = maps:get(<<"redirect_url">>, H, <<>>),
				valida_lang(Lang),
				valida_name_service(Name),
				valida_type_service(Type),
				valida_bool(Enable),
				valida_bool(Async),
				valida_length(Comment, 1000),
				valida_length(Version, 10),
				valida_length(Owner, 30),
				valida_authorization(Authorization),
				valida_bool(Debug),
				valida_bool(UseRE),
				case Lang of
					<<"erlang">> -> 
						Node = <<>>,
						Host = '',
						HostName = Conf#config.ems_hostname,
						valida_web_service(H, ServiceImpl, ModuleName, FunctionName, Enable);
					_ ->	
						Node = parse_node_service(maps:get(<<"node">>, H, Conf#config.cat_node_search)),
						{Host, HostName} = parse_host_service(maps:get(<<"host">>, H, Conf#config.cat_host_search), ModuleNameCanonical, Node, Conf)
				end,
				{Querystring, QtdQuerystringRequired} = parse_querystring(maps:get(<<"querystring">>, H, [])),
				IdBin = list_to_binary(integer_to_list(Id)),
				Page = maps:get(<<"page">>, H, undefined),
				PageModule = compile_page_module(Page, Rowid),
				ServiceView = new_service_view(IdBin, Name, Url2, ModuleName, FunctionName, 
												 Type, Enable, Comment, Version, Owner, 
												 Async, Host, Result_Cache, Authorization, Node, Lang,
												 Datasource, Debug, SchemaIn, SchemaOut, 
												 Page, Timeout, Middleware, Cache_Control, 
												 ExpiresMinute, Public, ContentType, Path, RedirectUrl),
				case UseRE of
					true -> 
						Service = new_service_re(Rowid, IdBin, Name, Url2, 
												   ServiceImpl,
												   ModuleName, 
												   ModuleNameCanonical,
												   FunctionName, Type, Enable, Comment, 
												   Version, Owner, Async, 
												   Querystring, QtdQuerystringRequired,
												   Host, HostName, Result_Cache,
												   Authorization, Node, Lang,
												   Datasource, Debug, SchemaIn, SchemaOut, 
												   PoolSize, PoolMax, H, Page, 
												   PageModule, Timeout, 
												   Middleware, Cache_Control, ExpiresMinute, 
												   Public, ContentType, Path, RedirectUrl),
						case Type of
							<<"KERNEL">> -> parse_catalog(T, Cat2, Cat3, Cat4, [Service|CatK], Id+1, Conf);
							_ -> parse_catalog(T, Cat2, [Service|Cat3], [ServiceView|Cat4], CatK, Id+1, Conf)
						end;
					false -> 
						Service = new_service(Rowid, IdBin, Name, Url2, 
												ServiceImpl,
												ModuleName,
												ModuleNameCanonical,
												FunctionName, Type, Enable, Comment,
												Version, Owner, Async, 
												Querystring, QtdQuerystringRequired,
												Host, HostName, Result_Cache,
												Authorization, Node, Lang,
												Datasource, Debug, SchemaIn, SchemaOut, 
												PoolSize, PoolMax, H, Page, 
												PageModule, Timeout, 
												Middleware, Cache_Control, 
												ExpiresMinute, Public, 
												ContentType, Path, RedirectUrl),
						case Type of
							<<"KERNEL">> -> parse_catalog(T, Cat2, Cat3, Cat4, [Service|CatK], Id+1, Conf);
							_ -> parse_catalog(T, [{Rowid, Service}|Cat2], Cat3, [ServiceView|Cat4], CatK, Id+1, Conf)
						end
				end;
			false -> 
				parse_catalog(T, Cat2, Cat3, Cat4, CatK, Id, Conf)
		end
	catch
		_Exception:_Reason -> 
			ems_logger:format_warn("Invalid catalog specification: \n\t~p.\n", [H]),
			?DEBUG("Reason to invalid catalog specification: ~p.\n", [_Reason]),
			parse_catalog(T, Cat2, Cat3, Cat4, CatK, Id, Conf)
	end.

parse_middleware(undefined) -> undefined;
parse_middleware(Middleware) -> erlang:binary_to_atom(Middleware, utf8).
	

compile_page_module(undefined, _) -> undefined;
compile_page_module(Page, Rowid) -> 
	ModuleNamePage =  "page" ++ integer_to_list(Rowid),
	case ems_page:compile_file(Page, ModuleNamePage) of
		{ok, PageModule} -> PageModule;
		_ -> throw({einvalid_page, Page})
	end.

	
parse_schema(null) -> null;
parse_schema(Name) -> Name.

	
parse_datasource(undefined, _) -> undefined;
parse_datasource(M, Rowid) -> 
	#service_datasource{rowid = Rowid,
						type = list_to_atom(binary_to_list(maps:get(<<"type">>, M))),
						driver = maps:get(<<"driver">>, M, <<>>),
						connection = binary_to_list(maps:get(<<"connection">>, M, <<>>)),
						table_name = binary_to_list(maps:get(<<"table_name">>, M, <<>>)),
						primary_key = binary_to_list(maps:get(<<"primary_key">>, M, <<>>)),
						csv_delimiter = binary_to_list(maps:get(<<"csv_delimiter">>, M, <<";">>)),
						sql = binary_to_list(maps:get(<<"sql">>, M, <<>>)),
						timeout = maps:get(<<"timeout">>, M, ?MAX_TIME_ODBC_QUERY),
						max_pool_size = maps:get(<<"max_pool_size">>, M, ?MAX_CONNECTION_BY_POOL)
						}.
	
	
	
parse_service_service(Service) ->
	try
		[ModuleName, FunctionName] = binary:split(Service, <<":">>),
		ModuleName2 = binary_to_list(ModuleName),
		FunctionName2 = binary_to_list(FunctionName),
		ModuleNameCanonical = lists:last(string:tokens(ModuleName2, ".")),
		{ModuleName2, ModuleNameCanonical, FunctionName2}
	catch
		_Exception:_Reason ->  erlang:error(invalid_service_service)
	end.
	
parse_node_service(<<>>) -> <<>>;
parse_node_service(List) -> List.

	
%% @doc O host pode ser um alias definido no arquivo de configuração
parse_host_service(<<>>, _,_,_) -> {'', atom_to_list(node())};
parse_host_service(_Host, ModuleNameCanonical, Node, _Conf) ->
	ListHost = case net_adm:host_file() of
		{error, _Reason} -> [list_to_atom(net_adm:localhost())];
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
	

new_service_re(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName, 
			   Type, Enable, Comment, Version, Owner, Async, Querystring, 
			   QtdQuerystringRequired, Host, HostName, Result_Cache,
			   Authorization, Node, Lang, Datasource,
			   Debug, SchemaIn, SchemaOut, PoolSize, PoolMax, Properties,
			   Page, PageModule, Timeout, Middleware, Cache_Control, 
			   ExpiresMinute, Public, ContentType, Path, RedirectUrl) ->
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
				async = ems_util:binary_to_bool(Async),
			    querystring = Querystring,
			    qtd_querystring_req = QtdQuerystringRequired,
			    host = Host,
			    host_name = HostName,
			    result_cache = Result_Cache,
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
			    cache_control = Cache_Control,
			    expires = ExpiresMinute,
			    content_type = ContentType,
			    path = Path,
			    redirect_url = RedirectUrl,
			    enable = Enable
			}.

new_service(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName,
			Type, Enable, Comment, Version, Owner, Async, Querystring, 
			QtdQuerystringRequired, Host, HostName, Result_Cache,
			Authorization, Node, Lang, Datasource, Debug, SchemaIn, SchemaOut, 
			PoolSize, PoolMax, Properties, Page, PageModule, Timeout, Middleware, 
			Cache_Control, ExpiresMinute, Public, ContentType, Path, RedirectUrl) ->
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
			    result_cache = Result_Cache,
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
			    cache_control = Cache_Control,
			    expires = ExpiresMinute,
			    content_type = ContentType,
			    path = Path,
			    redirect_url = RedirectUrl,
			    enable = Enable
			}.

new_service_view(Id, Name, Url, ModuleName, FunctionName, Type, Enable,
				  Comment, Version, Owner, Async, Host, Result_Cache,
				  Authorization, Node, Lang, _Datasource, 
				  Debug, SchemaIn, SchemaOut, Page, Timeout, 
				  Middleware, Cache_Control, ExpiresMinute, 
				  Public, ContentType, Path, RedirectUrl) ->
	Service = #{<<"id">> => Id,
				<<"name">> => Name,
				<<"url">> => Url,
				<<"type">> => Type,
			    <<"module">> => list_to_binary(ModuleName),
			    <<"function">> => list_to_binary(FunctionName),
			    <<"public">> => Public,
			    <<"comment">> => Comment,
			    <<"version">> => Version,
			    <<"owner">> => Owner,
			    <<"async">> => Async,
			    <<"host">> => Host,
			    <<"result_cache">> => Result_Cache,
			    <<"authorization">> => Authorization,
			    <<"node">> => Node,
			    <<"page">> => Page,
			    <<"debug">> => Debug,
			    <<"schema_in">> => SchemaIn,
			    <<"schema_out">> => SchemaOut,
			    <<"timeout">> => Timeout,
			    <<"middleware">> => Middleware,
   			    <<"cache_control">> => Cache_Control,
			    <<"expires">> => ExpiresMinute,
				<<"lang">> => Lang,
				<<"content_type">> => ContentType,
				<<"path">> => Path,
				<<"redirect_url">> => RedirectUrl,
				<<"enable">> => Enable},
	Service.



