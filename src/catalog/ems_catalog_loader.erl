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
	?DEBUG("ems_catalog_loader scan catalogs."),
	ListCatalog = scan_catalogs(Conf#config.cat_path_search, Conf, []),
	?DEBUG("ems_catalog_loader scan catalogs ok."),
	case parse_catalog(ListCatalog, [], [], [], [], 1, Conf) of
		{Cat2, Cat3, Cat4, CatK} ->
			?DEBUG("ems_catalog_loader parse catalogs ok."),
			{ok, Cat4, Cat2, Cat3, CatK};
		Error -> 
			?DEBUG("ems_catalog_loader parse catalog error: ~p.", [Error]),
			Error
	end.
	
	
-spec scan_catalogs(list(tuple()), #config{}, list()) -> list().
scan_catalogs([], _, Result) -> Result;
scan_catalogs([{CatName, FileName}|Rest], Conf, Result) ->
	case parse_filename_catalog(FileName, ?CATALOGO_PATH) of
		{ok, FileName2} ->
			io:format("ems_catalog_loader loading ~p from ~p.\n", [binary_to_list(CatName), FileName2]),
			Result2 = scan_catalog(FileName2, Conf, Result),
			scan_catalogs(Rest, Conf, Result2);
		{error, FileName2} ->
			ems_logger:format_warn("ems_catalog_loader failed to scan invalid catalog ~p. Ignoring this catalog.\n", [FileName2])
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
			ems_logger:format_warn("ems_catalog_loader catalog ~p does not exist, ignoring this catalog.\n", [FileName]),
			Result;
		_ -> 
			ems_logger:format_warn("ems_catalog_loader failed to read invalid catalog ~p. Ignoring this catalog.\n", [FileName]),
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
					?DEBUG("ems_catalog_loader scan ~p.", [FileName]),
					Result2 = scan_catalog(FileName, Conf, Result),
					scan_catalog_entry(CatTail, Conf, CurrentDir, Result2);			
				{error, FileName} ->
					ems_logger:format_warn("ems_catalog_loader scan invalid catalog ~p. Ignoring this catalog.\n", [FileName]),
					?DEBUG("~p: ~p.", [FileName, Cat]),
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
	case re:run(Name, "^[/_a-zA-Z-.][.:/_a-zA-Z0-9-]{0,300}$") of
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

valida_length(Value, MaxLength) ->
	case is_valid_length(Value, MaxLength) of
		true -> ok;
		false -> erlang:error(invalid_length)
	end.

%valida_web_service(_, _, _, _, false) -> ok;
%valida_web_service(Cat, ServiceImpl, ModuleName, FunctionName, true) ->
%	Module = list_to_atom(ModuleName),
%	Function = list_to_atom(FunctionName),
%	case proplists:lookup(Function, apply(Module, module_info, [exports])) of
%		{Function, 1} -> ok;
%		{Function, 2} -> ok;
%		_ -> throw({enoent, ServiceImpl, Cat})
%	end.


% Process the path "~" and "." wildcards and variable path. Return path
-spec parse_path_catalog(string(), list(tuple())) -> string().
parse_path_catalog(<<>>, _) -> undefined;
parse_path_catalog(Path, StaticFilePathList) when is_binary(Path) ->
	parse_path_catalog(binary_to_list(Path), StaticFilePathList);
parse_path_catalog(Path, StaticFilePathList) ->
	Ch = string:substr(Path, 1, 1),
	Ch2 = string:substr(Path, 2, 1),
	case Ch =:= "/" orelse (ems_util:is_letter(Ch) andalso Ch2 =:= ":")   of
		true -> ems_util:remove_ult_backslash_url(Path);  
		false ->
			case Ch == "~" of
				true -> 
					case init:get_argument(home) of
						{ok, [[HomePath]]} -> ems_util:replace(Path, "~", HomePath);
						_Error -> throw({error, einvalid_path_catalog})
					end;
				_ -> 
					case Ch == "." of
						true -> ems_util:remove_ult_backslash_url(?STATIC_FILE_PATH ++ "/" ++ string:substr(Path, 3));
						false -> 
							Path2 = ems_util:replace_all_vars(Path, StaticFilePathList),
							% after process variables, check ~ or . wildcards
							case string:substr(Path2, 1, 1) == "~" of
								true -> 
									case init:get_argument(home) of
										{ok, [[HomePath]]} -> ems_util:replace(Path2, "~", HomePath);
										_Error -> throw({error, einvalid_path_catalog})
									end;
								_ -> 
									case Ch == "." of
										true -> ems_util:remove_ult_backslash_url(?STATIC_FILE_PATH ++ "/" ++ string:substr(Path2, 3));
										false ->  Path2
									end
							end
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


parse_tcp_listen_address(ListenAddress) ->
	lists:map(fun(IP) -> 
					{ok, L2} = inet:parse_address(IP),
					L2 
			  end, ListenAddress).

parse_allowed_address_t(all) -> all;
parse_allowed_address_t(undefined) -> undefined;
parse_allowed_address_t(AllowedAddress) ->
	lists:map(fun(IP) -> 
					ems_http_util:mask_ipaddress_to_tuple(IP)
			  end, AllowedAddress).

parse_allowed_address(all) -> all;
parse_allowed_address(undefined) -> all;
parse_allowed_address(AddrList) -> 
	ems_util:binlist_to_list(AddrList).


parse_tcp_port(undefined) -> undefined;
parse_tcp_port(<<Port/binary>>) -> 
	parse_tcp_port(binary_to_list(Port));		
parse_tcp_port(Port) when is_list(Port) -> 
	parse_tcp_port(list_to_integer(Port));
parse_tcp_port(Port) when is_integer(Port) -> 
	case ems_consist:is_range_valido(Port, ?TCP_PORT_MIN, ?TCP_PORT_MAX) of
		true -> Port;
		false -> erlang:error("Parameter tcp_port invalid. Enter a value between 1024 and 5000.")
	end.

parse_ssl_path(undefined) -> erlang:error(einvalid_ssl_config);
parse_ssl_path(P) -> ?SSL_PATH ++  "/" ++ binary_to_list(P).


compile_modulo_erlang(undefined, _) -> ok;
compile_modulo_erlang(Path, ModuleNameCanonical) ->
	FileName = Path ++ "/" ++ ModuleNameCanonical ++ ".erl",
	case filelib:is_regular(FileName) of
		true ->
			io:format("Compile file ~p ", [FileName]),
			code:add_path(Path), 
			case compile:file(FileName, [{outdir, Path ++ "/"}]) of
				error -> io:format("[ ERROR ]\n");
				{error, Errors, _Warnings} -> 
					io:format("[ ERROR ]\n"),
					io:format_error("~p\n", [Errors]);
				_ -> io:format("[ OK ]\n")
			end;
		_ -> ok
	end.


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
		?DEBUG("Parse catalog ~p.", [H]),
		Name = maps:get(<<"name">>, H),
		Enable0 = maps:get(<<"enable">>, H, true),
		case Enable0 =:= false andalso lists:member(Name, Conf#config.cat_enable_services) of
			true -> Enable1 = true;
			false -> Enable1 = Enable0
		end,
		case Enable1 =:= true andalso lists:member(Name, Conf#config.cat_disable_services) of
			true -> Enable = false;
			false -> Enable = Enable1
		end,
		case Enable of 
			true ->
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
				Ds = maps:get(<<"datasource">>, H, undefined),
				case parse_datasource(Ds, Rowid, Conf) of
					{error, enoent} ->
						ems_logger:format_warn("Service ~p will be disabled because the datasource ~p was not found in the configuration file.\n", [Name, Ds]),
						parse_catalog(T, Cat2, Cat3, Cat4, CatK, Id, Conf);	
					Datasource ->
						ResultCache = maps:get(<<"result_cache">>, H, Conf#config.ems_result_cache),
						Authorization = ems_http_util:parse_authorization_type(maps:get(<<"authorization">>, H, Conf#config.authorization)),
						OAuth2WithCheckConstraint = maps:get(<<"oauth2_with_check_constraint">>, H, Conf#config.oauth2_with_check_constraint),
						OAuth2TokenEncrypt = maps:get(<<"oauth2_token_encrypt">>, H, false),
						Debug = ems_util:binary_to_bool(maps:get(<<"debug">>, H, false)),
						UseRE = maps:get(<<"use_re">>, H, false),
						SchemaIn = parse_schema(maps:get(<<"schema_in">>, H, null)),
						SchemaOut = parse_schema(maps:get(<<"schema_out">>, H, null)),
						PoolSize = parse_schema(maps:get(<<"pool_size">>, H, 1)),
						PoolMax = parse_schema(maps:get(<<"pool_max">>, H, 1)),
						Timeout = maps:get(<<"timeout">>, H, ?SERVICE_TIMEOUT),
						Middleware = parse_middleware(maps:get(<<"middleware">>, H, undefined)),
						CacheControl = maps:get(<<"cache_control">>, H, ?CACHE_CONTROL_1_SECOND),
						ExpiresMinute = maps:get(<<"expires_minute">>, H, 1),
						Public = maps:get(<<"public">>, H, true),
						ContentType = maps:get(<<"content_type">>, H, ?CONTENT_TYPE_JSON),
						Path = parse_path_catalog(maps:get(<<"path">>, H, <<>>), Conf#config.static_file_path),
						RedirectUrl = maps:get(<<"redirect_url">>, H, <<>>),
						valida_lang(Lang),
						valida_name_service(Name),
						valida_type_service(Type),
						valida_bool(Enable),
						valida_bool(Async),
						valida_length(Comment, 1000),
						valida_length(Version, 10),
						valida_length(Owner, 30),
						valida_bool(Public),
						valida_bool(Debug),
						valida_bool(UseRE),
						valida_bool(OAuth2WithCheckConstraint),
						valida_bool(OAuth2TokenEncrypt),
						ListenAddress = ems_util:binlist_to_list(maps:get(<<"tcp_listen_address">>, H, Conf#config.tcp_listen_address)),
						ListenAddress_t = parse_tcp_listen_address(ListenAddress),
						AllowedAddress = parse_allowed_address(maps:get(<<"tcp_allowed_address">>, H, Conf#config.tcp_allowed_address)),
						AllowedAddress_t = parse_allowed_address_t(AllowedAddress),
						MaxConnections = maps:get(<<"tcp_max_connections">>, H, [?HTTP_MAX_CONNECTIONS]),
						Port = parse_tcp_port(maps:get(<<"tcp_port">>, H, undefined)),
						Ssl = maps:get(<<"tcp_ssl">>, H, undefined),
						case Ssl of
							undefined ->
								IsSsl = false,
								SslCaCertFile = undefined,
								SslCertFile = undefined,
								SslKeyFile = undefined;
							_ ->
								IsSsl = true,
								SslCaCertFile = parse_ssl_path(maps:get(<<"cacertfile">>, Ssl, undefined)),
								SslCertFile = parse_ssl_path(maps:get(<<"certfile">>, Ssl, undefined)),
								SslKeyFile = parse_ssl_path(maps:get(<<"keyfile">>, Ssl, undefined))
						end,
						case Lang of
							<<"erlang">> -> 
								Node = <<>>,
								Host = '',
								HostName = Conf#config.ems_hostname,
								compile_modulo_erlang(Path, ModuleNameCanonical);
							_ ->	
								Node = parse_node_service(maps:get(<<"node">>, H, Conf#config.cat_node_search)),
								{Host, HostName} = parse_host_service(maps:get(<<"host">>, H, Conf#config.cat_host_search), ModuleName, Node, Conf)
						end,
						{Querystring, QtdQuerystringRequired} = parse_querystring(maps:get(<<"querystring">>, H, [])),
						IdBin = list_to_binary(integer_to_list(Id)),
						Page = maps:get(<<"page">>, H, undefined),
						PageModule = compile_page_module(Page, Rowid, Conf),
						ServiceView = new_service_view(IdBin, Name, Url2, ModuleName, FunctionName, 
														 Type, Enable, Comment, Version, Owner, 
														 Async, Host, ResultCache, Authorization, Node, Lang,
														 Datasource, Debug, SchemaIn, SchemaOut, 
														 Page, Timeout, Middleware, CacheControl, 
														 ExpiresMinute, Public, ContentType, Path, RedirectUrl,
														 ListenAddress, AllowedAddress, 
														 Port, MaxConnections,
														 IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
														 OAuth2WithCheckConstraint, OAuth2TokenEncrypt),
						case UseRE of
							true -> 
								Service = new_service_re(Rowid, IdBin, Name, Url2, 
														   ServiceImpl,
														   ModuleName, 
														   ModuleNameCanonical,
														   FunctionName, Type, Enable, Comment, 
														   Version, Owner, Async, 
														   Querystring, QtdQuerystringRequired,
														   Host, HostName, ResultCache,
														   Authorization, Node, Lang,
														   Datasource, Debug, SchemaIn, SchemaOut, 
														   PoolSize, PoolMax, H, Page, 
														   PageModule, Timeout, 
														   Middleware, CacheControl, ExpiresMinute, 
														   Public, ContentType, Path, RedirectUrl,
														   ListenAddress, ListenAddress_t, AllowedAddress, 
														   AllowedAddress_t, Port, MaxConnections,
														   IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
														   OAuth2WithCheckConstraint, OAuth2TokenEncrypt),
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
														Host, HostName, ResultCache,
														Authorization, Node, Lang,
														Datasource, Debug, SchemaIn, SchemaOut, 
														PoolSize, PoolMax, H, Page, 
														PageModule, Timeout, 
														Middleware, CacheControl, 
														ExpiresMinute, Public, 
														ContentType, Path, RedirectUrl,
														ListenAddress, ListenAddress_t, AllowedAddress, 
														AllowedAddress_t, Port, MaxConnections,
														IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
														OAuth2WithCheckConstraint, OAuth2TokenEncrypt),
								case Type of
									<<"KERNEL">> -> parse_catalog(T, Cat2, Cat3, Cat4, [Service|CatK], Id+1, Conf);
									_ -> parse_catalog(T, [{Rowid, Service}|Cat2], Cat3, [ServiceView|Cat4], CatK, Id+1, Conf)
								end
						end
				end;
			false -> 
				parse_catalog(T, Cat2, Cat3, Cat4, CatK, Id, Conf)
		end
	catch
		_Exception:Reason -> 
			ems_logger:format_warn("ems_catalog_loader parse invalid catalog specification: ~p\n\t~p.\n", [Reason, H]),
			parse_catalog(T, Cat2, Cat3, Cat4, CatK, Id, Conf)
	end.

parse_middleware(undefined) -> undefined;
parse_middleware(Middleware) -> erlang:binary_to_atom(Middleware, utf8).
	

compile_page_module(undefined, _, _) -> undefined;
compile_page_module(Page, Rowid, Conf) -> 
	ModuleNamePage =  "page" ++ integer_to_list(Rowid),
	PageFile = parse_path_catalog(Page, Conf#config.static_file_path),
	case ems_django:compile_file(binary_to_list(PageFile), ModuleNamePage) of
		{ok, PageModule} -> PageModule;
		_ -> throw({einvalid_page, Page})
	end.

	
parse_schema(null) -> null;
parse_schema(Name) -> Name.

parse_datasource(undefined, _, _) -> undefined;
parse_datasource(M, Rowid, _) when erlang:is_map(M) -> ems_db:create_datasource_from_map(M, Rowid);
parse_datasource(DsName, _Rowid, Conf) -> 
	case maps:get(DsName, Conf#config.ems_datasources, undefined) of
		undefined -> {error, enoent};
		M -> M
	end.
	
	
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
			   OAuth2WithCheckConstraint, OAuth2TokenEncrypt) ->
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
				oauth2_token_encrypt = OAuth2TokenEncrypt
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
			OAuth2WithCheckConstraint, OAuth2TokenEncrypt) ->
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
				oauth2_token_encrypt = OAuth2TokenEncrypt
			}.

new_service_view(Id, Name, Url, ModuleName, FunctionName, Type, Enable,
				  Comment, Version, Owner, Async, Host, ResultCache,
				  Authorization, Node, Lang, _Datasource, 
				  Debug, SchemaIn, SchemaOut, Page, Timeout, 
				  Middleware, CacheControl, ExpiresMinute, 
				  Public, ContentType, Path, RedirectUrl,
				  ListenAddress, AllowedAddress, 
				  Port, MaxConnections,
				  IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
				  OAuth2WithCheckConstraint, OAuth2TokenEncrypt) ->
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
			    <<"result_cache">> => ResultCache,
			    <<"authorization">> => Authorization,
			    <<"node">> => Node,
			    <<"page">> => Page,
			    <<"debug">> => Debug,
			    <<"schema_in">> => SchemaIn,
			    <<"schema_out">> => SchemaOut,
			    <<"timeout">> => Timeout,
			    <<"middleware">> => Middleware,
   			    <<"cache_control">> => CacheControl,
			    <<"expires">> => ExpiresMinute,
				<<"lang">> => Lang,
				<<"content_type">> => ContentType,
				<<"path">> => Path,
				<<"redirect_url">> => RedirectUrl,
				<<"enable">> => Enable,
				<<"tcp_listen_address">> => ListenAddress,
				<<"tcp_allowed_address">> => AllowedAddress,
				<<"tcp_max_connections">> => MaxConnections,
				<<"tcp_port">> => Port,
				<<"tcp_is_ssl">> => IsSsl,
				<<"tcp_ssl_cacertfile">> => SslCaCertFile,
				<<"tcp_ssl_certfile">> => SslCertFile,
				<<"tcp_ssl_keyfile">> => SslKeyFile,
				<<"oauth2_with_check_constraint">> => OAuth2WithCheckConstraint,
				<<"oauth2_token_encrypt">> => OAuth2TokenEncrypt
				
},
	Service.



