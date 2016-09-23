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


%% @doc Obtém o catálogo
get_catalog() -> 
	case get_main_catalog() of
		{ok, CatMestre} ->
			CatalogoDefsPath = ?CATALOGO_PATH ++ "/",
			%% Obtém a lista do conteúdo de todos os catálogos
			CatDefs = lists:map(fun(M) -> 
									NomeArq = CatalogoDefsPath ++ binary_to_list(maps:get(<<"file">>, M)),
									NomeCatalogo = binary_to_list(maps:get(<<"catalog">>, M)),
									case file:read_file(NomeArq) of
										{ok, Arq} -> Arq;
										{error, enoent} -> 
											io:format("Catalog ~s not found. Filename: ~s.", [NomeCatalogo, NomeArq]),
											<<>>
									end
								end, CatMestre),

			%% Adiciona "," entre as definições de cada catálogo
			CatDefs1 = lists:foldl(fun(X, Y) ->
										case Y of
											<<>> -> X;
											Y2 -> iolist_to_binary([X, <<","/utf8>>, Y2])
										end 
									end, <<>>, CatDefs),

			%% Adiciona abertura e fechamento de lista para o parser correto do JSON
			CatDefs2 = iolist_to_binary([<<"[">>, CatDefs1, <<"]">>]),

			{ok, Cat1} = ems_util:json_decode_as_map(CatDefs2),
			%% Faz o parser do catálogo
			Conf = ems_config:getConfig(),
			case parse_catalog(Cat1, [], [], [], [], 1, Conf) of
				{Cat2, Cat3, Cat4, CatK} -> {ok, Cat4, Cat2, Cat3, CatK};
				Error -> Error
			end;
		Error -> Error
	end.
			

%% @doc O catálogo mestre possui os includes para os catálogos

get_main_catalog() ->
	case file:read_file(?CATALOGO_PATH ++ "/catalog.conf") of
		{ok, Arq} -> ems_util:json_decode_as_map(Arq);
		Error -> Error
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

%% @doc Indica se a URL contém expressão regular
is_url_com_re([]) -> false;
is_url_com_re([H|T]) -> 
	case lists:member(H, [$?, $<, $>, $$, ${, $}, $-, $,]) of
		true -> true;
		false -> is_url_com_re(T)
	end.

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

valida_authentication(<<"Basic">>) -> ok;
valida_authentication(<<>>) -> ok;
valida_authentication(_) -> erlang:error(invalid_authentication).

valida_length(Value, MaxLength) ->
	case is_valid_length(Value, MaxLength) of
		true -> ok;
		false -> erlang:error(invalid_length)
	end.

%% @doc Retorna uma mapa das querystrings e a quantidade de queries obrigatórias
parse_querystring(<<>>) -> {<<>>, 0};
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

%% @doc Faz o parser dos contratos de serviços no catálogo de serviços
parse_catalog([], Cat2, Cat3, Cat4, CatK, _Id, _Conf) ->
	EtsCat2 = ems_util:list_to_ets(Cat2, ets_cat2, [set, 
													  public, 
													  {read_concurrency, true}]),
	{EtsCat2, Cat3, Cat4, CatK};
	
parse_catalog([H|T], Cat2, Cat3, Cat4, CatK, Id, Conf) ->
	try
		Name = maps:get(<<"name">>, H),
		Url = maps:get(<<"url">>, H),
		Url2 = Url,
		Type = maps:get(<<"type">>, H, <<"GET">>),
		valida_url_service(Url2),
		ServiceImpl = maps:get(<<"service">>, H),
		{ModuleName, ModuleNameCanonical, FunctionName} = parse_service_service(ServiceImpl),
		Apikey = maps:get(<<"APIkey">>, H, <<"false">>),
		Comment = maps:get(<<"comment">>, H, <<>>),
		Version = maps:get(<<"version">>, H, <<>>),
		Owner = maps:get(<<"owner">>, H, <<>>),
		Async = maps:get(<<"async">>, H, <<"false">>),
		Rowid = ems_util:make_rowid_from_url(Url2, Type),
		Lang = maps:get(<<"lang">>, H, <<>>),
		Datasource = parse_datasource(maps:get(<<"datasource">>, H, null)),
		Result_Cache = maps:get(<<"result_cache">>, H, 0),
		Authentication = maps:get(<<"authentication">>, H, <<>>),
		Debug = ems_util:binary_to_bool(maps:get(<<"debug">>, H, <<"false">>)),
		UseRE = ems_util:binary_to_bool(maps:get(<<"use_re">>, H, <<"false">>)),
		SchemaIn = parse_schema(maps:get(<<"schema_in">>, H, null)),
		SchemaOut = parse_schema(maps:get(<<"schema_out">>, H, null)),
		PoolSize = parse_schema(maps:get(<<"pool_size">>, H, 1)),
		PoolMax = parse_schema(maps:get(<<"pool_max">>, H, 1)),
		valida_lang(Lang),
		valida_name_service(Name),
		valida_type_service(Type),
		valida_bool(Apikey),
		valida_bool(Async),
		valida_length(Comment, 1000),
		valida_length(Version, 10),
		valida_length(Owner, 30),
		valida_authentication(Authentication),
		valida_bool(Debug),
		valida_bool(UseRE),
		case Lang of
			<<"erlang">> -> 
				Node = <<>>,
				Host = '',
				HostName = Conf#config.ems_hostname;
			_ ->	
				Node = parse_node_service(maps:get(<<"node">>, H, Conf#config.cat_node_search)),
				{Host, HostName} = parse_host_service(maps:get(<<"host">>, H, Conf#config.cat_host_search), ModuleNameCanonical, Node, Conf)
		end,
		{Querystring, QtdQuerystringRequired} = parse_querystring(maps:get(<<"querystring">>, H, <<>>)),
		IdBin = list_to_binary(integer_to_list(Id)),
		ServiceView = new_service_view(IdBin, Name, Url, ModuleName, FunctionName, 
										 Type, Apikey, Comment, Version, Owner, 
										 Async, Host, Result_Cache, 
										 Authentication, Node, Lang,
										 Datasource,
										 Debug, SchemaIn, SchemaOut),
		case is_url_com_re(binary_to_list(Url2)) orelse ModuleName =:= "ems_static_file_service" orelse ModuleName =:= "ems_options_service" of
			true -> 
				Service = new_service_re(Rowid, IdBin, Name, Url2, 
										   ServiceImpl,
										   ModuleName, 
										   ModuleNameCanonical,
										   FunctionName, Type, Apikey, Comment, 
										   Version, Owner, Async, 
										   Querystring, QtdQuerystringRequired,
										   Host, HostName, Result_Cache,
										   Authentication, Node, Lang,
										   Datasource, 
										   Debug, SchemaIn, SchemaOut, PoolSize, PoolMax, H),
				parse_catalog(T, Cat2, [Service|Cat3], [ServiceView|Cat4], CatK, Id+1, Conf);
			false -> 
				Service = new_service(Rowid, IdBin, Name, Url2, 
										ServiceImpl,
										ModuleName,
										ModuleNameCanonical,
										FunctionName, Type, Apikey, Comment,
										Version, Owner, Async, 
										Querystring, QtdQuerystringRequired,
										Host, HostName, Result_Cache,
										Authentication, Node, Lang,
										Datasource, 
										Debug, SchemaIn, SchemaOut, PoolSize, PoolMax, H)
		end,
		case Type of
			<<"KERNEL">> -> 
				parse_catalog(T, Cat2, Cat3, [ServiceView|Cat4], [Service|CatK], Id+1, Conf);
			_ -> parse_catalog(T, [{Rowid, Service}|Cat2], Cat3, [ServiceView|Cat4], CatK, Id+1, Conf)
		end
	catch
		_Exception:Reason -> {error, Reason}
	end.
	
	
parse_schema(null) -> null;
parse_schema(Name) -> Name.

	
parse_datasource(null) -> null;
parse_datasource(M) -> 
	#service_datasource{type = list_to_atom(binary_to_list(maps:get(<<"type">>, M))),
						connection = binary_to_list(maps:get(<<"connection">>, M, <<>>)),
						table_name = binary_to_list(maps:get(<<"table_name">>, M, <<>>)),
						primary_key = binary_to_list(maps:get(<<"primary_key">>, M, <<>>)),
						csv_delimiter = binary_to_list(maps:get(<<"csv_delimiter">>, M, <<";">>)),
						sql = binary_to_list(maps:get(<<"sql">>, M, <<>>)),
						timeout = maps:get(<<"timeout">>, M, ?MAX_TIME_ODBC_QUERY)
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
parse_host_service(Host, ModuleNameCanonical, Node, Conf) ->
	HostAlias = Conf#config.cat_host_alias,
	case erlang:is_list(Host) of
		true  -> ListHost = Host;
		false -> ListHost = [Host]
	end,
	case erlang:is_list(Node) of
		true  -> ListNode = Node;
		false -> ListNode = [Node]
	end,
	ListHost2 = lists:map(fun(X) -> binary_to_list(maps:get(X, HostAlias, X)) end, ListHost),
	ListNode2 = lists:map(fun(X) -> binary_to_list(X) end, ListNode),
	ClusterName = [case X of
						[] -> ModuleNameCanonical ++ "@" ++ Y;
						_  -> ModuleNameCanonical ++ "_" ++ X ++ "@" ++ Y 
				   end || X <- ListNode2, Y <- ListHost2],
	ClusterNode = lists:map(fun(X) -> list_to_atom(X) end, ClusterName),
	{ClusterNode, ClusterName}.
	

new_service_re(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName, 
			   Type, Apikey, Comment, Version, Owner, Async, Querystring, 
			   QtdQuerystringRequired, Host, HostName, Result_Cache,
			   Authentication, Node, Lang, Datasource,
			   Debug, SchemaIn, SchemaOut, PoolSize, PoolMax, Properties) ->
	{ok, Id_re_compiled} = re:compile(Rowid),
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
			    apikey = ems_util:binary_to_bool(Apikey),
			    comment = Comment,
			    version = Version,
			    owner = Owner,
				async = ems_util:binary_to_bool(Async),
			    querystring = Querystring,
			    qtd_querystring_req = QtdQuerystringRequired,
			    host = Host,
			    host_name = HostName,
			    result_cache = Result_Cache,
			    authentication = Authentication,
			    node = Node,
			    datasource = Datasource,
			    debug = Debug,
			    lang = Lang,
			    schema_in = SchemaIn,
			    schema_out = SchemaOut,
			    pool_size = PoolSize,
			    pool_max = PoolMax,
			    properties = Properties
			}.

new_service(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName,
			Type, Apikey, Comment, Version, Owner, Async, Querystring, 
			QtdQuerystringRequired, Host, HostName, Result_Cache,
			Authentication, Node, Lang, Datasource, 
			Debug, SchemaIn, SchemaOut, PoolSize, PoolMax, Properties) ->
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
			    apikey = ems_util:binary_to_bool(Apikey),
			    comment = Comment,
			    version = Version,
			    owner = Owner,
			    async = ems_util:binary_to_bool(Async),
			    querystring = Querystring,
			    qtd_querystring_req = QtdQuerystringRequired,
			    host = Host,
			    host_name = HostName,
			    result_cache = Result_Cache,
			    authentication = Authentication,
			    node = Node,
			    datasource = Datasource,
			    debug = Debug,
			    lang = Lang,
			    schema_in = SchemaIn,
			    schema_out = SchemaOut,
			    pool_size = PoolSize,
			    pool_max = PoolMax,
			    properties = Properties
			}.

new_service_view(Id, Name, Url, ModuleName, FunctionName, Type, Apikey,
				  Comment, Version, Owner, Async, Host, Result_Cache,
				  Authentication, Node, Lang, 
				  _Datasource, 
				  Debug, SchemaIn, SchemaOut) ->
	Service = #{<<"id">> => Id,
				<<"name">> => Name,
				<<"url">> => Url,
				<<"type">> => Type,
			    <<"module">> => list_to_binary(ModuleName),
			    <<"function">> => list_to_binary(FunctionName),
			    <<"apikey">> => Apikey,
			    <<"comment">> => Comment,
			    <<"version">> => Version,
			    <<"owner">> => Owner,
			    <<"async">> => Async,
			    <<"host">> => Host,
			    <<"result_cache">> => Result_Cache,
			    <<"authentication">> => Authentication,
			    <<"node">> => Node,
			    <<"debug">> => Debug,
			    <<"schema_in">> => SchemaIn,
			    <<"schema_out">> => SchemaOut,
			    <<"lang">> => Lang},
	Service.


