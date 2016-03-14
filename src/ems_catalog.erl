%%********************************************************************
%% @title Module ems_catalog
%% @version 1.0.0
%% @doc Module responsible for catalog management services
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog).

-compile(export_all).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Client
-export([init_catalog/0,
		 list_catalog/0, 
		 update_catalog/0,
		 lookup/1,
		 get_querystring/2, 
		 get_ult_lookup/0,
		 list_cat2/0, list_cat3/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do catálogo. 
-record(state, {cat1, 			%% Catalog JSON
				cat2, 			%% Parsed catalog 
				cat3, 			%% Regular expression parsed catalog
				ult_lookup, 	%% Last lookup performed
				ult_rowid,		%% Rowid of the last request
				tbl_cache_lookup = [],
				tbl_cache_index = 0
		}). 


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Client API
%%====================================================================
 
list_catalog() ->
	ems_pool:call(ems_catalog_pool, list_catalog).

update_catalog() ->
	ems_pool:cast(ems_catalog_pool, update_catalog).

lookup(Request) ->	
	ems_pool:call(ems_catalog_pool, {lookup, Request}).

list_cat2() ->
	ems_pool:call(ems_catalog_pool, list_cat2).

list_cat3() ->
	ems_pool:call(ems_catalog_pool, list_cat3).

get_ult_lookup() ->
	ems_pool:call(ems_catalog_pool, get_ult_lookup).

	
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
	case ets:lookup(ets_ems_catalog, cat) of
		[] -> 
			{stop, nocatalog};
		[{cat, {Cat1, Cat2, Cat3}}] ->
			{ok, #state{cat1 = Cat1, cat2 = Cat2, cat3 = Cat3}}
	end.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(update_catalog, _State) ->
	NewState = get_catalog(),
	{noreply, NewState}.

handle_call(list_catalog, _From, State) ->
	Reply = do_list_catalog(State),
	{reply, Reply, State};
    
handle_call({lookup, Request}, _From, State) ->
	Ult_lookup = lookup(Request, State),
	%NewState = add_lookup_cache(Request#request.rowid, Ult_lookup, State),
	{reply, Ult_lookup, State, 60000};

handle_call(list_cat2, _From, State) ->
	{reply, State#state.cat2, State};

handle_call(list_cat3, _From, State) ->
	{reply, State#state.cat3, State};

handle_call(get_ult_lookup, _From, State) ->
	{reply, State#state.ult_lookup, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================

init_catalog() ->
	ets:new(ets_ems_catalog, [set, named_table, public]),
	case get_catalog() of
		{ok, Cat1, Cat2, Cat3} -> 
			ets:insert(ets_ems_catalog, {cat, {Cat1, Cat2, Cat3}}),
			ok;
		{error, emfile} ->
			ems_logger:error("Falha ao carregar o catálogo de serviços para o processo ~p. Muitos arquivos abertos no sistema operacional.", [self()]),
			{error, invalidcatalog};
		{error, eacces} ->
			ems_logger:error("Falha ao carregar o catálogo de serviços para o processo ~p. Não tem permissão para ler o catálogo de serviços.", [self()]),
			{error, invalidcatalog};
		{error, enoent} ->
			ems_logger:error("Falha ao carregar o catálogo de serviços para o processo ~p. Catálogo de serviços não encontrado.", [self()]),
			{error, invalidcatalog};
		{error, enamem} ->
			ems_logger:error("Falha ao carregar o catálogo de serviços para o processo ~p. Não há memória suficiente para ler o catálogo de serviços.", [self()]),
			{error, invalidcatalog};
		{error, Reason} ->
			ems_logger:error("Falha ao carregar o catálogo de serviços para o processo ~p. Erro interno: ~p.", [self(), Reason]),
			{stop, invalidcatalog}
	end.
	


add_lookup_cache(Ult_rowid, Ult_lookup, State=#state{tbl_cache_lookup=Tbl,
													 tbl_cache_index=Index}) when Index < 10 ->
	State#state{tbl_cache_lookup = [{Ult_rowid, Ult_lookup}|Tbl],
	            tbl_cache_index = Index+1};

add_lookup_cache(Ult_rowid, Ult_lookup, State) ->
	State#state{tbl_cache_lookup = [{Ult_rowid, Ult_lookup}], 
				tbl_cache_index = 1}.


lookup_cache(Rowid, State) ->
	lookup_cache_tail(State#state.tbl_cache_lookup, Rowid).

lookup_cache_tail([], _Rowid) -> [];

lookup_cache_tail([{Ult_rowid, Ult_lookup}|T], Rowid) ->
	case Rowid == Ult_rowid of
		true -> Ult_lookup;
		false -> lookup_cache_tail(T, Rowid)
	end.


%% @doc Serviço que lista o catálogo
do_list_catalog(State) -> State#state.cat1.

%% @doc Obtém o catálogo
get_catalog() -> 
	case get_main_catalog() of
		{ok, CatMestre} ->
			CatalogoDefsPath = ?CONF_PATH ++ "/catalog/",
			%% Obtém a lista do conteúdo de todos os catálogos
			CatDefs = lists:map(fun(M) -> 
									NomeArq = CatalogoDefsPath ++ binary_to_list(maps:get(<<"file">>, M)),
									NomeCatalogo = binary_to_list(maps:get(<<"catalog">>, M)),
									case file:read_file(NomeArq) of
										{ok, Arq} -> Arq;
										{error, enoent} -> 
											ems_logger:error("Catalog ~s not found. Filename: ~s.", [NomeCatalogo, NomeArq]),
											<<>>
									end
								end, CatMestre),

			%% Adiciona "," entre as definições de cada catálogo
			CatDefs1 = lists:foldl(fun(X, Y) ->
										case Y of
											<<>> -> X;
											Y2 -> iolist_to_binary([X, <<",">>, Y2])
										end 
									end, <<>>, CatDefs),

			%% Adiciona abertura e fechamento de lista para o parser correto do JSON
			CatDefs2 = iolist_to_binary([<<"[">>, CatDefs1, <<"]">>]),
			{ok, Cat1} = ems_util:json_decode_as_map(CatDefs2),
			%% Faz o parser do catálogo
			Conf = ems_config:getConfig(),
			{Cat2, Cat3, Cat4} = parse_catalog(Cat1, [], [], [], 1, Conf),
			{ok, Cat4, Cat2, Cat3};
		Error -> Error
	end.
			

%% @doc O catálogo mestre possui os includes para os catálogos

get_main_catalog() ->
	case file:read_file(?CATALOGO_PATH) of
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
is_pseudo_name_param_valido(Name) ->
	case re:run(Name, "^[a-z0-9]{0,29}$") of
		nomatch -> false;
		_ -> true
	end.

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
valida_pseudo_name_param(Name) ->
	case is_pseudo_name_param_valido(Name) of
		true -> ok;
		false -> erlang:error(invalid_pseudo_name_param)
	end.

%% @doc Valida o tipo de dado da querystring
valida_type_querystring(Type) ->
	case is_type_valido(Type) of
		true -> ok;
		false -> erlang:error(invalid_type_querystring)
	end.

%% @doc Valida o método do serviço 
valida_type_service(Type) ->
	case ems_http_util:is_metodo_suportado(Type) of
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
valida_bool(_) -> erlang:error(invalid_bool).

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
pseudoparam_to_re(":id")   -> "(?<id>[0-9]{1,9})";
pseudoparam_to_re(":top")  -> "(?<top>[0-9]{1,4})";
pseudoparam_to_re(_)  -> erlang:error(invalid_pseudo_param).
pseudoparam_to_re(":id", Nome)  -> io_lib:format("(?<id_~s>[0-9]{1,9})", [Nome]);
pseudoparam_to_re(":top", Nome) -> io_lib:format("(?<top_~s>[0-9]{1,4})", [Nome]);
pseudoparam_to_re(_, _)  -> erlang:error(invalid_pseudo_param).

%% @doc Faz o parser da URL convertendo os pseudo parâmetros em expressão regular
parse_url_service(<<Url/binary>>) ->
	Url1 = string:tokens(binary_to_list(Url), "/"),
	parse_url_service(Url1, []).

parse_url_service([], []) -> <<"/">>;
parse_url_service([], Url) -> list_to_binary(["/" | string:join(lists:reverse(Url), "/")]);
parse_url_service([H|T], Url) when hd(H) /= $: -> parse_url_service(T, [H | Url]);
parse_url_service([H|T], Url) ->
	case string:chr(H, $_) > 0 of
		true ->
			[Pseudo, Nome] = string:tokens(H, "_"),
			valida_pseudo_name_param(Nome),
			P = pseudoparam_to_re(Pseudo, Nome),
			parse_url_service(T, [P | Url]);
		false ->
			Pseudo = H,
			P = pseudoparam_to_re(Pseudo),
			parse_url_service(T, [P | Url])
	end.

%% @doc Faz o parser dos contratos de serviços no catálogo de serviços
parse_catalog([], Cat2, Cat3, Cat4, _Id, _Conf) ->
	EtsCat2 = ems_util:list_to_ets(Cat2, ets_cat2, [set, 
													  public, 
													  {read_concurrency, true}]),
	{EtsCat2, Cat3, Cat4};
	
parse_catalog([H|T], Cat2, Cat3, Cat4, Id, Conf) ->
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
	case Lang of
		<<"erlang">> -> 
			Node = <<>>,
			Host = '',
			HostName = Conf#config.ems_hostname;
		_ ->	
			Node = parse_node_service(maps:get(<<"node">>, H, Conf#config.cat_node_search)),
			{Host, HostName} = parse_host_service(maps:get(<<"host">>, H, Conf#config.cat_host_search), ModuleNameCanonical, Node, Conf)
	end,
	Result_Cache = maps:get(<<"result_cache">>, H, 0),
	Authentication = maps:get(<<"authentication">>, H, <<>>),
	valida_name_service(Name),
	valida_type_service(Type),
	valida_bool(Apikey),
	valida_bool(Async),
	valida_length(Comment, 1000),
	valida_length(Version, 10),
	valida_length(Owner, 30),
	valida_authentication(Authentication),
	{Querystring, QtdQuerystringRequired} = parse_querystring(maps:get(<<"querystring">>, H, <<>>)),
	IdBin = list_to_binary(integer_to_list(Id)),
	ServiceView = new_service_view(IdBin, Name, Url, ModuleName, FunctionName, 
							         Type, Apikey, Comment, Version, Owner, 
								     Async, Host, Result_Cache, Authentication, Node, Lang),
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
									   Authentication, Node, Lang),
			parse_catalog(T, Cat2, [Service|Cat3], [ServiceView|Cat4], Id+1, Conf);
		false -> 
			Service = new_service(Rowid, IdBin, Name, Url2, 
									ServiceImpl,
									ModuleName,
									ModuleNameCanonical,
									FunctionName, Type, Apikey, Comment,
									Version, Owner, Async, 
									Querystring, QtdQuerystringRequired,
									Host, HostName, Result_Cache,
									Authentication, Node, Lang),
			parse_catalog(T, [{Rowid, Service}|Cat2], Cat3, [ServiceView|Cat4], Id+1, Conf)
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
					case QuerystringServico =:= <<>> of
						true -> QuerystringUser;
						false -> valida_querystring(QuerystringServico, QuerystringUser)
					end;
				false -> 
					case QuerystringServico =:= <<>> of
						true -> notfound;
						false -> valida_querystring(QuerystringServico, QuerystringUser)
					end
			end
	end.

valida_querystring(QuerystringServico, QuerystringUser) ->
	case valida_querystring(QuerystringServico, QuerystringUser, []) of
		{ok, Querystring} -> Querystring;
		notfound -> notfound
	end.

valida_querystring([], _QuerystringUser, QuerystringList) ->
	{ok, maps:from_list(QuerystringList)};

valida_querystring([H|T], QuerystringUser, QuerystringList) ->
	%% Verifica se encontra a query na querystring do usuário
	NomeQuery = maps:get(<<"name">>, H),
	case maps:find(NomeQuery, QuerystringUser) of
		{ok, Value} -> valida_querystring(T, QuerystringUser, [{NomeQuery, Value} | QuerystringList]);
		error ->
			%% se o usuário não informou a querystring, verifica se tem valor default na definição do serviço
			case maps:get(<<"default">>, H, notfound) of
				notfound -> notfound;
				Value -> valida_querystring(T, QuerystringUser, [{NomeQuery, Value} | QuerystringList])
			end
	end.


lookup(Request, State) ->
	Rowid = Request#request.rowid,
	ems_logger:info("REQUEST ROWID ~p.", [Request#request.rowid]),
	case ets:lookup(State#state.cat2, Rowid) of
		[] -> 
			case lookup_re(Request, State#state.cat3) of
				{Service, ParamsMap} -> 
					Querystring = processa_querystring(Service, Request),
					{Service, ParamsMap, Querystring};
				notfound -> notfound
			end;
		[{_Rowid, Service}] -> 
			Querystring = processa_querystring(Service, Request),
			{Service, Request#request.params_url, Querystring}
	end.


lookup_re(_Request, []) ->
	notfound;

lookup_re(Request, [H|T]) ->
	RE = H#service.id_re_compiled,
	case re:run(Request#request.rowid, RE, [{capture,all_names,binary}]) of
		match -> {H, #{}};
		{match, Params} -> 
			{namelist, ParamNames} = re:inspect(RE, namelist),
			ParamsMap = maps:from_list(lists:zip(ParamNames, Params)),
			{H, ParamsMap};
		nomatch -> lookup_re(Request, T);
		{error, _ErrType} -> notfound 
	end.

new_service_re(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName, 
			   Type, Apikey, Comment, Version, Owner, Async, Querystring, 
			   QtdQuerystringRequired, Host, HostName, Result_Cache,
			   Authentication, Node, Lang) ->
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
			    lang = Lang
			}.

new_service(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName,
			Type, Apikey, Comment, Version, Owner, Async, Querystring, 
			QtdQuerystringRequired, Host, HostName, Result_Cache,
			Authentication, Node, Lang) ->
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
			    lang = Lang
			}.

new_service_view(Id, Name, Url, ModuleName, FunctionName, Type, Apikey,
				  Comment, Version, Owner, Async, Host, Result_Cache,
				  Authentication, Node, Lang) ->
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
			    <<"lang">> => Lang},
	Service.


