%%********************************************************************
%% @title Módulo catálogo de serviços
%% @version 1.0.0
%% @doc Módulo responsável pelo gerenciamento do catálogo de serviços
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_catalogo).

-compile(export_all).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Client
-export([lista_catalogo/0, 
		 update_catalogo/0,
		 lookup/1,
		 get_querystring/2, 
		 get_ult_lookup/0,
		 list_cat2/0, list_cat3/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do catálogo. 
-record(state, {cat1, 		%% Catalogo JSON
				cat2, 		%% Parsed catalog 
				cat3, 		%% Regular expression parsed catalog
				ult_lookup 	%% Último lookup realizado
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
%% Cliente API
%%====================================================================
 
lista_catalogo() ->
	msbus_pool:call(msbus_catalogo_pool, lista_catalogo).

update_catalogo() ->
	msbus_pool:cast(msbus_catalogo_pool, update_catalogo).

lookup(Request) ->	
	msbus_pool:call(msbus_catalogo_pool, {lookup, Request}).

list_cat2() ->
	msbus_pool:call(msbus_catalogo_pool, list_cat2).

list_cat3() ->
	msbus_pool:call(msbus_catalogo_pool, list_cat3).

get_ult_lookup() ->
	msbus_pool:call(msbus_catalogo_pool, get_ult_lookup).

	
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
	%% Cat1 = JSON catalog, Cat2 = parsed catalog, Cat3 = regular expression parsed catalog
	NewState = get_catalogo(),
    {ok, NewState}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(update_catalogo, _State) ->
	NewState = get_catalogo(),
	{noreply, NewState}.

handle_call(lista_catalogo, _From, State) ->
	Reply = do_lista_catalogo(State),
	{reply, Reply, State};
    
handle_call({lookup, Request}, _From, State) ->
	{Reply, NewState} = lookup(Request, State),
	{reply, Reply, NewState, 60000};

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

%% @doc Serviço que lista o catálogo
do_lista_catalogo(State) -> State#state.cat1.

%% @doc Obtém o catálogo
get_catalogo() -> 
	%% O arquivo do catálogo contém os includes para os catálogos com as definições de serviço
	Cat0 = get_catalogo_from_disk(),
	CatalogoDefsPath = ?CONF_PATH ++ "/catalogo/",
	%% Obtém a lista do conteúdo de todos os catálogos
	CatDefs = lists:map(fun(M) -> 
							NomeArq = CatalogoDefsPath ++ binary_to_list(maps:get(<<"file">>, M)),
							NomeCatalogo = binary_to_list(maps:get(<<"catalogo">>, M)),
							case file:read_file(NomeArq) of
								{ok, Arq} -> Arq;
								{error, enoent} -> 
									msbus_logger:error("Catalogo ~s não foi encontrado. Arquivo: ~s.", [NomeCatalogo, NomeArq]),
									<<>>
							end
					    end, Cat0),
	%% Adiciona "," entre as definições de cada catálogo
	CatDefs1 = lists:foldl(fun(X, Y) ->
								case Y of
									<<>> -> X;
									Y2 -> iolist_to_binary([X, <<",">>, Y2])
								end 
						    end, <<>>, CatDefs),

	%% Adiciona abertura e fechamento de lista para o parser correto do JSON
	CatDefs2 = iolist_to_binary([<<"[">>, CatDefs1, <<"]">>]),
	{ok, Cat1} = msbus_util:json_decode_as_map(CatDefs2),
	%% Faz o parser do catálogo
	Conf = msbus_config:getConfig(),
	{Cat2, Cat3, Cat4} = parse_catalogo(Cat1, [], [], [], 1, Conf),
	#state{cat1=Cat4, cat2=Cat2, cat3=Cat3}.

%% @doc Lê o catálogo do disco
get_catalogo_from_disk() ->
	{ok, Cat} = file:read_file(?CATALOGO_PATH),
	{ok, Cat2} = msbus_util:json_decode_as_map(Cat),
	Cat2.

%% @doc Indica se o nome da querystring é valido
is_name_querystring_valido(Name) ->
	case re:run(Name, "^[_a-zA-Z][_a-zA-Z0-9]{0,29}$") of
		nomatch -> false;
		_ -> true
	end.

%% @doc Indica se o nome do pseudo param é valido
is_pseudo_name_param_valido(Name) ->
	case re:run(Name, "^[a-z0-9]{0,29}$") of
		nomatch -> false;
		_ -> true
	end.

%% @doc Indica se o nome da querystring é valido
is_name_servico_valido(Name) ->
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

%% @doc Valida o nome do serviço
valida_name_contract(Name) ->
	case is_name_servico_valido(Name) of
		true -> ok;
		false -> erlang:error(invalid_name_servico)
	end.

%% @doc Valida o nome da querystring
valida_name_querystring(Name) ->
	case is_name_querystring_valido(Name) of
		true -> ok;
		false -> erlang:error(invalid_name_querystring)
	end.

%% @doc Valida o nome do pseudo param
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
valida_type_contract(Type) ->
	case msbus_http_util:is_metodo_suportado(Type) of
		true -> ok;
		false -> erlang:error(invalid_type_servico)
	end.

valida_url_contract(<<"/">>) -> ok;
valida_url_contract(Url) ->
	case msbus_http_util:is_url_valido(Url) andalso is_valid_length(Url, 300) of
		true -> ok;
		false -> erlang:error(invalid_url_servico)
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
parse_url_servico(<<Url/binary>>) ->
	Url1 = string:tokens(binary_to_list(Url), "/"),
	parse_url_servico(Url1, []).

parse_url_servico([], []) -> <<"/">>;
parse_url_servico([], Url) -> list_to_binary(["/" | string:join(lists:reverse(Url), "/")]);
parse_url_servico([H|T], Url) when hd(H) /= $: -> parse_url_servico(T, [H | Url]);
parse_url_servico([H|T], Url) ->
	case string:chr(H, $_) > 0 of
		true ->
			[Pseudo, Nome] = string:tokens(H, "_"),
			valida_pseudo_name_param(Nome),
			P = pseudoparam_to_re(Pseudo, Nome),
			parse_url_servico(T, [P | Url]);
		false ->
			Pseudo = H,
			P = pseudoparam_to_re(Pseudo),
			parse_url_servico(T, [P | Url])
	end.

%% @doc Faz o parser dos contratos de serviços no catálogo de serviços
parse_catalogo([], Cat2, Cat3, Cat4, _Id, _Conf) ->
	{maps:from_list(Cat2), Cat3, Cat4};
parse_catalogo([H|T], Cat2, Cat3, Cat4, Id, Conf) ->
	Name = maps:get(<<"name">>, H),
	Url = maps:get(<<"url">>, H),
	Url2 = parse_url_servico(Url),
	Service = maps:get(<<"service">>, H),
	{ModuleName, ModuleNameCanonical, FunctionName} = parse_service_contract(Service),
	Type = maps:get(<<"type">>, H, <<"GET">>),
	Apikey = maps:get(<<"APIkey">>, H, <<"false">>),
	Comment = maps:get(<<"comment">>, H, <<>>),
	Version = maps:get(<<"version">>, H, <<>>),
	Owner = maps:get(<<"owner">>, H, <<>>),
	Async = maps:get(<<"async">>, H, <<"false">>),
	Rowid = new_rowid_servico(Url2, Type),
	Lang = maps:get(<<"lang">>, H, <<>>),
	case Lang of
		<<"erlang">> -> 
			Node = <<>>,
			Host = '',
			HostName = Conf#config.msbus_hostname;
		_ ->	
			Node = parse_node_contract(maps:get(<<"node">>, H, Conf#config.cat_node_search)),
			{Host, HostName} = parse_host_contract(maps:get(<<"host">>, H, Conf#config.cat_host_search), ModuleNameCanonical, Node, Conf)
	end,
	Result_Cache = maps:get(<<"result_cache">>, H, 0),
	Authentication = maps:get(<<"authentication">>, H, <<>>),
	valida_name_contract(Name),
	valida_url_contract(Url2),
	valida_type_contract(Type),
	valida_bool(Apikey),
	valida_bool(Async),
	valida_length(Comment, 1000),
	valida_length(Version, 10),
	valida_length(Owner, 30),
	valida_authentication(Authentication),
	{Querystring, QtdQuerystringRequired} = parse_querystring(maps:get(<<"querystring">>, H, <<>>)),
	IdBin = list_to_binary(integer_to_list(Id)),
	ContractView = new_contract_view(IdBin, Name, Url, ModuleName, FunctionName, 
							         Type, Apikey, Comment, Version, Owner, 
								     Async, Host, Result_Cache, Authentication, Node, Lang),
	case is_url_com_re(binary_to_list(Url2)) orelse ModuleName =:= "msbus_static_file_service" orelse ModuleName =:= "msbus_options_service" of
		true -> 
			Contract = new_contract_re(Rowid, IdBin, Name, Url2, 
									   Service,
									   ModuleName, 
									   ModuleNameCanonical,
									   FunctionName, Type, Apikey, Comment, 
									   Version, Owner, Async, 
									   Querystring, QtdQuerystringRequired,
									   Host, HostName, Result_Cache,
									   Authentication, Node, Lang),
			parse_catalogo(T, Cat2, [Contract|Cat3], [ContractView|Cat4], Id+1, Conf);
		false -> 
			Contract = new_contract(Rowid, IdBin, Name, Url2, 
									Service,
									ModuleName,
									ModuleNameCanonical,
									FunctionName, Type, Apikey, Comment,
									Version, Owner, Async, 
									Querystring, QtdQuerystringRequired,
									Host, HostName, Result_Cache,
									Authentication, Node, Lang),
			parse_catalogo(T, [{Rowid, Contract}|Cat2], Cat3, [ContractView|Cat4], Id+1, Conf)
	end.	

parse_service_contract(Service) ->
	try
		[ModuleName, FunctionName] = binary:split(Service, <<":">>),
		ModuleName2 = binary_to_list(ModuleName),
		FunctionName2 = binary_to_list(FunctionName),
		ModuleNameCanonical = lists:last(string:tokens(ModuleName2, ".")),
		{ModuleName2, ModuleNameCanonical, FunctionName2}
	catch
		_Exception:_Reason ->  erlang:error(invalid_service_contract)
	end.
	
parse_node_contract(<<>>) -> <<>>;
parse_node_contract(List) -> List.

	
%% @doc O host pode ser um alias definido no arquivo de configuração
parse_host_contract(<<>>, _,_,_) -> {'', atom_to_list(node())};
parse_host_contract(Host, ModuleNameCanonical, Node, Conf) ->
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
	[Query] = [Q || Q <- maps:get(<<"querystring">>, Servico, <<>>), Q#servico.comment == QueryName],
	Query.

processa_querystring(notfound) -> notfound;
	
processa_querystring({ok, Request}) ->
	%% Querystrings do módulo msbus_static_file_service e msbus_options_service não são processados.
	case Request#request.servico#servico.module of
		msbus_static_file_service ->
			{ok, Request};
		msbus_options_service ->
			{ok, Request};
		_ ->
			QuerystringServico = Request#request.servico#servico.querystring,
			QuerystringUser = Request#request.querystring_map,
			case Request#request.querystring =:= "" of
				true -> 
					case QuerystringServico =:= <<>> of
						true -> {ok, Request};
						false -> valida_querystring({ok, Request}, QuerystringServico, QuerystringUser)
					end;
				false -> 
					case QuerystringServico =:= <<>> of
						true -> notfound;
						false -> valida_querystring({ok, Request}, QuerystringServico, QuerystringUser)
					end
			end
	end.

valida_querystring({ok, Request}, QuerystringServico, QuerystringUser) ->
	case valida_querystring(QuerystringServico, QuerystringUser, []) of
		{ok, Querystring} -> 
			Request2 = Request#request{querystring_map = Querystring},
			{ok, Request2};
		notfound -> notfound
	end;

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
	Rowid = new_rowid_servico(Request#request.url, Request#request.type),
	case maps:find(Rowid, State#state.cat2) of
		{ok, Servico} -> 
			Request2 = Request#request{servico = Servico},
			Result = {ok, Request2};
		error -> 
			Result = lookup_re(Request, State#state.cat3)
	end,
	Result2 = processa_querystring(Result),
	{Result2, State#state{ult_lookup = Result2}}.

lookup_re(_Request, []) ->
	notfound;

lookup_re(Request, [H|T]) ->
	RE = H#servico.id_re_compiled,
	Rowid = new_rowid_servico(Request#request.url, Request#request.type),
	case re:run(Rowid, RE, [{capture,all_names,binary}]) of
		match -> 
			Request2 = Request#request{servico = H},
			{ok, Request2};
		{match, Params} -> 
			{namelist, ParamNames} = re:inspect(RE, namelist),
			ParamsMap = maps:from_list(lists:zip(ParamNames, Params)),
			Request2 = Request#request{servico = H, params_url = ParamsMap},
			{ok, Request2};
		nomatch -> lookup_re(Request, T);
		{error, _ErrType} -> notfound 
	end.

new_rowid_servico(<<Url/binary>>, <<Type/binary>>) ->	
	[PrefixUrl|Url2] = binary_to_list(Url),
	case PrefixUrl of
		$^ -> iolist_to_binary([Type, <<"#">>, list_to_binary(Url2)]);
		_  -> iolist_to_binary([Type, <<"#">>, Url])
	end;

new_rowid_servico(Url, Type) ->	
	[PrefixUrl|Url2] = Url,
	case PrefixUrl of
		$^ -> iolist_to_binary([Type, <<"#">>, Url2]);
		_  -> iolist_to_binary([Type, <<"#">>, Url])
	end.
	
new_contract_re(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName, 
			   Type, Apikey, Comment, Version, Owner, Async, Querystring, 
			   QtdQuerystringRequired, Host, HostName, Result_Cache,
			   Authentication, Node, Lang) ->
	{ok, Id_re_compiled} = re:compile(Rowid),
	#servico{
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
			    apikey = msbus_util:binary_to_bool(Apikey),
			    comment = Comment,
			    version = Version,
			    owner = Owner,
				async = msbus_util:binary_to_bool(Async),
			    querystring = Querystring,
			    qtd_querystring_req = QtdQuerystringRequired,
			    host = Host,
			    host_name = HostName,
			    result_cache = Result_Cache,
			    authentication = Authentication,
			    node = Node,
			    lang = Lang
			}.

new_contract(Rowid, Id, Name, Url, Service, ModuleName, ModuleNameCanonical, FunctionName,
			Type, Apikey, Comment, Version, Owner, Async, Querystring, 
			QtdQuerystringRequired, Host, HostName, Result_Cache,
			Authentication, Node, Lang) ->
	#servico{
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
			    apikey = msbus_util:binary_to_bool(Apikey),
			    comment = Comment,
			    version = Version,
			    owner = Owner,
			    async = msbus_util:binary_to_bool(Async),
			    querystring = Querystring,
			    qtd_querystring_req = QtdQuerystringRequired,
			    host = Host,
			    host_name = HostName,
			    result_cache = Result_Cache,
			    authentication = Authentication,
			    node = Node,
			    lang = Lang
			}.

new_contract_view(Id, Name, Url, ModuleName, FunctionName, Type, Apikey,
				  Comment, Version, Owner, Async, Host, Result_Cache,
				  Authentication, Node, Lang) ->
	Contract = #{<<"id">> => Id,
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
	Contract.


