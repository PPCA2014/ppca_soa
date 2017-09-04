%%********************************************************************
%% @title Module ems_catalog
%% @version 1.0.0
%% @doc Module responsible for catalog management services
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Server API
-export([start/1, start_link/1, stop/0, get_metadata_json/1]).

%% Client
-export([list_catalog/0, 
		 lookup/1,
		 lookup/2,
		 get_querystring/2, 
		 get_ult_lookup/0,
		 list_cat2/0, 
		 list_cat3/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do catálogo. 
-record(state, {cat1, 			%% Catalog JSON
				cat2, 			%% Parsed catalog 
				cat3, 			%% Regular expression parsed catalog
				catk, 			%% Kernel catalog
				ult_lookup, 	%% Last lookup performed
				ult_rowid,		%% Rowid of the last request
				tbl_cache_lookup = [],
				tbl_cache_index = 0
		}). 


%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Client API
%%====================================================================

list_catalog() ->
	gen_server:call(?SERVER, list_catalog).

lookup(Request = #request{type = Type, rowid = Rowid, params_url = Params}) ->	
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
		[] -> 
			gen_server:call(?SERVER, {lookup, Request});
		[{_Rowid, Service}] -> 
			case processa_querystring(Service, Request) of
			   enoent -> {error, enoent};
			   Querystring -> {Service, Params, Querystring}
			end
	end.

lookup(Method, Uri) ->
	gen_server:call(?SERVER, {lookup, Method, Uri}).

list_cat2() ->
	gen_server:call(?SERVER, list_cat2).

list_cat3() ->
	gen_server:call(?SERVER, list_cat3).

get_ult_lookup() ->
	gen_server:call(?SERVER, get_ult_lookup).


	
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
	case ets:lookup(ets_ems_catalog, cat) of
		[] -> 
			{stop, nocatalog};
		[{cat, {Cat1, Cat2, Cat3, CatK}}] ->
			{ok, #state{cat1 = Cat1, cat2 = Cat2, cat3 = Cat3, catk = CatK}}
	end.
    
handle_cast(shutdown, State) ->
    {stop, normal, State}.

handle_call(list_catalog, _From, State) ->
	Reply = do_list_catalog(State),
	{reply, Reply, State};

handle_call({lookup, Request}, _From, State) ->
	Ult_lookup = do_lookup(Request, State),
	%NewState = add_lookup_cache(Request#request.rowid, Ult_lookup, State),
	{reply, Ult_lookup, State, 60000};

handle_call({lookup, Method, Uri}, _From, State) ->
	Ult_lookup = do_lookup(Method, Uri, State),
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

%% @doc Serviço que lista o catálogo
do_list_catalog(State) -> State#state.cat1.

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


do_lookup(Method, Uri, State) ->
	case ems_http_util:encode_request(Method, Uri) of
		{ok, Request} -> do_lookup(Request, State);
		{error, Reason} -> {error, Reason}
	end.


do_lookup(Request, #state{cat3 = Cat}) ->
	case do_lookup_re(Request, Cat) of
		{error, enoent} = Error -> Error;
		{Service, ParamsMap} -> 
			Querystring = processa_querystring(Service, Request),
			{Service, ParamsMap, Querystring}
	end.


do_lookup_re(_Request, []) ->
	{error, enoent};
do_lookup_re(Request = #request{type = Type, url = Url}, [H|T]) ->
	try
		RE = H#service.id_re_compiled,
		PatternKey = ems_util:make_rowid_from_url(Url, Type),
		case re:run(PatternKey, RE, [{capture,all_names,binary}]) of
			match -> {H, #{}};
			{match, Params} -> 
				{namelist, ParamNames} = re:inspect(RE, namelist),
				ParamsMap = maps:from_list(lists:zip(ParamNames, Params)),
				{H, ParamsMap};
			nomatch -> do_lookup_re(Request, T);
			{error, _ErrType} -> {error, enoent}
		end
	catch 
		_Exception:_Reason -> {error, enoent}
	end.


get_metadata_json(#service{id = Id,
						  name = Name,
						  content_type = ContentType,
						  type = Method,
						  url = Url,
						  service = Service,
						  comment = Comment,
						  version = Version,
						  owner = Owner,
						  result_cache = ResultCache,
						  authorization = Authorization,
						  timeout = Timeout,
						  path = Path,
						  lang = Lang,
						  querystring = Querystring}) ->
	iolist_to_binary([<<"{"/utf8>>,
					   <<"\"id\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Id, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"name\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Name, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"content_type\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, ContentType, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"method\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Method, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"service\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Service, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"url\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Url, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"comment\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Comment, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"version\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Version, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"owner\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Owner, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"result_cache\""/utf8>>, <<":"/utf8>>, integer_to_binary(ResultCache), <<","/utf8>>,
					   <<"\"timeout\""/utf8>>, <<":"/utf8>>, integer_to_binary(Timeout), <<","/utf8>>,
					   <<"\"path\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, case Path of
																			undefined -> <<>>;
																			_ -> Path
																		 end, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"lang\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Lang, <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"authorization\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, erlang:atom_to_binary(Authorization, utf8), <<"\""/utf8>>, <<","/utf8>>,
					   <<"\"querystring\""/utf8>>, <<":"/utf8>>, ems_util:json_encode(Querystring),
				   <<"}"/utf8>>]).
