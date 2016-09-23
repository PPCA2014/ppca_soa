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
-export([start/1, start_link/1, stop/0]).

%% Client
-export([lookup/1,
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

lookup(Request) ->	
	gen_server:call(?SERVER, {lookup, Request}).

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
						true -> enoent;
						false -> valida_querystring(QuerystringServico, QuerystringUser)
					end
			end
	end.

valida_querystring(QuerystringServico, QuerystringUser) ->
	case valida_querystring(QuerystringServico, QuerystringUser, []) of
		{ok, Querystring} -> Querystring;
		enoent -> enoent
	end.

valida_querystring([], _QuerystringUser, enoent) -> enoent;

valida_querystring([], _QuerystringUser, QuerystringList) ->
	{ok, maps:from_list(QuerystringList)};

valida_querystring([H|T], QuerystringUser, QuerystringList) ->
	%% Verifica se encontra a query na querystring do usuário
	NomeQuery = maps:get(<<"name">>, H),
	case maps:find(NomeQuery, QuerystringUser) of
		{ok, Value} -> valida_querystring(T, QuerystringUser, [{NomeQuery, Value} | QuerystringList]);
		error ->
			%% se o usuário não informou a querystring, verifica se tem valor default na definição do serviço
			case maps:get(<<"default">>, H, enoent) of
				enoent -> enoent;
				Value -> valida_querystring(T, QuerystringUser, [{NomeQuery, Value} | QuerystringList])
			end
	end.


do_lookup(Method, Uri, State) ->
	case ems_http_util:encode_request(Method, Uri) of
		{ok, Request} -> do_lookup(Request, State);
		{error, Reason} -> {error, Reason}
	end.


do_lookup(Request, State) ->
	Rowid = Request#request.rowid,
	case ets:lookup(State#state.cat2, Rowid) of
		[] -> 
			case do_lookup_re(Request, State#state.cat3) of
				{Service, ParamsMap} -> 
					Querystring = processa_querystring(Service, Request),
					{Service, ParamsMap, Querystring};
				enoent -> enoent
			end;
		[{_Rowid, Service}] -> 
			case processa_querystring(Service, Request) of
			   enoent -> enoent;
			   Querystring -> {Service, Request#request.params_url, Querystring}
			end
	end.


do_lookup_re(_Request, []) ->
	enoent;

do_lookup_re(Request, [H|T]) ->
	RE = H#service.id_re_compiled,
	case re:run(Request#request.rowid, RE, [{capture,all_names,binary}]) of
		match -> {H, #{}};
		{match, Params} -> 
			{namelist, ParamNames} = re:inspect(RE, namelist),
			ParamsMap = maps:from_list(lists:zip(ParamNames, Params)),
			{H, ParamsMap};
		nomatch -> do_lookup_re(Request, T);
		{error, _ErrType} -> enoent 
	end.

