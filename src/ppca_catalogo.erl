%%********************************************************************
%% @title Módulo catálogo de serviços
%% @version 1.0.0
%% @doc Módulo responsável pelo gerenciamento do catálogo de serviços
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(ppca_catalogo).

-behavior(gen_server). 

-include("../include/ppca_config.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client
-export([lista_catalogo/2, 
		 update_catalogo/0,
		 lookup/2, 
		 get_querystring/2, 
		 get_property_servico/2, 
		 get_property_servico/3, 
		 test/0, 
		 list_cat2/0, list_cat3/0, new_id_servico/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
%% Cat1 = JSON catalog, Cat2 = parsed catalog, Cat3 = regular expression parsed catalog
-record(state, {cat1, cat2, cat3}). 


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
lista_catalogo(Request, From) ->
	gen_server:cast(?SERVER, {lista_catalogo, Request, From}).

update_catalogo() ->
	gen_server:cast(?SERVER, update_catalogo).
	
lookup(Url, Type) ->	
	gen_server:call(?SERVER, {lookup, Url, Type}).

list_cat2() ->
	gen_server:call(?SERVER, list_cat2).

list_cat3() ->
	gen_server:call(?SERVER, list_cat3).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	%% Cat1 = JSON catalog, Cat2 = parsed catalog, Cat3 = regular expression parsed catalog
	NewState = get_catalogo(),
    {ok, NewState}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({lista_catalogo, _Request, From}, State) ->
	Cat = do_lista_catalogo(State),
	From ! {ok, Cat}, 
	{noreply, State};

handle_cast(update_catalogo, _State) ->
	NewState = get_catalogo(),
	{noreply, NewState}.
    
handle_call({lookup, Url, Type}, _From, State) ->
	Reply = do_lookup(Url, Type, State),
	{reply, Reply, State};

handle_call(list_cat2, _From, State) ->
	{reply, State#state.cat2, State};

handle_call(list_cat3, _From, State) ->
	{reply, State#state.cat3, State}.

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

do_lookup(Url, Type, State) ->
	Id = new_id_servico(Url, Type),
	case lookup_by_id(Id, State#state.cat2) of
		{ok, Servico} -> {ok, Servico};
		notfound -> lookup_re(Url, Type, State#state.cat3)
	end.

%% @doc Obtém o catálogo
get_catalogo() -> 
	Cat1 = get_catalogo_from_disk(),
	{Cat2, Cat3} = parse_catalogo(Cat1, [], []),
	#state{cat1=Cat1, cat2=Cat2, cat3=Cat3}.

%% @doc Lê o catálogo do disco
get_catalogo_from_disk() ->
	{ok, Cat} = file:read_file(?CATALOGO_PATH),
	{ok, Cat2} = ppca_util:json_decode_as_map(Cat),
	Cat2.

parse_catalogo([], Cat2, Cat3) ->
	{maps:from_list(Cat2), Cat3};

parse_catalogo([H|T], Cat2, Cat3) ->
	Url = get_property_servico(<<"url">>, H),
	{Module, Function} = get_property_servico(<<"service">>, H),
	Use_re = get_property_servico(<<"use_re">>, H, false),
	Type = get_property_servico(<<"type">>, H, <<"GET">>),
	Id = new_id_servico(Url, Type),
	case Use_re of
		true -> 
			Servico = new_servico_re(Id, Url, Module, Function, Type),
			parse_catalogo(T, Cat2, [Servico|Cat3]);
		false -> 
			Servico = new_servico(Id, Url, Module, Function, Type),
			parse_catalogo(T, [{Id, Servico}|Cat2], Cat3)
	end.	

get_property_servico(<<"service">>, Servico) ->
	Service = binary_to_list(maps:get(<<"service">>, Servico)),
	[NomeModule, NomeFunction] = string:tokens(Service, ":"),
	Module = list_to_atom(NomeModule),
	Function = list_to_atom(NomeFunction),
	{Module, Function};

get_property_servico(Key, Servico) ->
	Result = get_property_servico(Key, Servico, null),
	Result.
	
get_property_servico(Key, Servico, Default) ->
	V1 = maps:get(Key, Servico, Default),
	case is_binary(V1) of
		true ->	V2 = binary_to_list(V1);
		false -> V2 = V1
	end,
	case V2 of
		"true" -> V3 = true;
		"false" -> V3 = false;
		_ -> V3 = V2
	end,
	V3.

get_querystring(Cat, <<QueryName/binary>>) ->	
	[Query] = [Q || Q <- get_property_servico(<<"querystring">>, Cat), get_property_servico(<<"comment">>, Q) == QueryName],
	Query.
	
lookup_by_id(Id, Cat) ->
	case maps:find(Id, Cat) of
		{ok, Servico} -> {ok, Servico};
		error -> notfound
	end.

lookup_re(_Url, _Type, []) ->
	notfound;

lookup_re(Url, Type, [H|T]) ->
	RE = maps:get(<<"id_re_compiled">>, H),
	Id = new_id_servico(Url, Type),
	IdMap = maps:get(<<"id">>, H),
	io:format("este eh o id: ~p e este eh do map: ~p", [Id, IdMap]),
	case re:run(Id, RE, [{capture,all_names,binary}]) of
		match -> {ok, H, []};
		{match, Params} -> 
			{namelist, ParamNames} = re:inspect(RE, namelist),
			ParamsMap = maps:from_list(lists:zip(ParamNames, Params)),
			{ok, H, ParamsMap};
		nomatch -> lookup_re(Url, Type, T);
		{error, _ErrType} -> nofound 
	end.

new_id_servico(Url, Type) ->	
	[PrefixUrl|Url2] = Url,
	case PrefixUrl of
		$^ -> iolist_to_binary([Type, <<"#">>, Url2]);
		_  -> iolist_to_binary([Type, <<"#">>, Url])
	end.

new_servico_re(Id, Url, Module, Function, Type) ->
	{ok, Id_re_compiled} = re:compile(Id),
	Servico = #{<<"id">> => Id,
				<<"url">> => Url,
				<<"type">> => Type,
			    <<"module">> => Module,
			    <<"function">> => Function,
			    <<"use_re">> => true,
			    <<"id_re_compiled">> => Id_re_compiled},
	Servico.

new_servico(Id, Url, Module, Function, Type) ->
	Servico = #{<<"id">> => Id,
				<<"url">> => Url,
				<<"type">> => Type,
			    <<"module">> => Module,
			    <<"function">> => Function,
			    <<"use_re">> => false},
	Servico.

test() ->
	%%  {T1, T2, R1, R2, R3} = rota_table:test().

	Id1 = new_id_servico("^/aluno/lista_formandos/(?P<tipo>(sintetico|analitico))$", <<"GET">>),
	Id2 = new_id_servico("^/portal/[a-zA-Z0-9-_\.]+\.(html|js|css)$", <<"GET">>),
	Id4 = new_id_servico("^/portal/", <<"GET">>),
	
	R1 = new_servico_re(Id1, "^/aluno/lista_formandos/(?P<tipo>(sintetico|analitico))$", aluno_service_report, function, <<"GET">>),
	R2 = new_servico_re(Id2, "^/portal/[a-zA-Z0-9-_\.]+\.(html|js|css)$", ppca_static_file, function, <<"GET">>),
	R4 = new_servico_re(Id4, "^/portal/", aluno_service_report, function, <<"GET">>),

	Id3 = new_id_servico("/log/server.log", <<"GET">>),
	R3 = new_servico(Id3, "/log/server.log", ppca_static_file, function, <<"GET">>),
	
	
	T1 = [R1, R2, R4],
	T2 = maps:from_list([{"/log/server.log", R3}]),
	
	io:format("\n\n"),
	
	lookup_re("/aluno/lista_formandos/tipo", <<"GET">>, T1),
	lookup_re("/portal/index.html", <<"GET">>, T1),
	lookup_by_id(<<"GET#logs/server.log">>, T2),

	{T1, T2, R1, R2, R3}.



