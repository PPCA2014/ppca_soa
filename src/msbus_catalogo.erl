%%********************************************************************
%% @title Módulo catálogo de serviços
%% @version 1.0.0
%% @doc Módulo responsável pelo gerenciamento do catálogo de serviços
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_catalogo).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client
-export([lookup/1, 
		 lista_catalogo/2, 
		 get_querystring/2, 
		 get_property_servico/2, 
		 get_property_servico/3, 
		 test/0]).


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
	
lookup(Url) ->	
	gen_server:call(?SERVER, {lookup, Url}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	%% Cat1 = JSON catalog, Cat2 = parsed catalog, Cat3 = regular expression parsed catalog
	{Cat1, Cat2, Cat3} = get_catalogo(),
	NewState = #state{cat1=Cat1, cat2=Cat2, cat3=Cat3},
    {ok, NewState}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({lista_catalogo, _Request, From}, State) ->
	{Result, NewState} = do_lista_catalogo(State),
	From ! {ok, Result}, 
	{noreply, NewState}.
    
handle_call({lookup, Url}, _From, State) ->
	Reply = do_lookup(Url, State),
	{reply, Reply, State}.

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
do_lista_catalogo(State) ->
	Cat = State#state.cat1,
	{Cat, State}.

do_lookup(Url, State) ->
	case lookup(Url, State#state.cat2) of
		{ok, Servico} -> {ok, Servico};
		notfound -> lookup_re(Url, State#state.cat3)
	end.

%% @doc Lê o catálogo
get_catalogo() -> 
	Cat1 = get_catalogo_from_disk(),
	{Cat2, Cat3} = parse_catalogo(Cat1, [], []),
	{Cat1, Cat2, Cat3}.

%% @doc Lê o catálogo do disco
get_catalogo_from_disk() ->
	{ok, Cat} = file:read_file(?CATALOGO_PATH),
	{ok, Cat2} = msbus_util:json_decode_as_map(Cat),
	Cat2.

parse_catalogo([], Cat2, Cat3) ->
	{maps:from_list(Cat2), Cat3};

parse_catalogo([H|T], Cat2, Cat3) ->
	Url = get_property_servico(<<"url">>, H),
	{Module, Function} = get_property_servico(<<"service">>, H),
	Use_re = get_property_servico(<<"use_re">>, H, false),
	case Use_re of
		true -> 
			Servico = new_servico_re(Url, Module, Function),
			parse_catalogo(T, Cat2, [Servico|Cat3]);
		false -> 
			Servico = new_servico(Url, Module, Function),
			parse_catalogo(T, [{Url, Servico}|Cat2], Cat3)
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
	
lookup(Url, Cat) ->
	case maps:find(Url, Cat) of
		{ok, Servico} -> {ok, Servico};
		error -> notfound
	end.

lookup_re(_Url, []) ->
	notfound;

lookup_re(Url, [H|T]) ->
	RE = maps:get(<<"url_re_compiled">>, H),
	case re:run(Url, RE, [{capture,all_names,binary}]) of
		match -> {ok, H, []};
		{match, Params} -> 
			{namelist, ParamNames} = re:inspect(RE, namelist),
			ParamsMap = lists:zip(ParamNames, Params),
			{ok, H, ParamsMap};
		nomatch -> lookup_re(Url, T);
		{error, _ErrType} -> nofound
	end.

new_servico_re(Url, Module, Function) ->
	{ok, Url_re_compiled} = re:compile(Url),
	Servico = #{<<"url">> => Url,
			    <<"module">> => Module,
			    <<"function">> => Function,
			    <<"use_re">> => true,
			    <<"url_re_compiled">> => Url_re_compiled},
	Servico.

new_servico(Url, Module, Function) ->
	Servico = #{<<"url">> => Url,
			    <<"module">> => Module,
			    <<"function">> => Function,
			    <<"use_re">> => false},
	Servico.


test() ->
	%%  {T1, T2, R1, R2, R3} = rota_table:test().

	R1 = new_servico_re("^/aluno/lista_formandos/(?P<tipo>(sintetico|analitico))$", aluno_service_report, function),
	R2 = new_servico_re("^/portal/[a-zA-Z0-9-_\.]+\.(html|js|css)$", msbus_static_file, function),
	R4 = new_servico_re("^/portal/", aluno_service_report, function),
	
	R3 = new_servico("/log/server.log", msbus_static_file, function),
	
	
	T1 = [R1, R2, R4],
	T2 = maps:from_list([{"/log/server.log", R3}]),
	
	io:format("\n\n"),
	
	lookup_re("/aluno/lista_formandos/tipo", T1),
	lookup_re("/portal/index.html", T1),
	lookup("/logs/server.log", T2),

	{T1, T2, R1, R2, R3}.



