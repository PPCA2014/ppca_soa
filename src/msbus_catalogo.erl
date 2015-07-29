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
-export([lista_catalogo/0, 
		 update_catalogo/0,
		 lookup/2, 
		 get_querystring/2, 
		 get_property_servico/2, 
		 get_property_servico/3, 
		 list_cat2/0, list_cat3/0]).


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
 
lista_catalogo() ->
	gen_server:call(?SERVER, lista_catalogo).

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

handle_cast(update_catalogo, _State) ->
	NewState = get_catalogo(),
	{noreply, NewState}.

handle_call(lista_catalogo, _From, State) ->
	Reply = do_lista_catalogo(State),
	{reply, Reply, State};
    
handle_call({lookup, Url, Type}, _From, State) ->
	Reply = lookup(Url, Type, State),
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

%% @doc Obtém o catálogo
get_catalogo() -> 
	Cat1 = get_catalogo_from_disk(),
	{Cat2, Cat3, Cat4} = parse_catalogo(Cat1, [], [], [], 1),
	#state{cat1=Cat4, cat2=Cat2, cat3=Cat3}.

%% @doc Lê o catálogo do disco
get_catalogo_from_disk() ->
	{ok, Cat} = file:read_file(?CATALOGO_PATH),
	{ok, Cat2} = msbus_util:json_decode_as_map(Cat),
	Cat2.

parse_catalogo([], Cat2, Cat3, Cat4, _Id) ->
	{maps:from_list(Cat2), Cat3, Cat4};

parse_catalogo([H|T], Cat2, Cat3, Cat4, Id) ->
	Name = get_property_servico(<<"name">>, H),
	Url = get_property_servico(<<"url">>, H),
	{Module, Function} = get_property_servico(<<"service">>, H),
	Use_re = get_property_servico(<<"use_re">>, H, <<"false">>),
	Type = get_property_servico(<<"type">>, H, <<"GET">>),
	Apikey = get_property_servico(<<"APIkey">>, H, <<"false">>),
	Comment = get_property_servico(<<"comment">>, H, <<>>),
	Version = get_property_servico(<<"comment">>, H, <<>>),
	Owner = get_property_servico(<<"owner">>, H, <<>>),
	Async = get_property_servico(<<"async">>, H, <<"false">>),
	Rowid = new_rowid_servico(Url, Type),
	IdBin = list_to_binary(integer_to_list(Id)),
	ServicoView = new_servico_view(Rowid, IdBin, Name, Url, Module,
								   Function, Type, Apikey, Comment, Version, Owner, Async),
	case Use_re of
		<<"true">> -> 
			Servico = new_servico_re(Rowid, IdBin, Name, Url, Module, 
									 Function, Type, Apikey, Comment, Version, Owner, Async),
			parse_catalogo(T, Cat2, [Servico|Cat3], [ServicoView|Cat4], Id+1);
		<<"false">> -> 
			Servico = new_servico(Rowid, IdBin, Name, Url, Module,
								  Function, Type, Apikey, Comment, Version, Owner, Async),
			parse_catalogo(T, [{Rowid, Servico}|Cat2], Cat3, [ServicoView|Cat4], Id+1)
	end.	

get_property_servico(<<"service">>, Servico) ->
	Service = maps:get(<<"service">>, Servico),
	[Module, Function] = binary:split(Service, <<":">>),
	{Module, Function};

get_property_servico(Key, Servico) ->
	maps:get(Key, Servico, <<>>).
	
get_property_servico(Key, Servico, Default) ->
	maps:get(Key, Servico, Default).

get_querystring(Cat, <<QueryName/binary>>) ->	
	[Query] = [Q || Q <- get_property_servico(<<"querystring">>, Cat), get_property_servico(<<"comment">>, Q) == QueryName],
	Query.
	
lookup(Url, Type, State) ->
	Rowid = new_rowid_servico(Url, Type),
	case maps:find(Rowid, State#state.cat2) of
		{ok, Servico} -> {ok, Servico, []};
		error -> lookup_re(Url, Type, State#state.cat3)
	end.

lookup_re(_Url, _Type, []) ->
	notfound;

lookup_re(Url, Type, [H|T]) ->
	RE = maps:get(<<"id_re_compiled">>, H),
	Rowid = new_rowid_servico(Url, Type),
	case re:run(Rowid, RE, [{capture,all_names,binary}]) of
		match -> {ok, H, []};
		{match, Params} -> 
			{namelist, ParamNames} = re:inspect(RE, namelist),
			ParamsMap = maps:from_list(lists:zip(ParamNames, Params)),
			{ok, H, ParamsMap};
		nomatch -> lookup_re(Url, Type, T);
		{error, _ErrType} -> nofound 
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
	
new_servico_re(Rowid, Id, Name, Url, Module, Function, Type, Apikey, Comment, Version, Owner, Async) ->
	{ok, Id_re_compiled} = re:compile(Rowid),
	Servico = #{<<"rowid">> => Rowid,
				<<"id">> => Id,
				<<"name">> => Name,
				<<"url">> => Url,
				<<"type">> => Type,
			    <<"module">> => Module,
			    <<"function">> => Function,
			    <<"use_re">> => <<"true">>,
			    <<"id_re_compiled">> => Id_re_compiled,
			    <<"apikey">> => Apikey,
			    <<"comment">> => Comment,
			    <<"version">> => Version,
			    <<"owner">> => Owner,
			    <<"async">> => Async},
	Servico.

new_servico(Rowid, Id, Name, Url, Module, Function, Type, Apikey, Comment, Version, Owner, Async) ->
	Servico = #{<<"rowid">> => Rowid,
				<<"id">> => Id,
				<<"name">> => Name,
				<<"url">> => Url,
				<<"type">> => Type,
			    <<"module">> => Module,
			    <<"function">> => Function,
			    <<"use_re">> => <<"false">>,
			    <<"apikey">> => Apikey,
			    <<"comment">> => Comment,
			    <<"version">> => Version,
			    <<"owner">> => Owner,
			    <<"async">> => Async},
	Servico.

new_servico_view(Rowid, Id, Name, Url, Module, Function, Type, Apikey, Comment, Version, Owner, Async) ->
	Servico = #{<<"rowid">> => Rowid,
				<<"id">> => Id,
				<<"name">> => Name,
				<<"url">> => Url,
				<<"type">> => Type,
			    <<"module">> => Module,
			    <<"function">> => Function,
			    <<"apikey">> => Apikey,
			    <<"comment">> => Comment,
			    <<"version">> => Version,
			    <<"owner">> => Owner,
			    <<"async">> => Async},
	Servico.


