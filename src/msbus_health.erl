%%********************************************************************
%% @title Módulo msbus_health
%% @version 1.0.0
%% @doc Disponibiliza informações da saúde do servidor
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(msbus_health).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-compile(export_all).

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Client API
-export([get_top_services/3, 
		 get_top_services/4, 
		 get_top_services_by_type/3, 
		 get_qtd_requests_by_date/3,
		 groupBy/2, 
		 get_requests_submit/1, 
		 count/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {}).

-define(SERVER, ?MODULE).

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

%% @doc Obtém os serviços mais usados
get_top_services(Top, Periodo, Sort, From) -> 
	poolboy:transaction(msbus_health_pool, fun(Worker) ->
		gen_server:cast(Worker, {top_services, Top, Periodo, Sort, From})
    end).

get_top_services(Top, Periodo, Sort) -> 
	poolboy:transaction(msbus_health_pool, fun(Worker) ->
		gen_server:call(Worker, {top_services, Top, Periodo, Sort})
    end).

get_top_services_by_type(Top, Periodo, Sort) -> 
	poolboy:transaction(msbus_health_pool, fun(Worker) ->
		gen_server:call(Worker, {top_services_by_type, Top, Periodo, Sort})
    end).

get_qtd_requests_by_date(Top, Periodo, Sort) -> 
	poolboy:transaction(msbus_health_pool, fun(Worker) ->
		gen_server:call(Worker, {qtd_requets_by_date, Top, Periodo, Sort})
    end).
	

%% @doc Lista os requests por período
get_requests_submit(Periodo) ->	
	poolboy:transaction(msbus_health_pool, fun(Worker) ->
		gen_server:call(Worker, {requests_submit, Periodo})
    end).

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    create_cache_req_sub(),
    {ok, #state{}}.
    
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({top_services, Top, Periodo, Sort, From}, State) ->
	Result = do_get_top_services(Top, Periodo, Sort, State),
	From ! Result,
	{noreply, State}.

handle_call({top_services, Top, Periodo, Sort}, _From, State) ->
	Reply = do_get_top_services(Top, Periodo, Sort, State),
	{reply, Reply, State};

handle_call({top_services_by_type, Top, Periodo, Sort}, _From, State) ->
	Reply = do_get_top_services_by_type(Top, Periodo, Sort, State),
	{reply, Reply, State};

handle_call({qtd_requets_by_date, Top, Periodo, Sort}, _From, State) ->
	Reply = do_get_qtd_requests_by_date(Top, Periodo, Sort, State),
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
%% Internal functions
%%====================================================================

%% @doc Retorna a lista de requisições de um período
get_requests_submit(Periodo, _State) ->
	msbus_cache:get(health_req_sub, 60000, Periodo, 
					fun() -> 
						msbus_request:get_requests_periodo(Periodo)
					end).

%% @doc Retorna a lista de requisições de um período agrupado por FieldsGroup
get_requests_submit_by_group(Periodo, FieldsGroup, State) ->
	Requests = get_requests_submit(Periodo, State),
	maps:keys(groupBy(FieldsGroup, Requests)).

%% @doc Retorna os serviços mais acessados de um período
do_get_top_services(Top, Periodo, Sort, State) ->
    Fields = fun(X) -> {X#request.servico#servico.name} end,
	Requests = get_requests_submit(Periodo, State), 
	Urls = maps:keys(groupBy(Fields, Requests)),
	Urls2 = count(Fields, Urls, Requests),
	case Sort of
		"url" -> Urls3 = sort_first(Urls2);
		"qtd" -> Urls3 = sort_last(Urls2)
	end,
	top(Urls3, Top).
	
%% @doc Retorna os serviços mais acessados por tipo de verbo de um período
do_get_top_services_by_type(Top, Periodo, Sort, State) ->
    Fields = fun(X) -> {X#request.servico#servico.type, 
			  			X#request.servico#servico.name} end,
	Requests = get_requests_submit(Periodo, State), 
	Urls = maps:keys(groupBy(Fields, Requests)),
	Urls2 = count(Fields, Urls, Requests),
	case Sort of
		"url" -> Urls3 = sort_first(Urls2);
		"qtd" -> Urls3 = sort_last(Urls2)
	end,
	top(Urls3, Top).

%% @doc Retorna a quantidade de requisições por data de um período
do_get_qtd_requests_by_date(Top, Periodo, Sort, State) ->
    Fields = fun(X) -> {msbus_util:date_to_string(X#request.timestamp)} end,
	Requests = get_requests_submit(Periodo, State),
	Requests1 = maps:keys(groupBy(Fields, Requests)),
	Requests2 = count(Fields, Requests1, Requests),
	case Sort of
		"date" -> Requests3 = sort_first(Requests2);
		"qtd"  -> Requests3 = sort_last(Requests2)
	end,
	top(Requests3, Top).

	
groupBy(F, L) -> lists:foldr(fun({K,V}, D) -> maps:put(K, V, D) end , maps:new(), [ {F(X), X} || X <- L ]).
	
count(F, G, L) -> 
	CountFunc = fun(Fn, X) -> length([V || V <- L, Fn(V) == X]) end,
	[ erlang:tuple_to_list(X) ++ [CountFunc(F, X)] || X <- G].
	
sort_last(L) -> lists:sort(fun(X, Y) -> lists:last(X) >= lists:last(Y) end, L).

sort_first(L) -> lists:sort(fun(X, Y) -> hd(X) >= hd(Y) end, L).
	
top(L, T) -> 
	lists:sublist(L, T).

select() ->
	Query = fun() ->
		  qlc:e(
			 qlc:q([X || X <- mnesia:table(request)])
		  )
	   end,
	{atomic, Requests} = mnesia:transaction(Query),
	{ok, Requests}.

create_cache_req_sub() ->
	try
		msbus_cache:new(health_req_sub)
	catch
		_Exception:_Reason ->  ok
	end.

