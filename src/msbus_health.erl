%%********************************************************************
%% @title Módulo msbus_health
%% @version 1.0.0
%% @doc Disponibiliza informações da saúde do servidor
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_health).

-behavior(gen_server). 

-compile(export_all).

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([collect/2, list_metrics/0, get_top_services/2, get_top_services/3, groupBy/2, get_requests_submit/1, count/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-include("../include/msbus_config.hrl").

% estado do servidor
-record(state, {list=[]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================

%% @doc Coleta uma métrica importante
collect(Metric, Request) -> 
	gen_server:cast(?SERVER, {Metric, Request}).

%% @doc Obtém os serviços mais usados
get_top_services(Top, Periodo, From) -> 
	gen_server:cast(?SERVER, {top_services, Top, Periodo, From}).

get_top_services(Top, Periodo) -> 
	gen_server:call(?SERVER, {top_services, Top, Periodo}).

%% @doc Lista todas as métricas coletadas
list_metrics() ->	
	gen_server:call(?SERVER, list_metrics).

%% @doc Lista todas as métricas coletadas
get_requests_submit(Periodo) ->	
	gen_server:call(?SERVER, {requests_submit, Periodo}).
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({top_services, Top, Periodo, From}, State) ->
	Result = do_get_top_services(Top, Periodo, State),
	From ! Result,
	{noreply, State};

handle_cast({Metric, Request}, State) ->
	NewState = do_collect(Metric, Request, State),
	{noreply, NewState}.

handle_call({top_services, Top, Periodo}, _From, State) ->
	Reply = do_get_top_services(Top, Periodo, State),
	{reply, Reply, State};

handle_call({requests_submit, Periodo}, _From, State) ->
	Reply = get_requests_submit(Periodo, State),
	{reply, Reply, State};
    
handle_call(list_metrics, _From, State) ->
	{reply, State#state.list, State}.

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

do_collect(Metric, Request, State) ->
	#state{list = [{Metric, Request} | State#state.list]}.

%% @doc Retorna os requets_submit pelo período. Default é "year".
get_requests_submit(Periodo, State) ->
	case Periodo of
		"hour"  ->  [X || {_, Request} = X <- State#state.list, msbus_util:in_last_hour(Request#request.timestamp)];
		"day"   ->  [X || {_, Request} = X <- State#state.list, msbus_util:in_last_day(Request#request.timestamp)];
		"week"  ->  [X || {_, Request} = X <- State#state.list, msbus_util:in_last_week(Request#request.timestamp)];
		"month" ->  [X || {_, Request} = X <- State#state.list, msbus_util:in_last_month(Request#request.timestamp)];
		_       ->  [X || {_, Request} = X <- State#state.list, msbus_util:in_last_year(Request#request.timestamp)]
	end.

%% @doc Retorna os tops services por período
do_get_top_services(Top, Periodo, State) ->
    Fields = fun({_, X}) -> {maps:get(<<"type">>, X#request.servico), 
			  			     maps:get(<<"url">>, X#request.servico)} end,
	Requests = get_requests_submit(Periodo, State), 
	Urls = maps:keys(groupBy(Fields, Requests)),
	Urls2 = count(Fields, Urls, Requests),
	top(Urls2, Top).
	

	
groupBy(F, L) -> lists:foldr(fun({K,V}, D) -> maps:put(K, V, D) end , maps:new(), [ {F(X), X} || X <- L ]).
	
count(F, G, L) -> 
	CountFunc = fun(Fn, X) -> length([V || V <- L, Fn(V) == X]) end,
	[ erlang:tuple_to_list(X) ++ [CountFunc(F, X)] || X <- G].
	
sort(L) -> lists:sort(fun(X, Y) -> lists:last(X) >= lists:last(Y) end, L).
	
top(L, T) ->
	L1 = sort(L),
	lists:sublist(L1, T).
	
