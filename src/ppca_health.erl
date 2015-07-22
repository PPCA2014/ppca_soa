%%********************************************************************
%% @title Módulo ppca_health
%% @version 1.0.0
%% @doc Disponibiliza informações da saúde do servidor
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(ppca_health).

-behavior(gen_server). 

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([collect/3, list_metrics/0, get_top_services/2, get_top_services/3, groupBy/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

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
collect(RID, Metric, Data) -> 
	gen_server:cast(?SERVER, {RID, Metric, Data}).

%% @doc Obtém os serviços mais usados
get_top_services(Top, Periodo, From) -> 
	gen_server:cast(?SERVER, {top_services, Top, Periodo, From}).

get_top_services(Top, Periodo) -> 
	gen_server:call(?SERVER, {top_services, Top, Periodo}).

%% @doc Lista todas as métricas coletadas
list_metrics() ->	
	gen_server:call(?SERVER, list_metrics).
 
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

handle_cast({RID, Metric, Data}, State) ->
	NewState = do_collect(RID, Metric, Data, State),
	{noreply, NewState}.

handle_call({top_services, Top, Periodo}, _From, State) ->
	Result = do_get_top_services(Top, Periodo, State),
	{reply, Result, State};
    
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

do_collect(RID, Metric, Data, State) ->
	MetricData = {RID, Metric, calendar:local_time(), Data},
	#state{list = [MetricData | State#state.list]}.

%% @doc Retorna os requets_submit pelo período. Default é "year".
get_requests_submit(Periodo, State) ->
	case Periodo of
		"hour"  -> Requests = [X || {_, request_submit, DT, _} = X <- State#state.list, ppca_util:in_last_hour(DT)]; 
		"day"   -> Requests = [X || {_, request_submit, DT, _} = X <- State#state.list, ppca_util:in_last_day(DT)];
		"week"  -> Requests = [X || {_, request_submit, DT, _} = X <- State#state.list, ppca_util:in_last_week(DT)];
		"month" -> Requests = [X || {_, request_submit, DT, _} = X <- State#state.list, ppca_util:in_last_month(DT)];
		_       -> Requests = [X || {_, request_submit, DT, _} = X <- State#state.list, ppca_util:in_last_year(DT)]
	end,
	Requests.
	
%% @doc Retorna os tops services por período
do_get_top_services(Top, Periodo, State) ->
	Requests = get_requests_submit(Periodo, State), 
	ListaUrls = [element(1, element(4, X)) || X <- Requests],
	Urls = maps:keys( ppca_health:groupBy( fun(X) -> X end, ListaUrls  ) ),
	CountFunc = fun(Url) -> length([X || X <- ListaUrls, X == Url]) end,
	Url_QtdReq = [ [X, CountFunc(X)] || X <- Urls],
	Url_QtdReq_Sorted = lists:sort(fun(X, Y) -> lists:last(X) >= lists:last(Y) end, Url_QtdReq),
	Top_Url_QtdReq = lists:sublist(Url_QtdReq_Sorted, Top),
	Top_Url_QtdReq.
	

	
groupBy(F, L) -> lists:foldr(fun({K,V}, D) -> maps:put(K, V, D) end , maps:new(), [ {F(X), X} || X <- L ]).
	
	
	
	
	
	
