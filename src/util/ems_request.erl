%%********************************************************************
%% @title Módulo ems_request
%% @version 1.0.0
%% @doc Módulo repositório de requisições
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_request).
-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API
-export([start/1, start_link/1, stop/0]).

%% Client API
-export([registra_request/1, 
		 finaliza_request/1,
		 get_requests_periodo/1,
		 get_requests_periodo/2,
		 get_request_by_rid/1,
		 get_request_em_andamento/1]).

-export([get_property_request/2, 
		 get_param_url/3,
		 get_querystring/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {checkpoint_time_ref = undefined}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?SERVER, Args, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================

%% @doc Registra ou atualiza um request
registra_request(Request) -> 
	gen_server:cast(?SERVER, {registra_request, Request}).

finaliza_request(Request) -> 
	gen_server:cast(?SERVER, {finaliza_request, Request}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) -> 
	ets:new(tbl_request_cache, [set, 
								private, 
								named_table,
								{write_concurrency, true},
								{keypos, 2}]),
	ets:new(tbl_request_andamento, [set, 
									public, 
									named_table,
									{write_concurrency, true},
									{keypos, 2}]),
	%fprof:trace([start, {procs, [self()]}]),
	{ok, #state{}}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({registra_request, Request}, State) ->
	case State#state.checkpoint_time_ref of
		undefined -> ok;
		Time_ref -> erlang:cancel_timer(Time_ref, [])
	end,
	do_registra_request(Request),
	Time_ref2 = erlang:send_after(?REQ_CACHE_SYNC_CHECKPOINT, self(), checkpoint),
	{noreply, #state{checkpoint_time_ref = Time_ref2}};
	
handle_cast({finaliza_request, Request}, State) ->	
	do_finaliza_request(Request),
	{noreply, State}.

handle_call(Msg, _From, State) ->
   {reply, Msg, State}.

handle_info(checkpoint, State) ->
   do_sync_buffer(),
   {noreply, State};

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

do_registra_request(Request) ->
	ets:insert(tbl_request_cache, Request),
	ets:insert(tbl_request_andamento, Request).
	
do_finaliza_request(Request) ->
	ets:delete(tbl_request_cache, Request#request.rid),
	ets:delete(tbl_request_andamento, Request#request.rid).
		
do_sync_buffer() ->
	SyncBuffer = fun() ->  
						ets:foldl(fun(Request, DontCare) ->
									mnesia:write(Request),
									ets:delete(ems_request_cache, Request#request.rid),
									DontCare
								  end, notused, ems_request_cache)
				 end,
	mnesia:transaction(SyncBuffer).


%% @doc Retorna a lista de requisições de um período
get_requests_periodo(Periodo) ->
	Query = fun() ->
		  qlc:e(
			 qlc:sort(
				 qlc:q([R || R <- mnesia:table(request), 
							 ems_util:no_periodo(R#request.timestamp, Periodo)]), [{order, descending}]
							 
				)
		  )
	   end,
	{atomic, Requests} = mnesia:transaction(Query),
	Requests.

%% @doc Retorna a lista de requisições por período e por code
get_requests_periodo(Periodo, Code) ->
	Query = fun() ->
		  qlc:e(
			 qlc:sort(
				 qlc:q([R || R <- mnesia:table(request), 
							 ems_util:no_periodo(R#request.timestamp, Periodo),
							 R#request.code == Code
							 ]), [{order, descending}]
				)
		  )
	   end,
	{atomic, Requests} = mnesia:transaction(Query),
	Requests.

%% @doc Retorna uma requisição pelo seu id
get_request_by_rid(RID) -> 
	ems_db:get(request, RID).

%% @doc Retorna uma requisição pelo seu id
get_request_em_andamento(RID) -> 
	case ets:lookup(tbl_request_andamento, RID) of
		[] -> {error, enoent};
		[Request] -> {ok, Request}
	end.

%% @doc Retorna a URL do request
get_property_request(<<"url">>, Request) ->
	Request#request.url;

%% @doc Retorna o tipo do request
get_property_request(<<"metodo">>, Request) ->
	Request#request.type;

get_property_request(<<"type">>, Request) ->
	Request#request.type;

%% @doc Retorna a URL do request
get_property_request(<<"http_version">>, Request) ->
	Request#request.version;

%% @doc Retorna o payload/body do request
get_property_request(<<"payload">>, Request) ->
	Request#request.payload_map;

%% @doc Retorna o payload/body do request
get_property_request(<<"body">>, Request) ->
	Request#request.payload_map.

%% @doc Retorna um parâmetro do request
get_param_url(NomeParam, Default, Request) ->
	ParamsUrl = Request#request.params_url,
	NomeParam2 = iolist_to_binary(NomeParam),
	maps:get(NomeParam2, ParamsUrl, Default).

%% @doc Retorna uma querystring do request
get_querystring(QueryName, Default, #request{querystring_map = QuerystringMap}) ->
	Value = maps:get(QueryName, QuerystringMap, Default),
	case erlang:is_list(Value) of
		true -> list_to_binary(Value);
		false -> Value
	end.

