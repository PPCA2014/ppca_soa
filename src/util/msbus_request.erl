%%********************************************************************
%% @title Módulo msbus_request
%% @version 1.0.0
%% @doc Módulo que cuida do registro da requisição.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(msbus_request).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-compile(export_all).

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Client API
-export([registra_request/1, 
		 get_requests_periodo/1,
		 get_request_rid/1]).

-export([get_property_request/2, 
		 get_param_url/3,
		 get_querystring/3]).

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
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================

%% @doc Registra um request
registra_request(Request) -> 
	msbus_pool:call(msbus_request_pool, {registra_request, Request}).

%% @doc Atualiza o request
update_request(Request) -> 
	msbus_pool:cast(msbus_request_pool, {update_request, Request}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
   	msbus_eventmgr:adiciona_evento(new_request),
	msbus_eventmgr:adiciona_evento(ok_request),
	msbus_eventmgr:adiciona_evento(erro_request),
    {ok, #state{}}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.
	
handle_call({registra_request, Request}, _From, State) ->
	Request2 = do_registra_request(Request, State),
	{reply, Request2, State};

handle_call({update_request, Request}, _From, State) ->
	Request2 = do_update_request(Request, State),
	{reply, Request2, State}.

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

do_registra_request(Request, _State) ->
	msbus_db:insert(Request).

do_update_request(Request, _State) ->
	msbus_db:update(Request).

%% @doc Retorna a lista de requisições de um período
get_requests_periodo(Periodo) ->
	Query = fun() ->
		  qlc:e(
			 qlc:sort(
				 qlc:q([R || R <- mnesia:table(request), 
							 msbus_util:no_periodo(R#request.timestamp, Periodo)]), [{order, descending}]
							 
				)
		  )
	   end,
	{atomic, Requests} = mnesia:transaction(Query),
	Requests.

%% @doc Retorna uma requisição pelo seu id
get_request_rid(RID) -> msbus_db:get(request, RID).

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
	Request#request.versao_http;

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
	Value = maps:get(NomeParam2, ParamsUrl, Default),
	binary_to_list(Value).

%% @doc Retorna uma querystring do request
get_querystring(QueryName, Default, Request) ->
	Value = maps:get(QueryName, Request#request.querystring_map, Default),
	case erlang:is_binary(Value) of
		true -> binary_to_list(Value);
		false -> Value
	end.

