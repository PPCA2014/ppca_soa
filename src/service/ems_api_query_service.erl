%%********************************************************************
%% @title Module ems_api_query_service
%% @version 1.0.0
%% @doc It provides dynamic_view service for relational databases.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-compile(export_all).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").


%% Server API
-export([start/0, start_link/1, stop/0]).

%% Client API
-export([find/2, find_by_id/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Stores the state of the service.
-record(state, {}).  


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
 
find(Request, From) ->
	ems_pool:cast(ems_api_query_service_pool, {find, Request, From}).

find_by_id(Request, From) ->
	ems_pool:cast(ems_api_query_service_pool, {find_by_id, Request, From}).

insert(Request, From) ->
	ems_pool:cast(ems_api_query_service_pool, {insert, Request, From}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    State = #state{},
    {ok, State}. 
    
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({find, Request, _From}, State) ->
	Result = execute_command(find, Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Result}),
	{noreply, State};

handle_cast({find_by_id, Request, _From}, State) ->
	Result = execute_command(find_by_id, Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Result}),
	{noreply, State};

handle_cast({insert, Request, _From}, State) ->
	Result = execute_command(insert, Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Result}),
	{noreply, State}.

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

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



execute_command(Command, Request = #request{service = #service{datasource = Datasource}}, State) ->
	try
		case ems_db:get_connection(Datasource) of
			{ok, Datasource2} -> 
				Result = case Command of
					find -> do_find(Request, Datasource2, State);
					find_by_id -> do_find_by_id(Request, Datasource2, State);
					insert -> do_insert(Request, Datasource2, State)
				end,
				ems_db:release_connection(Datasource2),
				Result;
			{error, Reason} ->	{error, Reason}
		end
	catch
		_Exception:Reason2 -> {error, Reason2}
	end.


do_find(#request{querystring_map = QuerystringMap, 
				 service = #service{debug = Debug}},
				 Datasource, _State) ->
	FilterJson = maps:get(<<"filter">>, QuerystringMap, <<>>),
	Fields = binary_to_list(maps:get(<<"fields">>, QuerystringMap, <<>>)),
	Limit = binary_to_integer(maps:get(<<"limit">>, QuerystringMap, <<"100">>)),
	Offset = binary_to_integer(maps:get(<<"offset">>, QuerystringMap, <<"1">>)),
	Sort = binary_to_list(maps:get(<<"sort">>, QuerystringMap, <<>>)),
	ems_api_query:find(FilterJson, Fields, Limit, Offset, Sort, Datasource, Debug).


do_find_by_id(Request = #request{querystring_map = QuerystringMap, 
								 service = #service{debug = Debug}}, 
			 Datasource, _State) ->
	Id = ems_request:get_param_url(<<"id">>, 0, Request),
	Fields = binary_to_list(maps:get(<<"fields">>, QuerystringMap, <<>>)),
	ems_api_query:find_by_id(Id, Fields, Datasource, Debug).


do_insert(#request{payload_map = Payload, service = Service}, Datasource, _State) ->
	ems_api_query:insert(Payload, Service, Datasource).


