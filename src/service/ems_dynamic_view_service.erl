%%********************************************************************
%% @title Module ems_dynamic_view_service
%% @version 1.0.0
%% @doc It provides dynamic_view service for relational databases.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_dynamic_view_service).

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
-record(state, {connection,			 	%% connection to dynamic_view database
				datasource		 		%% odbc datasource
		}).  


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
	ems_pool:cast(ems_dynamic_view_service_pool, {find, Request, From}).

find_by_id(Request, From) ->
	ems_pool:cast(ems_dynamic_view_service_pool, {find_by_id, Request, From}).


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



execute_command(Command, 
				Request = #request{service = #service{datasource = Datasource,
													  table_name = TableName,
													  primary_key = PrimaryKey,
													  debug = Debug}}, 
				State) ->
	try
		case get_connection(Datasource, TableName, PrimaryKey, Debug) of
			{ok, ConnRef} -> 
				Result = case Command of
					find -> do_find(Request, ConnRef, State);
					find_by_id -> do_find_by_id(Request, ConnRef, State)
				end,
				release_connection(ConnRef),
				Result;
			{error, Reason} ->	{error, Reason}
		end
	catch
		_Exception:Reason2 -> {error, Reason2}
	end.


get_connection(_, _, _, true) -> {ok, null};
get_connection(Datasource, TableName, PrimaryKey, false) ->
	ConnType = get_datasource_type(Datasource),
	PidModule = erlang:pid_to_list(self()),
	case ConnType of
		odbc_datasource -> 
			{ok, Conn} = ems_db:get_odbc_connection(PidModule, Datasource),
			{ok, {ConnType, Datasource, Conn, PidModule}};
		csv_file -> 
			{ok, Conn} = ems_db:get_odbc_connection_csv_file(PidModule, Datasource, TableName, PrimaryKey, ";"),
			{ok, {odbc_datasource, ?DATABASE_SQLITE_STRING_CONNECTION, Conn, PidModule}};
		mnesia_db -> {ok, {ConnType, null, null}}
	end.


release_connection({mnesia_db, _, _, _}) -> ok;
release_connection({odbc_datasource, Datasource, Conn, PidModule}) -> ems_db:release_odbc_connection(PidModule, Datasource, Conn).


get_datasource_type(Datasource) ->
	Datasource2 = string:to_lower(Datasource),
	case lists:suffix(".csv", Datasource2) of
		true -> csv_file;
		_ -> 
			case lists:suffix(".erl", Datasource2) of
				true -> mnesia_db;
				_ -> odbc_datasource
			end
	end.
	

do_find(#request{querystring_map = QuerystringMap, 
				 service = #service{table_name = TableName,
									debug = Debug}},
				 ConnRef,
				 _State) ->
	FilterJson = maps:get(<<"filter">>, QuerystringMap, <<>>),
	Fields = binary_to_list(maps:get(<<"fields">>, QuerystringMap, <<>>)),
	Limit = binary_to_integer(maps:get(<<"limit">>, QuerystringMap, <<"100">>)),
	Offset = binary_to_integer(maps:get(<<"offset">>, QuerystringMap, <<"1">>)),
	Sort = binary_to_list(maps:get(<<"sort">>, QuerystringMap, <<>>)),
	ems_api_query:find(FilterJson, Fields, TableName, Limit, Offset, Sort, Debug, ConnRef).


do_find_by_id(Request = #request{querystring_map = QuerystringMap, 
								 service = #service{table_name = TableName,
													primary_key = PrimaryKey,
													debug = Debug}}, 
			 ConnRef,
			 _State) ->
	Id = ems_request:get_param_url(<<"id">>, 0, Request),
	Fields = binary_to_list(maps:get(<<"fields">>, QuerystringMap, <<>>)),
	ems_api_query:find_by_id(Id, Fields, TableName, PrimaryKey, Debug, ConnRef).

