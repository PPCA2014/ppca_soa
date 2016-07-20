%%********************************************************************
%% @title Module ems_dynamic_view_service
%% @version 1.0.0
%% @doc It provides dynamic_view service.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_dynamic_view_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).


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
	Result = do_find(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Result}),
	{noreply, State};

handle_cast({find_by_id, Request, _From}, State) ->
	Result = do_find_by_id(Request, State),
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
    

do_find(#request{service = #service{datasource = Datasource, 
								 table_name = TableName}}, _State) ->
	case ems_db:get_odbc_connection(Datasource) of
		{ok, Conn} -> 
			try
				Params = [],
				Sql = "select * from " ++ TableName,
				case odbc:param_query(Conn, Sql, Params, 3500) of
					{_, _, Record} -> 
						{ok, Record};
					{error, Reason} ->
						{error, Reason}
				end
			catch
				_Exception:Reason2 -> 
					ems_logger:error("Connection or query dynamic_view database error. Reason: ~p", [Reason2]),
					{error, Reason2}
			after 
				ems_db:release_odbc_connection(Conn)
			end;
		{error, Reason3} ->
			{error, Reason3}
	end.

do_find_by_id(Request = #request{service = #service{datasource = Datasource, 
													table_name = TableName,
													primary_key = Primary_key}}, _State) ->
	Id = list_to_integer(ems_request:get_param_url(<<"id">>, "0", Request)),
	case ems_db:get_odbc_connection(Datasource) of
		{ok, Conn} -> 
			try
				Params = [{sql_integer, [Id]}],
				Sql = "select * from " ++ TableName ++ " where " ++ Primary_key ++ "= ?",
				case odbc:param_query(Conn, Sql, Params, 3500) of
					{selected, _Columns, Records} -> 
						{ok, Records};
					{error, Reason} ->
						{error, Reason}
				end
			catch
				_Exception:Reason2 -> 
					ems_logger:error("Connection or query dynamic_view database error. Reason: ~p", [Reason2]),
					{error, Reason2}
			after 
				ems_db:release_odbc_connection(Conn)
			end;
		{error, Reason3} ->
			{error, Reason3}
	end.



