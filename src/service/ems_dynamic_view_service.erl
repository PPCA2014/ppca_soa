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
	Result = execute_query(find, Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Result}),
	{noreply, State};

handle_cast({find_by_id, Request, _From}, State) ->
	Result = execute_query(find_by_id, Request, State),
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

execute_query(Command, Request, State) ->
	try
		case Command of
			find -> do_find(Request, State);
			find_by_id -> do_find_by_id(Request, State)
		end
	catch
		_Exception:Reason2 -> {error, Reason2}
	end.
	    
	    
parse_filter(<<>>) -> [];   


parse_filter(Filter) ->    
    case ems_util:json_decode(Filter) of
		{ok, Filter2} -> parse_filter(Filter2, [], []);
		_ -> erlang:error(einvalid_filter)
	end.
parse_filter([], [], []) -> {"", ""};

	
parse_filter([], Filter, Params) -> 
	Filter2 = lists:flatten(lists:reverse(Filter)),
	Params2 = lists:flatten(lists:reverse(Params)),
	{" where " ++ Filter2, Params2};

parse_filter([H|T], Filter, Params) ->
	{Param, Op, Value} = parse_condition(H),
	And = filter_conjuntion(T),
	{Cond, Params2} = generate_condition(Param, Op, Value, And, Params),
	parse_filter(T, [Cond | Filter], Params2).

	
filter_conjuntion([]) -> "";
filter_conjuntion(_) -> "and".


generate_condition(Param, "isnull", "true", And, Params) -> 
	Cond = lists:flatten([Param, " is null ", And, " "]),
	{Cond, Params};

generate_condition(Param, "isnull", "false", And, Params) -> 
	Cond = lists:flatten([Param, " is not null ", And, " "]),
	{Cond, Params};

generate_condition(_, "isnull", _, _, _) -> 
	erlang:error(einvalid_filter);
	
generate_condition(Param, Op, Value, And, Params) ->
	SqlOp = parse_sql_operator(Op),
	Cond = lists:flatten([Param, " ", SqlOp, " ", "?", " ", And, " "]),
	{Cond, [Value | Params]}.

	
parse_condition({<<Param/binary>>, Value}) when is_integer(Value) -> 
	Param2 = binary_to_list(Param), 
	parse_condition(Param2, Value, sql_integer);

parse_condition({<<Param/binary>>, Value}) -> 
	Param2 = binary_to_list(Param), 
	Value2 = binary_to_list(Value),
	parse_condition(Param2, Value2, sql_varchar).


parse_condition(Param, Value, DataType) -> 
	{Param2, Op} = parse_name_and_operator(Param),
	{Param3, Value2} = parse_value(Param2, Value, Op, DataType),
	{Param3, Op, Value2}.	


parse_value(Param, Value, "=", DataType) -> 
	OdbcDataType = parse_odbc_data_type(Value, DataType),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "like", sql_varchar) -> 
	OdbcDataType = parse_odbc_data_type(Value, sql_varchar),
	Value2 = "%" ++ Value ++ "%",
	OdbcValue = [{OdbcDataType, [Value2]}],
	{Param, OdbcValue};
	
parse_value(Param, Value, "ilike", sql_varchar) -> 
	OdbcDataType = parse_odbc_data_type(Value, sql_varchar),
	Param2 = "lower(" ++ Param ++ ")",
	Value2 = string:to_lower(Value) ++ "%",
	OdbcValue = [{OdbcDataType, [Value2]}],
	{Param2, OdbcValue};

parse_value(Param, Value, "contains", sql_varchar) -> 
	OdbcDataType = parse_odbc_data_type(Value, sql_varchar),
	Value2 = "%" ++ Value ++ "%",
	OdbcValue = [{OdbcDataType, [Value2]}],
	{Param, OdbcValue};
	
parse_value(Param, Value, "icontains", sql_varchar) -> 
	OdbcDataType = parse_odbc_data_type(Value, sql_varchar),
	Param2 = "lower(" ++ Param ++ ")",
	Value2 = "%" ++ string:to_lower(Value) ++ "%",
	OdbcValue = [{OdbcDataType, [Value2]}],
	{Param2, OdbcValue};
	
parse_value(Param, Value, "isnull", sql_varchar) -> 
	OdbcDataType = parse_odbc_data_type(Value, sql_varchar),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "gt", sql_integer) -> 
	OdbcDataType = parse_odbc_data_type(Value, sql_integer),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "gte", sql_integer) -> 
	OdbcDataType = parse_odbc_data_type(Value, sql_integer),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "lt", sql_integer) -> 
	OdbcDataType = parse_odbc_data_type(Value, sql_integer),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "lte", sql_integer) -> 
	OdbcDataType = parse_odbc_data_type(Value, sql_integer),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(_, _, _, _) -> 
	erlang:error(einvalid_filter).

parse_odbc_data_type(_Value, sql_varchar) -> {sql_varchar, 100};
parse_odbc_data_type(_Value, sql_integer) -> sql_integer.

parse_name_and_operator(Param) ->
	case re:run(Param, "^([a-zA-Z]+)(__(like|ilike|contains|icontains|equal|gt|gte|lt|lte|isnull))?$") of
		{match, [_, {PosNameIni, NameLen}]} -> 
			Name = string:sub_string(Param, PosNameIni+1, PosNameIni+1+NameLen),
			{Name, "="};
		{match, [_, {PosNameIni, NameLen}, _, {PosOpIni, OpLen}]} ->
			Name = string:sub_string(Param, PosNameIni+1, PosNameIni+NameLen),
			Op = string:sub_string(Param, PosOpIni+1, PosOpIni+1+OpLen),
			{Name, Op};
		nomatch -> erlang:error(einvalid_filter)
	end.


parse_sql_operator("like") -> "like";
parse_sql_operator("ilike") -> "like";
parse_sql_operator("contains") -> "like";
parse_sql_operator("icontains") -> "like";
parse_sql_operator("equal") -> "=";
parse_sql_operator("gt") -> ">";
parse_sql_operator("gte") -> ">=";
parse_sql_operator("lt") -> "<" ;
parse_sql_operator("lte") -> "<=";
parse_sql_operator("isnull") -> "is null";
parse_sql_operator(_) -> "=".


generate_dynamic_sql(<<>>, TableName) ->
	Sql = "select * from " ++ TableName,
	Params = [],
	io:format("\n\nSQL-> ~p  ~p\n", [Sql, Params]),
	{ok, {Sql, Params}};


generate_dynamic_sql(FilterJson, TableName) ->
	{Filter, Params} = parse_filter(FilterJson),
	Sql = "select * from " ++ TableName ++ Filter,
	io:format("\n\nSQL-> ~p  ~p\n", [Sql, Params]),
	{ok, {Sql, Params}}.


generate_dynamic_sql(Id, TableName, Primary_key) ->
	Params = [{sql_integer, [Id]}],
	Sql = "select * from " ++ TableName ++ " where " ++ Primary_key ++ "= ?",
	{ok, {Sql, Params}}.


execute_dynamic_sql(Sql, Params, Datasource) ->
	case ems_db:get_odbc_connection(Datasource) of
		{ok, Conn} -> fetch_records_database(Sql, Params, Conn);
		{error, Reason} ->	{error, Reason}
	end.


fetch_records_database(Sql, Params, Conn) ->
	try
		case odbc:param_query(Conn, Sql, Params, 3500) of
			{_, _, Records} -> {ok, Records};
			{error, Reason} -> {error, Reason}
		end
	catch
		_Exception:Reason2 -> 
			ems_logger:error("Execute dynamic_view query database error. Reason: ~p", [Reason2]),
			{error, Reason2}
	after 
		ems_db:release_odbc_connection(Conn)
	end.


do_find(#request{querystring_map = QuerystringMap, 
				 service = #service{datasource = Datasource, 
									table_name = TableName}}, _State) ->
	FilterJson = maps:get(<<"filter">>, QuerystringMap, <<>>),
	case generate_dynamic_sql(FilterJson, TableName) of
		{ok, {Sql, Params}} -> execute_dynamic_sql(Sql, Params, Datasource);
		{error, Reason} -> {error, Reason}
	end.


do_find_by_id(Request = #request{service = #service{datasource = Datasource, 
													table_name = TableName,
													primary_key = Primary_key}}, _State) ->
	Id = list_to_integer(ems_request:get_param_url(<<"id">>, "0", Request)),
	case generate_dynamic_sql(Id, TableName, Primary_key) of
		{ok, {Sql, Params}} -> execute_dynamic_sql(Sql, Params, Datasource);
		{error, Reason} -> {error, Reason}
	end.

