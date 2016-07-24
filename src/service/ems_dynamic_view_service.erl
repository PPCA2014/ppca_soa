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
	create_ets_dynamic_view(),
    State = #state{},
    {ok, State}. 
    
create_ets_dynamic_view() ->
    try
		ets:new(ets_dynamic_view, [set, named_table, public]),
		{ok, Re_Check_Param} = re:compile("^([a-zA-Z_]+)(__(like|ilike|contains|icontains|e|ne|gt|gte|lt|lte|isnull)?$)"),
		ets:insert(ets_dynamic_view, {re_check_param, Re_Check_Param})
	catch
		_:_ -> ok
	end.
    
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
	{Cond, Params2} = format_sql_condition(Param, Op, Value, And, Params),
	parse_filter(T, [Cond | Filter], Params2).

	
filter_conjuntion([]) -> "";
filter_conjuntion(_) -> " and ".

	
parse_condition({<<Param/binary>>, Value}) when is_integer(Value) -> 
	Param2 = binary_to_list(Param), 
	parse_condition(Param2, Value, sql_integer);

parse_condition({<<Param/binary>>, Value}) when is_boolean(Value) -> 
	Param2 = binary_to_list(Param), 
	parse_condition(Param2, Value, sql_boolean);

parse_condition({<<Param/binary>>, Value}) -> 
	Param2 = binary_to_list(Param), 
	Value2 = binary_to_list(Value),
	parse_condition(Param2, Value2, sql_varchar).
	
parse_condition(Param, Value, DataType) -> 
	{Param2, Op} = parse_name_and_operator(Param),
	{Param3, Value2} = parse_value(Param2, Value, Op, DataType),
	{Param3, Op, Value2}.	


parse_value(Param, Value, "e", DataType) -> 
	OdbcDataType = format_odbc_data_type(Value, DataType),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "ne", DataType) -> 
	OdbcDataType = format_odbc_data_type(Value, DataType),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "like", sql_varchar) -> 
	OdbcDataType = format_odbc_data_type(Value, sql_varchar),
	Value2 = "%" ++ Value ++ "%",
	OdbcValue = [{OdbcDataType, [Value2]}],
	{Param, OdbcValue};
	
parse_value(Param, Value, "ilike", sql_varchar) -> 
	OdbcDataType = format_odbc_data_type(Value, sql_varchar),
	Param2 = "lower(" ++ Param ++ ")",
	Value2 = string:to_lower(Value) ++ "%",
	OdbcValue = [{OdbcDataType, [Value2]}],
	{Param2, OdbcValue};

parse_value(Param, Value, "contains", sql_varchar) -> 
	OdbcDataType = format_odbc_data_type(Value, sql_varchar),
	Value2 = "%" ++ Value ++ "%",
	OdbcValue = [{OdbcDataType, [Value2]}],
	{Param, OdbcValue};
	
parse_value(Param, Value, "icontains", sql_varchar) -> 
	OdbcDataType = format_odbc_data_type(Value, sql_varchar),
	Param2 = "lower(" ++ Param ++ ")",
	Value2 = "%" ++ string:to_lower(Value) ++ "%",
	OdbcValue = [{OdbcDataType, [Value2]}],
	{Param2, OdbcValue};
	
parse_value(Param, Value, "isnull", sql_boolean) -> 
	OdbcDataType = format_odbc_data_type(Value, sql_boolean),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "gt", sql_integer) -> 
	OdbcDataType = format_odbc_data_type(Value, sql_integer),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "gte", sql_integer) -> 
	OdbcDataType = format_odbc_data_type(Value, sql_integer),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "lt", sql_integer) -> 
	OdbcDataType = format_odbc_data_type(Value, sql_integer),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(Param, Value, "lte", sql_integer) -> 
	OdbcDataType = format_odbc_data_type(Value, sql_integer),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};

parse_value(_, _, _, _) -> 
	erlang:error(einvalid_operator_filter).

format_odbc_data_type(_Value, sql_varchar) -> {sql_varchar, 100};
format_odbc_data_type(_Value, sql_integer) -> sql_integer;
format_odbc_data_type(_Value, sql_boolean) -> sql_boolean;
format_odbc_data_type(_, _) -> erlang:error(einvalid_odbc_data_type).

parse_name_and_operator(Param) ->
	[{_, RE}] = ets:lookup(ets_dynamic_view, re_check_param),
	case re:run(Param, RE) of
		{match, [_, {PosNameIni, NameLen}]} -> 
			Name = string:sub_string(Param, PosNameIni+1, PosNameIni+1+NameLen),
			{Name, "e"};
		{match, [_, {PosNameIni, NameLen}, _, {PosOpIni, OpLen}]} ->
			Name = string:sub_string(Param, PosNameIni+1, PosNameIni+NameLen),
			Op = string:sub_string(Param, PosOpIni+1, PosOpIni+1+OpLen),
			{Name, Op};
		nomatch -> erlang:error(einvalid_filter)
	end.


format_sql_condition(Param, "isnull", Value, And, Params) -> 
	case Value of
		[{sql_boolean,[true]}] -> Cond = lists:flatten([Param, " is null", And]);
		_ -> Cond = lists:flatten([Param, " is not null", And])
	end,
	{Cond, Params};

format_sql_condition(Param, Op, Value, And, Params) ->
	SqlOp = format_sql_operator(Op),
	Cond = lists:flatten([Param, " ", SqlOp, " ", "?", And]),
	{Cond, [Value | Params]}.

format_sql_operator("e") -> "=";
format_sql_operator("ne") -> "!=";
format_sql_operator("like") -> "like";
format_sql_operator("ilike") -> "like";
format_sql_operator("contains") -> "like";
format_sql_operator("icontains") -> "like";
format_sql_operator("gt") -> ">";
format_sql_operator("gte") -> ">=";
format_sql_operator("lt") -> "<" ;
format_sql_operator("lte") -> "<=";
format_sql_operator("isnull") -> "is null";
format_sql_operator(_) -> erlang:error(invalid_operator_filter).


generate_dynamic_sql(<<>>, TableName) ->
	Sql = "select * from " ++ TableName,
	{ok, {Sql, []}};


generate_dynamic_sql(FilterJson, TableName) ->
	{Filter, Params} = parse_filter(FilterJson),
	Sql = "select * from " ++ TableName ++ Filter,
	{ok, {Sql, Params}}.


generate_dynamic_sql(Id, TableName, Primary_key) ->
	Params = [{sql_integer, [Id]}],
	Sql = "select * from " ++ TableName ++ " where " ++ Primary_key ++ " = ?",
	{ok, {Sql, Params}}.


execute_dynamic_sql(Sql, _, _, true) ->  {ok, Sql};

execute_dynamic_sql(Sql, Params, Datasource, false) ->
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
		_Exception:Reason2 -> {error, Reason2}
	after 
		ems_db:release_odbc_connection(Conn)
	end.


do_find(#request{querystring_map = QuerystringMap, 
				 service = #service{datasource = Datasource, 
									table_name = TableName,
									debug = Debug}}, _State) ->
	FilterJson = maps:get(<<"filter">>, QuerystringMap, <<>>),
	case generate_dynamic_sql(FilterJson, TableName) of
		{ok, {Sql, Params}} -> execute_dynamic_sql(Sql, Params, Datasource, Debug);
		{error, Reason} -> {error, Reason}
	end.


do_find_by_id(Request = #request{service = #service{datasource = Datasource, 
													table_name = TableName,
													primary_key = Primary_key,
													debug = Debug}}, _State) ->
	Id = list_to_integer(ems_request:get_param_url(<<"id">>, "0", Request)),
	case generate_dynamic_sql(Id, TableName, Primary_key) of
		{ok, {Sql, Params}} -> execute_dynamic_sql(Sql, Params, Datasource, Debug);
		{error, Reason} -> {error, Reason}
	end.

