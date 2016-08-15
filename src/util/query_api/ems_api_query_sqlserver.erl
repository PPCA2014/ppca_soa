%%********************************************************************
%% @title Module ems_api_query_sqlserver
%% @version 1.0.0
%% @doc It provides api query functions for odbc database.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_sqlserver).

-export([find/7, find_by_id/4]).

-include("../../../include/ems_config.hrl").
-include("../../../include/ems_schema.hrl").


find(FilterJson, Fields, Limit, Offset, Sort, Datasource, Debug) ->
	case generate_dynamic_query(FilterJson, Fields, Datasource, Limit, Offset, Sort) of
		{ok, {Sql, Params}} -> execute_dynamic_query(Sql, Params, Datasource, Debug);
		{error, Reason} -> {error, Reason}
	end.


find_by_id(Id, Fields, Datasource, Debug) ->
	case generate_dynamic_query(Id, Fields, Datasource) of
		{ok, {Sql, Params}} -> execute_dynamic_query(Sql, Params, Datasource, Debug);
		{error, Reason} -> {error, Reason}
	end.

	    
parse_fields([]) -> "*";
parse_fields(Fields) -> 
	Fields2 = string:tokens(Fields, ","),
	string:join(Fields2, ",").	    

	    
parse_filter(<<>>) -> {"", ""};
parse_filter(Filter) ->    
    case ems_util:json_decode(Filter) of
		{ok, Filter2} -> parse_filter(Filter2, [], []);
		_ -> erlang:error(einvalid_filter)
	end.
parse_filter([], [], []) -> {"", ""};
parse_filter([], Filter, Params) -> 
	Filter2 = lists:flatten(lists:reverse(Filter)),
	Params2 = lists:flatten(lists:reverse(Params)),
	{"where " ++ Filter2, Params2};
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


parse_value(Param, Value, "e", DataType) when is_integer(Value) -> 
	OdbcDataType = format_odbc_data_type(Value, DataType),
	OdbcValue = [{OdbcDataType, [ Value ]}],
	{Param, OdbcValue};
parse_value(Param, Value, "e", DataType) -> 
	OdbcDataType = format_odbc_data_type(Value, DataType),
	OdbcValue = [{OdbcDataType, [ Value ]}],
	{Param, OdbcValue};
parse_value(Param, Value, "ne", DataType) -> 
	OdbcDataType = format_odbc_data_type(Value, DataType),
	OdbcValue = [{OdbcDataType, [Value]}],
	{Param, OdbcValue};
parse_value(Param, Value, "like", sql_varchar) -> 
	OdbcDataType = format_odbc_data_type(Value, sql_varchar),
	Value2 = Value ++ "%",
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

format_odbc_data_type(Value, sql_varchar) -> {sql_varchar, length(Value)};
format_odbc_data_type(_Value, sql_integer) -> sql_integer;
format_odbc_data_type(_Value, sql_boolean) -> sql_boolean;
format_odbc_data_type(_, _) -> erlang:error(einvalid_odbc_data_type).

parse_name_and_operator(Param) ->
	case string:str(Param, "__") of
		Idx when Idx > 1 ->
			Name = string:sub_string(Param, 1, Idx-1),
			Op = string:to_lower(string:sub_string(Param, Idx+2)),
			case lists:member(Op, ["like", "ilike", "contains", "icontains", "e", "ne", "gt", "gte", "lt", "lte", "isnull"]) of
				true -> {Name, Op};
				_ -> erlang:error(einvalid_param_filter)
			end;
		0 -> {Param, "e"};
		_ -> erlang:error(einvalid_param_filter)
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


parse_sort([]) -> "";
parse_sort(SortFields) -> 
	SortFields2 = string:tokens(SortFields, ","),
	"order by " ++ parse_sort(SortFields2, []).

parse_sort([], L) -> string:join(L, ",");		
parse_sort([F|Fs], L) -> 
	F2 = string:tokens(string:to_lower(F), " "),
	case length(F2) of
		2 -> 
			F3 = parse_sort_asc_desc(F, tl(F2)),
			parse_sort(Fs, [F3 | L]);
		_ -> parse_sort(Fs, [F | L])
	end.
		

parse_sort_asc_desc(F, ["asc"]) -> F;
parse_sort_asc_desc(F, ["desc"]) -> F;
parse_sort_asc_desc(_, _) -> erlang:error(invalid_sort_filter).
	


parse_limit(Limit, Offset) when Limit > 0, Offset >= 0, Limit < ?MAX_LIMIT_API_QUERY, Offset =< ?MAX_OFFSET_API_QUERY ->
	io_lib:format("_t._RowNumber BETWEEN ~p AND ~p", [Offset, Offset+Limit-1]);
parse_limit(_, _) -> erlang:error(einvalid_limit_filter).


generate_dynamic_query(FilterJson, Fields, 
					   #service_datasource{table_name = TableName, 
										   primary_key = PrimaryKey}, 
					   Limit, Offset, Sort) ->
	{FilterSmnt, Params} = parse_filter(FilterJson),
	FieldsSmnt = parse_fields(Fields),
	SortSmnt = parse_sort(Sort),
	LimitSmnt = parse_limit(Limit, Offset),
	SqlSmnt = lists:flatten(io_lib:format("select * from (select ~s, ROW_NUMBER() OVER (ORDER BY ~s) AS _RowNumber from ~s ~s ~s) _t where ~s", [FieldsSmnt, PrimaryKey, TableName, FilterSmnt, SortSmnt, LimitSmnt])),
	io:format("sql is ~p\n", [SqlSmnt]),
	{ok, {SqlSmnt, Params}}.


generate_dynamic_query(Id, Fields, #service_datasource{table_name = TableName, primary_key = PrimaryKey}) ->
	Params = [{sql_integer, [Id]}],
	Fields2 = parse_fields(Fields),
	Sql = lists:flatten(io_lib:format("select ~s from ~s where ~s = ?", [Fields2, TableName, PrimaryKey])),
	{ok, {Sql, Params}}.


execute_dynamic_query(Sql, _, _, true) -> 
	Result = list_to_binary(io_lib:format("{\"sql\" : ~p}", [Sql])), 
	{ok, Result};
execute_dynamic_query(Sql, Params, #service_datasource{conn_ref = Conn}, false) ->
	try
		case odbc:param_query(Conn, Sql, Params, ?MAX_TIME_ODBC_QUERY) of
			{_, Fields, Records} -> 
				Objects = ems_util:json_encode_table(Fields, Records),
				{ok, Objects};
			{error, Reason} -> {error, Reason}
		end
	catch
		_Exception:Reason2 -> {error, Reason2}
	end.

