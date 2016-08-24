%%********************************************************************
%% @title Module ems_api_query_mnesia_parse
%% @version 1.0.0
%% @doc It provides parse api query mnesia
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_mnesia_parse).

-export([generate_dynamic_query/6, generate_dynamic_query/3]).

-include("../../../include/ems_schema.hrl").


generate_dynamic_query(FilterJson, Fields, Datasource, Limit, Offset, Sort) ->
	FieldList = parse_fields(Fields),
	FilterList = parse_filter(FilterJson),
	SortSmnt = parse_sort(Sort),
	LimitSmnt = parse_limit(Limit, Offset),
	{ok, {FieldList, FilterList, LimitSmnt}}.

generate_dynamic_query(Id, Fields, Datasource) ->
	FieldList = parse_fields(Fields),
	{ok, FieldList}.

   
parse_fields([]) -> [];
parse_fields(Fields) -> string:tokens(Fields, ",").

parse_filter(<<>>) -> [];
parse_filter(Filter) ->    
    case ems_util:json_decode(Filter) of
		{ok, Filter2} -> parse_filter(Filter2, []);
		_ -> erlang:error(einvalid_filter)
	end.


parse_filter([], []) -> [];
parse_filter([], Filter) -> lists:reverse(Filter);
parse_filter([H|T], Filter) ->
	{Param, Op, Value} = parse_condition(H),
	parse_filter(T, [{Param, Op, Value} | Filter]).

	
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
	{Param2, Op, Value}.	


parse_name_and_operator(Param) ->
	case string:str(Param, "__") of
		Idx when Idx > 1 ->
			Name = string:sub_string(Param, 1, Idx-1),
			Op = string:sub_string(Param, Idx+2),
			case lists:member(Op, ["like", "ilike", "contains", "icontains", "e", "ne", "gt", "gte", "lt", "lte", "isnull"]) of
				true -> 
					Op2 = format_mnesia_operator(Op),
					{Name, Op2};
				_ -> erlang:error(einvalid_operator_filter)
			end;
		0 -> {Param, "=="};
		_ -> erlang:error(einvalid_param_filter)
	end.


format_mnesia_operator("e") -> "==";
format_mnesia_operator("ne") -> "=/=";
format_mnesia_operator("like") -> "like";
format_mnesia_operator("ilike") -> "like";
format_mnesia_operator("contains") -> "like";
format_mnesia_operator("icontains") -> "like";
format_mnesia_operator("gt") -> ">";
format_mnesia_operator("gte") -> ">=";
format_mnesia_operator("lt") -> "<" ;
format_mnesia_operator("lte") -> "=<";
format_mnesia_operator("isnull") -> "is null";
format_mnesia_operator(_) -> erlang:error(invalid_operator_filter).


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
	

parse_limit(Limit, Offset) when Limit > 0, Offset > 0, Limit < 9999, Offset < 9999 -> {Limit, Offset};
parse_limit(_, _) -> erlang:error(einvalid_limit_filter).



	

