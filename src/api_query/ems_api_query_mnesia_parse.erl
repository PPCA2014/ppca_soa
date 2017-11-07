%%********************************************************************
%% @title Module ems_api_query_mnesia_parse
%% @version 1.0.0
%% @doc It provides parse api query mnesia
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_mnesia_parse).

-export([generate_dynamic_query/6, generate_dynamic_query/3]).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").


generate_dynamic_query(FilterJson, Fields, Datasource, Limit, Offset, _Sort) ->
	FieldList = parse_fields(Fields, Datasource),
	FilterList = parse_filter(FilterJson, Datasource),
	%SortSmnt = parse_sort(Sort),
	LimitSmnt = parse_limit(Limit, Offset),
	{ok, {FieldList, FilterList, LimitSmnt}}.

generate_dynamic_query(_Id, Fields, Datasource) ->
	FieldList = parse_fields(Fields, Datasource),
	{ok, FieldList}.

   
parse_fields([], #service_datasource{fields = TblFields}) -> TblFields;
parse_fields(Fields, #service_datasource{fields = TblFields, remap_fields = RemapFields}) -> 
	case string:tokens(string:strip(Fields), ",") of
		[] -> TblFields;
		Fields2 -> 
			case RemapFields == undefined of
				true -> Fields2;
				false -> [binary_to_list(maps:get(F, RemapFields, F)) || F <- ems_util:list_to_binlist(Fields2)]
			end
	end.

parse_filter(<<>>, _) -> [];
parse_filter(Filter, Datasource) ->    
    case ems_util:json_decode(Filter) of
		{ok, Filter2} -> 
			parse_filter(Filter2, Datasource, []);
		_ -> 
			erlang:error(einvalid_filter)
	end.


parse_filter([], _, []) -> [];
parse_filter([], _, Filter) -> lists:reverse(Filter);
parse_filter([H|T], Datasource, Filter) ->
	{Param, Op, Value} = parse_condition(H, Datasource),
	parse_filter(T, Datasource, [{Param, Op, Value} | Filter]).

	
parse_condition({<<Param/binary>>, Value}, Datasource) when is_integer(Value) -> 
	Param2 = binary_to_list(Param), 
	parse_condition(Param2, Value, sql_integer, Datasource);
parse_condition({<<Param/binary>>, Value}, Datasource) when is_boolean(Value) -> 
	Param2 = binary_to_list(Param), 
	parse_condition(Param2, Value, sql_boolean, Datasource);
parse_condition({<<Param/binary>>, Value}, Datasource) -> 
	Param2 = binary_to_list(Param), 
	Value2 = binary_to_list(Value),
	parse_condition(Param2, Value2, sql_varchar, Datasource).
	
parse_condition(Param, Value, _DataType, Datasource) -> 
	{Param2, Op} = parse_name_and_operator(Param, Datasource),
	{Param2, Op, Value}.	

parse_name_and_operator(Param, #service_datasource{remap_fields = RemapFields}) ->
	case string:str(Param, "__") of
		Idx when Idx > 1 ->
			Name0 = string:sub_string(Param, 1, Idx-1),
			case RemapFields == undefined of
				true -> Name = Name0;
				false -> 
					NameBin = list_to_binary(Name0),
					Name = binary_to_list(maps:get(NameBin, RemapFields, NameBin))
			end,
			Op = string:sub_string(Param, Idx+2),
			case lists:member(Op, ["e", "ne", "gt", "gte", "lt", "lte"]) of
				true -> 
					Op2 = format_mnesia_operator(Op),
					{Name, Op2};
				_ -> throw({einvalid_condition, Param})
			end;
		0 -> 
			case RemapFields == undefined of
				true -> 
					{Param, "=="};
				false -> 
					NameBin = list_to_binary(Param),
					Param2 = binary_to_list(maps:get(NameBin, RemapFields, NameBin)),
					{Param2, "=="}
			end;
		_ -> throw({einvalid_condition, Param})
	end.


format_mnesia_operator("e") -> "==";
format_mnesia_operator("ne") -> "=/=";
format_mnesia_operator("gt") -> ">";
format_mnesia_operator("gte") -> ">=";
format_mnesia_operator("lt") -> "<" ;
format_mnesia_operator("lte") -> "=<";
format_mnesia_operator(_) -> erlang:error(invalid_operator_filter).


parse_limit(Limit, Offset) when Limit > 0, Offset >= 0, Limit =< ?MAX_LIMIT_API_QUERY, Offset =< ?MAX_OFFSET_API_QUERY -> {Limit, Offset};
parse_limit(_, _) -> erlang:error(einvalid_limit_filter).



	

