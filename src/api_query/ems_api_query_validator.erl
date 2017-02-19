%%********************************************************************
%% @title Module ems_api_query_validator
%% @version 1.0.0
%% @doc It validates with json schema
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_validator).

-export([validate/2]).

-include("../../include/ems_schema.hrl").

validate(_, null) -> ok;
validate(Record, Schema) when is_tuple(Record) ->
	validate(maps:from_list(ems_schema:to_list(Record)), Schema);
validate(Map, Schema) ->
	case ems_catalog_schema:find_by_name(Schema) of
		{error, enoent} -> {error, eschema_not_found};
		CatalogSchema ->
			jesse:add_schema(Schema, CatalogSchema#catalog_schema.json_schema),
			case jesse:validate(Schema, Map) of
				{ok, _} -> ok;
				{error, [{data_invalid, SchemaErrorMap, TypeError, TypeValue, [TypeField]}]} ->
					{error, {<<"field">>, TypeField,  <<"reason">>, TypeError, <<"value">>, TypeValue, <<"field_def">>, SchemaErrorMap}};
				{error, [{data_invalid, SchemaErrorMap, missing_required_property, _, []}]} ->
					{error, {<<"reason">>, missing_required_property, <<"schema">>, SchemaErrorMap}};
				{error, [{data_invalid, _SchemaErrorMap, not_one_schema_valid, _Object, _}]} ->
					{error, not_one_schema_valid};
				{error,[{schema_invalid, SchemaErrorMap = #{<<"type">> := TypeField}, wrong_type_specification}]} ->
                    {error, {<<"field">>, TypeField,  <<"reason">>, wrong_type_specification, <<"field_def">>, SchemaErrorMap}};
				Other -> 
					ems_logger:error("ems_api_query_validator validation schema error: ~p.", [Other]),
					Other
			end
	end.


	


