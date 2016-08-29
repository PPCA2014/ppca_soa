%%********************************************************************
%% @title Module ems_api_query_validator
%% @version 1.0.0
%% @doc It validates with json schema
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_validator).

-export([validate/2]).

-include("../../../include/ems_schema.hrl").


validate(Record, CatalogSchemaId) when is_tuple(Record) ->
	validate(maps:from_list(ems_schema:to_list(Record)), CatalogSchemaId);
validate(Map, CatalogSchemaId) ->
	{ok, CatalogSchema} = ems_catalog_schema:find_by_id(CatalogSchemaId),
	jesse:add_schema(CatalogSchemaId, CatalogSchema#catalog_schema.json_schema),
	case jesse:validate(CatalogSchemaId, Map) of
		{ok, _} -> ok;
		{error, [{data_invalid, SchemaErrorMap, TypeError, TypeValue, [TypeField]}]} ->
			{error, {<<"field">>, TypeField,  <<"reason">>, TypeError, <<"value">>, TypeValue, <<"field_def">>, SchemaErrorMap}};
		{error, [{data_invalid, SchemaErrorMap, missing_required_property, _, []}]} ->
			{error, {<<"reason">>, missing_required_property, <<"schema">>, SchemaErrorMap}};
		Other -> io:format("~p\n", [Other]), {error, naosei}
	end.


	


