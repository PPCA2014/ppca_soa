-module(ems_catalog_schema_middleware).

-export([onvalidate/2]).

-include("../include/ems_schema.hrl").

onvalidate(Operation, CatalogSchema) ->
	case unique(Operation, CatalogSchema) of
		true -> ok;
		false -> {error, ealready_exist}
	end.

unique(update, #catalog_schema{id = Id, name = Name}) ->
	case ems_db:find_first(catalog_schema, [{id, "=/=", Id}, {name, "==", Name}]) of
		{error, enoent} -> true;
		_ -> false
	end;
unique(insert, #catalog_schema{name = Name}) ->
	case ems_db:find_first(catalog_schema, [{name, "==", Name}]) of
		{error, enoent} -> true;
		_ -> false
	end.
	
