%%********************************************************************
%% @title Module ems_catalog
%% @version 1.0.0
%% @doc Manages information about catalog CatalogSchemas
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_schema).

-export([find_by_id/1, insert_or_update/1, insert/1, update/2, all/0, delete/1]).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


find_by_id(Id) -> ems_db:get(catalog_schema, Id).

insert_or_update(CatalogSchemaMap) when is_map(CatalogSchemaMap) -> 
	Name = maps:get(<<"name">>, CatalogSchemaMap),
	Description = maps:get(<<"description">>, CatalogSchemaMap),
	JsonSchema = maps:get(<<"json_schema">>, CatalogSchemaMap),
	F = fun() -> 
		Match = ets:fun2ms(fun(Schema = #catalog_schema{name = Name2}) when Name2 =:= Name -> Schema end),
		CatalogSchema = case mnesia:select(catalog_schema, Match) of
			[] -> 
				 Id = ems_db:sequence(catalog_schema),
				 #catalog_schema{id = Id,
								 name = Name,
								 description = Description,
								 json_schema = JsonSchema};
            L -> 
				R = hd(L),
				R#catalog_schema{description = Description,
								 json_schema = JsonSchema}
		end,
		mnesia:write(CatalogSchema),
		CatalogSchema
	end,
	mnesia:activity(transaction, F).

insert(CatalogSchemaMap) when is_map(CatalogSchemaMap) -> 
	Name = maps:get(<<"name">>, CatalogSchemaMap),
	Description = maps:get(<<"description">>, CatalogSchemaMap),
	JsonSchema = maps:get(<<"json_schema">>, CatalogSchemaMap),
	F = fun() -> 
		Match = ets:fun2ms(fun(Schema = #catalog_schema{name = Name2}) when Name2 =:= Name -> Schema end),
		case mnesia:select(catalog_schema, Match) of
			[] -> 
				 Id = ems_db:sequence(catalog_schema),
				 CatalogSchemaMap2 = #catalog_schema{id = Id,
													 name = Name,
													 description = Description,
													 json_schema = JsonSchema},
				 mnesia:write(CatalogSchemaMap2),
				 CatalogSchemaMap2;
            _ -> {error, ealready_exist}
		end
	end,
	mnesia:activity(transaction, F).

update(Id, CatalogSchemaMap) when is_map(CatalogSchemaMap) -> 
	F = fun() -> 
		case find_by_id(Id) of
			{ok, CatalogSchemaMap2} ->
				Name = maps:get(<<"name">>, CatalogSchemaMap, CatalogSchemaMap2#catalog_schema.name),
				Description = maps:get(<<"description">>, CatalogSchemaMap, CatalogSchemaMap2#catalog_schema.description),
				JsonSchema = maps:get(<<"json_schema">>, CatalogSchemaMap, CatalogSchemaMap2#catalog_schema.json_schema),
				CatalogSchemaMap3 = #catalog_schema{id = Id,
													name = Name,
													description = Description,
													json_schema = JsonSchema},
				mnesia:write(CatalogSchemaMap3),
				CatalogSchemaMap3;
            _ -> {error, notfound}
		end
	end,
	mnesia:activity(transaction, F).
	

all() -> ems_db:all(catalog_schema).

delete(Id) -> ems_db:delete(catalog_schema, Id).



	
