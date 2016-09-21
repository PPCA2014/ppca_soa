%%********************************************************************
%% @title Module ems_catalog
%% @version 1.0.0
%% @doc Manages information about catalog CatalogSchemas
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_schema).

-compile({parse_transform, exprecs}).

-record(book, {
      style      :: atom(),
      count      :: integer(),
      available  :: boolean(),
      pages      :: integer(),
      excerpt    :: string(),
      author     :: string()
     }). 


-export_records([book, schema_type]).


-export([new/0, new/1, find_by_id/1, insert_or_update/1, insert/1, 
		 update/2, all/0, delete/1, to_record/2, find_by_name/1,
		 find_id_by_name/1]).


-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


%% new records

new() -> '#new-schema_type'().

new(book) -> '#new-book'();
new(schema_type) -> '#new-schema_type'();
new(_) ->  undefined.


%% Convertions

to_record(Json, Type) when is_binary(Json), is_atom(Type) ->
	{ok, JsonMap} = ems_util:json_decode(Json),
	JsonStruct = {struct, JsonMap},
	NewRecord = new(Type),
		io:format("struct is ~p\n", [JsonStruct]),
	json_rec:to_rec(JsonStruct, ?MODULE, NewRecord);

to_record(Json, Record) when is_binary(Json), is_tuple(Record)->
	{ok, JsonMap} = ems_util:json_decode(Json),
	JsonStruct = {struct, JsonMap},
	io:format("struct is ~p\n", [JsonStruct]),
		json_rec:to_rec(JsonStruct, ?MODULE, Record);

to_record(Map, Type) when is_map(Map), is_atom(Type) ->
	List = maps:to_list(Map),
	JsonStruct = {struct, List},
	NewRecord = new(Type),
	json_rec:to_rec(JsonStruct, ?MODULE, NewRecord);

to_record(Map, Record) when is_map(Map), is_tuple(Record)->
	List = maps:to_list(Map),
	JsonStruct = {struct, List},
	io:format("struct is ~p\n", [JsonStruct]),
	json_rec:to_rec(JsonStruct, ?MODULE, Record);

to_record(_, _) -> erlang:error(einvalid_to_record).

%% Finds

find_by_id(Id) -> ems_db:get(catalog_schema, Id).

find_by_name(Name) -> ems_db:find_first(catalog_schema, {name, "==", Name}).

find_id_by_name(Name) -> 
	case ems_db:find_first(catalog_schema, [id], {name, "==", Name}) of
		[{_, Id}] -> Id;
		_Error -> erlang:error(einvalid_catalog_schema_name)
	end.

all() -> ems_db:all(catalog_schema).


%% CRUDs

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
            _ -> {error, enoent}
		end
	end,
	mnesia:activity(transaction, F).
	

delete(Id) -> ems_db:delete(catalog_schema, Id).



	
