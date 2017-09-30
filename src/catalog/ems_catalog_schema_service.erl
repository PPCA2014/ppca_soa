%%********************************************************************
%% @title Módulo ems_catalog_schema_service
%% @version 1.0.0
%% @doc Módulo de serviço ems_catalog_schema_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_schema_service).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%% Cliente interno API
-export([all/1, find_by_id/1, insert/1, update/1, delete/1]).


%%====================================================================
%% Client API
%%====================================================================
 

find_by_id(Request) -> 
	Id = ems_util:get_param_url(<<"id">>, -1, Request),
	{ok, Record} = ems_catalog_schema:find_by_id(Id),
	ems_schema:to_json(Record).
	
insert(#request{payload_map = CatalogSchemaMap}) ->
	ems_catalog_schema:insert(CatalogSchemaMap).

update(Request = #request{payload_map = CatalogSchemaMap}) ->
	Id = ems_util:get_param_url(<<"id">>, -1, Request),
	ems_catalog_schema:update(Id, CatalogSchemaMap).

all(_Request) -> 
	{ok, Records} = ems_catalog_schema:all(),
	ems_schema:to_json(Records).
	
delete(Request) -> 
	Id = ems_util:get_param_url(<<"id">>, -1, Request),
	ems_catalog_schema:delete(Id).
	

