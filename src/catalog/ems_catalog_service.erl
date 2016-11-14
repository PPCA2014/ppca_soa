%%********************************************************************
%% @title Módulo ems_catalog_service
%% @version 1.0.0
%% @doc Module ems_catalog_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_service).

-include("../../include/ems_schema.hrl").

-export([list_catalog/1, get/1, insert/1, update/1, delete/1]).


%%====================================================================
%% Cliente API
%%====================================================================
 
list_catalog(Request) ->
	CatalogJson = ems_util:json_encode(ems_catalog:list_catalog()),
	{ok, Request#request{code = 200,
						 response_data = CatalogJson}
	}.

get(Request)	->
	{ok, Request#request{code = 200,
						 response_data = {ok, nao_implementado}}
	}.

	
insert(Request)	->
	{ok, Request#request{code = 200,
						 response_data = {ok, nao_implementado}}
	}.

update(Request)	->
	{ok, Request#request{code = 200,
						 response_data = {ok, nao_implementado}}
	}.

delete(Request)	->
	{ok, Request#request{code = 200,
						 response_data = {ok, nao_implementado}}
	}.
	
