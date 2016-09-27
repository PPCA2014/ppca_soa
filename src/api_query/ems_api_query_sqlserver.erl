%%********************************************************************
%% @title Module ems_api_query_sqlserver
%% @version 1.0.0
%% @doc It provides api query functions for odbc database.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_sqlserver).

-export([find/7, find_by_id/4]).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").


find(FilterJson, Fields, Limit, Offset, Sort, Datasource, Debug) ->
	case ems_api_query_sqlserver_parse:generate_dynamic_query(FilterJson, Fields, Datasource, Limit, Offset, Sort) of
		{ok, {Sql, Params}} -> ems_api_query_sqlserver_parse:execute_dynamic_query(Sql, Params, Datasource, Debug);
		{error, Reason} -> {error, Reason}
	end.


find_by_id(Id, Fields, Datasource, Debug) ->
	case ems_api_query_sqlserver_parse:generate_dynamic_query(Id, Fields, Datasource) of
		{ok, {Sql, Params}} -> ems_api_query_sqlserver_parse:execute_dynamic_query(Sql, Params, Datasource, Debug);
		{error, Reason} -> {error, Reason}
	end.

	    
