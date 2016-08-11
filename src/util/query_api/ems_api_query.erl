%%********************************************************************
%% @title Module ems_api_query
%% @version 1.0.0
%% @doc It provides API query functions
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query).

-export([find/7, find_by_id/4]).

-include("../../../include/ems_schema.hrl").


find(FilterJson, Fields, Limit, Offset, Sort, Datasource = #service_datasource{type = ConnType}, Debug) ->
	case ConnType of
		sqlserver -> ems_api_query_sqlserver:find(FilterJson, Fields, Limit, Offset, Sort, Datasource, Debug);
		sqlite -> ems_api_query_sqlite:find(FilterJson, Fields, Limit, Offset, Sort, Datasource, Debug);
		mnesia -> ems_api_query_mnesia:find(FilterJson, Fields, Limit, Offset, Sort, Datasource, Debug);
		_ -> erlang:raise(einvalid_datasource)
	end.


find_by_id(Id, Fields, Datasource =  #service_datasource{type = ConnType}, Debug) ->
	case ConnType of
		sqlserver -> ems_api_query_sqlserver:find_by_id(Id, Fields, Datasource, Debug);
		sqlite -> ems_api_query_sqlite:find_by_id(Id, Fields, Datasource, Debug);
		mnesia -> ems_api_query_mnesia:find_by_id(Id, Fields, Datasource, Debug);
		_ -> erlang:raise(einvalid_datasource)
	end.
		
		

