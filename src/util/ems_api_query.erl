%%********************************************************************
%% @title Module ems_api_query
%% @version 1.0.0
%% @doc It provides API query functions
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query).

-export([find/8, find_by_id/6]).


find(FilterJson, Fields, TableName, Limit, Offset, Sort, Conn, Debug) ->
	ems_api_query_odbc:find(FilterJson, Fields, TableName, Limit, Offset, Sort, Conn, Debug).


find_by_id(Id, Fields, TableName, PrimaryKey, Conn, Debug) ->
	ems_api_query_odbc:find_by_id(Id, Fields, TableName, PrimaryKey, Conn, Debug).

