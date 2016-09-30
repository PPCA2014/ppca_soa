%%********************************************************************
%% @title Module ems_api_query_mnesia
%% @version 1.0.0
%% @doc It provides api query functions for odbc database.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_mnesia).

-export([find/7, find_by_id/4, insert/3, update/4]).

-include("../../include/ems_schema.hrl").


find(FilterJson, Fields, Limit, Offset, Sort, Datasource = #service_datasource{table_name = TableName}, _Debug) ->
	case ems_api_query_mnesia_parse:generate_dynamic_query(FilterJson, Fields, Datasource, Limit, Offset, Sort) of
		{ok, {FieldList, FilterList, LimitSmnt}} -> 
			TableName2 = list_to_atom(TableName),
			ems_db:find(TableName2, FieldList, FilterList, Limit, Offset);
		{error, Reason} -> {error, Reason}
	end.


find_by_id(Id, Fields, Datasource = #service_datasource{table_name = TableName}, Debug) ->
	case ems_api_query_mnesia_parse:generate_dynamic_query(Id, Fields, Datasource) of
		{ok, FieldList} -> 
			TableName2 = list_to_atom(TableName),
			ems_db:find_by_id(TableName2, Id, FieldList);
		{error, Reason} -> {error, Reason}
	end.


insert(Payload, Service = #service{schema_in = Schema}, #service_datasource{table_name = TableName}) -> 
	io:format("aqui1 -> ~p\n\n", [Schema]),
	case ems_api_query_validator:validate(Payload, Schema) of
		ok -> 
			io:format("validado"),
			Record = ems_schema:to_record(Payload, list_to_atom(TableName)),
			io:format("record is ~p\n", [Record]),
			case ems_db:insert(Record) of
				{ok, Record2 } -> Record2;
				Error -> Error
			end;
		Error -> Error
	end.


update(Id, Payload, Service = #service{schema_in = Schema}, #service_datasource{table_name = TableNameStr}) -> 
	TableName = list_to_atom(TableNameStr),
	case ems_db:get(TableName, Id) of
		{ok, Record} ->
			Record2 = ems_schema:to_record(Payload, Record),  %% copia os dados do payload para Record
			case ems_api_query_validator:validate(Record2, Schema) of
				ok -> 
					case ems_db:update(Record2) of
						ok -> Record2;
						Error -> Error
					end;
				Error -> Error
			end;
		Error -> Error
	end.

	

