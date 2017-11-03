%%********************************************************************
%% @title Module ems_api_query_mnesia
%% @version 1.0.0
%% @doc It provides api query functions for odbc database.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_mnesia).

-export([find/6, find_by_id/3, find_by_owner/6, insert/3, update/4, delete/3]).

-include("include/ems_schema.hrl").


find(FilterJson, Fields, Limit, Offset, Sort, Datasource = #service_datasource{table_name = TableName, table_name2 = TableName2}) ->
	case ems_api_query_mnesia_parse:generate_dynamic_query(FilterJson, Fields, Datasource, Limit, Offset, Sort) of
		{ok, {FieldList, FilterList, _LimitSmnt}} -> 
			TableNameAtom = list_to_atom(TableName),
			Result1 = ems_db:find(TableNameAtom, FieldList, FilterList, Limit, Offset),
			case TableName2 =/= "" of
				true ->
					TableName2Atom = list_to_atom(TableName2),
					Result2 = ems_db:find(TableName2Atom, FieldList, FilterList, Limit, Offset),
					ResultJson = ems_schema:to_json(Result1 ++ Result2);
				false ->
					ResultJson = ems_schema:to_json(Result1)
			end,
			{ok, ResultJson};
		{error, Reason} -> {error, Reason}
	end.


find_by_id(Id, Fields, Datasource = #service_datasource{table_name = TableName, table_name2 = TableName2}) ->
	case ems_api_query_mnesia_parse:generate_dynamic_query(Id, Fields, Datasource) of
		{ok, FieldList} -> 
			TableNameAtom = list_to_atom(TableName),
			Result1 = ems_db:find_by_id(TableNameAtom, Id, FieldList),
			case Result1 == [] of
				true ->
					TableName2Atom = list_to_atom(TableName2),
					Result2 = ems_db:find_by_id(TableName2Atom, Id, FieldList),
					ResultJson = ems_schema:to_json(Result2);
				false ->
					ResultJson = ems_schema:to_json(Result1)
			end,
			{ok, ResultJson};
		{error, Reason} -> {error, Reason}
	end.

find_by_owner(FilterJson, Fields, Limit, Offset, Sort, IdOwner, Datasource = #service_datasource{table_name = TableName, table_name2 = TableName2, foreign_key = ForeignKey}) ->
	case ems_api_query_mnesia_parse:generate_dynamic_query(FilterJson, Fields, Datasource, Limit, Offset, Sort) of
		{ok, {FieldList, FilterList, _LimitSmnt}} -> 
			TableNameAtom = list_to_atom(TableName),
			FilterList2 = [{ForeignKey, "==", IdOwner} | FilterList],
			Result1 = ems_db:find(TableNameAtom, FieldList, FilterList2, Limit, Offset),
			case TableName2 =/= "" of
				true ->
					TableName2Atom = list_to_atom(TableName2),
					Result2 = ems_db:find(TableName2Atom, FieldList, FilterList, Limit, Offset),
					ResultJson = ems_schema:to_json(Result1 ++ Result2);
				false ->
					ResultJson = ems_schema:to_json(Result1)
			end,
			{ok, ResultJson};
		{error, Reason} -> {error, Reason}
	end.



insert(Payload, Service = #service{schema_in = Schema}, #service_datasource{table_name = TableName}) -> 
	case ems_api_query_validator:validate(Payload, Schema) of
		ok -> 
			Record = ems_schema:to_record(Payload, list_to_atom(TableName)),
			case onvalidate(insert, Record, Service) of
				ok ->
					case ems_db:insert(Record) of
						{ok, Result} -> 
							ResultJson = ems_schema:to_json(Result),
							{ok, ResultJson};
						Error -> Error
					end;
				Error2 -> 
					Error2
			end;
		Error -> Error
	end.


update(Id, Payload, Service = #service{schema_in = Schema}, #service_datasource{table_name = TableNameStr}) -> 
	TableName = list_to_atom(TableNameStr),
	case ems_db:get(TableName, Id) of
		{ok, Record} ->
			Record2 = ems_schema:to_record(Payload, Record),  %% copia os dados do payload para o Record
			case ems_api_query_validator:validate(Record2, Schema) of
				ok -> 
					case onvalidate(update, Record2, Service) of
						ok ->
							case ems_db:update(Record2) of
								ok -> 
									ResultJson = ems_schema:to_json(Record2),
									{ok, ResultJson};
								Error -> Error
							end;
						Error2 -> Error2
					end;
				Error -> Error
			end;
		Error -> Error
	end.


delete(Id, _Service, #service_datasource{table_name = TableNameStr}) -> 
	TableName = list_to_atom(TableNameStr),
	case ems_db:delete(TableName, Id) of
		ok -> {ok, <<"{\"ok\", true}">>};
		Error -> ems_util:json_encode(Error)
	end.


onvalidate(_, _, #service{middleware = undefined}) -> ok;
onvalidate(Operation, Record, #service{middleware = Middleware}) ->
	case code:ensure_loaded(Middleware) of
		{module, _} ->
			case erlang:function_exported(Middleware, onvalidate, 2) of
				true -> apply(Middleware, onvalidate, [Operation, Record]);
				false -> ok
			end;
		{error, Reason} -> {
			error, {Reason, Middleware}}
	end.
			




