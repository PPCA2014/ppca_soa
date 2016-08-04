%%********************************************************************
%% @title Módule ems_schema
%% @version 1.0.0
%% @doc Module ems_schema
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_schema).

-compile({parse_transform, exprecs}).

-include("../include/ems_schema.hrl").

-export([to_record/2, to_list/1, to_json/1, new/1]).

-export_records([user, catalog_schema, schema_type]).


% to_record
to_record(Json, Type) when is_binary(Json), is_atom(Type) ->
	{ok, JsonMap} = ems_util:json_decode(Json),
	JsonStruct = {struct, JsonMap},
	NewRecord = new(Type),
	json_rec:to_rec(JsonStruct, ?MODULE, NewRecord);

to_record(Json, Record) when is_binary(Json), is_tuple(Record)->
	{ok, JsonMap} = ems_util:json_decode(Json),
	JsonStruct = {struct, JsonMap},
	json_rec:to_rec(JsonStruct, ?MODULE, Record);

to_record(Map, Type) when is_map(Map), is_atom(Type) ->
	List = maps:to_list(Map),
	JsonStruct = {struct, List},
	NewRecord = new(Type),
	json_rec:to_rec(JsonStruct, ?MODULE, NewRecord);

to_record(Map, Record) when is_map(Map), is_tuple(Record)->
	List = maps:to_list(Map),
	JsonStruct = {struct, List},
	json_rec:to_rec(JsonStruct, ?MODULE, Record);

to_record(_, _) -> erlang:error(einvalid_to_record).


% to_list
to_list(Record) when is_tuple(Record)-> 
	{struct, Result} = json_rec:to_json(Record, ?MODULE),
	Result;
to_list(Type) when is_atom(Type)-> 
	NewRecord = new(Type),
	{struct, Result} = json_rec:to_json(NewRecord, ?MODULE),
	Result;
to_list(ListRecord) when is_list(ListRecord)-> 
    F = fun(Record) -> 
		{struct, Result} = json_rec:to_json(Record, ?MODULE),
		Result
	end,
	lists:map(F, ListRecord);
to_list(_) -> erlang:error(einvalid_to_list).


% to_json
to_json(Record) when is_tuple(Record)-> 
	ListTuple = to_list(Record),
	to_json_rec(ListTuple, []);
to_json(List) when is_list(List) -> 
	Dados = lists:flatten(to_json_list(List, [])),
	io_lib:format(<<"{~p}">>, [Dados]).

	
to_json_rec([], L) -> lists:flatten(lists:reverse(L));
to_json_rec([{F, V}|[]], L) -> 
    to_json_rec([], [io_lib:format(<<"~p:~p"/utf8>>, [F, V]) | L]);
to_json_rec([{F, V}|T], L) -> 
    to_json_rec(T, [io_lib:format(<<"~p:~p,"/utf8>>, [F, V]) | L]).
	
to_json_list([], L) ->	L;
to_json_list([H|T], L) ->
  to_json_list(T, [to_json(H) | L]).

new(catalog_schema) -> #catalog_schema{};
new(user) -> #user{};
new(schema_type) -> #schema_type{};
new(_) -> erlang:raise(einvalid_type).
  
	
    



