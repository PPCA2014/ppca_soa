%%********************************************************************
%% @title Módule ems_schema
%% @version 1.0.0
%% @doc Module ems_schema
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_schema).

-compile({parse_transform, exprecs}).

-include("include/ems_schema.hrl").

-export([to_record/2, to_list/1, to_list/2, to_json/1, new/1, new_/1, prop_list_to_json/1]).

-export_records([user, user_permission, user_perfil, 
				 user_email, user_dados_funcionais, user_endereco, user_telefone,
				 catalog_schema, schema_type, produto, service, service_owner, 
				 client, service_datasource, stat_counter_hist]).


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
to_record(DestRecord, Source) when is_tuple(DestRecord) ->
	ListRec = to_list(DestRecord),
	MapRec = maps:from_list(ListRec),
	to_record(MapRec, Source);
to_record(_, _) -> erlang:error(einvalid_to_record).


% to_list (all fields)
to_list(Record) when is_tuple(Record)-> 
	try
		{struct, Result} = json_rec:to_json(Record, ?MODULE),
		Result2 = [case V of
						<<"undefined">> -> {K, null};
						_ -> {K, V} 
				   end || {K,V} <- Result],
		Result2
	catch 
		_Exception:invalid_string -> 
			ems_logger:warn("ems_schema to_list invalid_string: ~p.", [Record]),
			[];
		_Exception:_Reason -> to_list_tuple(Record)
	end;
to_list(Type) when is_atom(Type)-> 
	NewRecord = new(Type),
	to_list(NewRecord);
to_list(ListRecord) when is_list(ListRecord)-> 
    F = fun(Record) -> 
		{struct, Result} = json_rec:to_json(Record, ?MODULE),
		Result
	end,
	lists:map(F, ListRecord);
to_list(_) -> erlang:error(einvalid_to_list).


to_list_tuple(Tuple) ->
	T2 = ems_util:tuple_to_binlist(Tuple),
	to_list_tuple(T2, []).


to_list_tuple([], L) ->	L;	
to_list_tuple([F|[V|T]], L) ->	
	to_list_tuple(T, [{F, V} | L]).


% to_list (selective fields)
to_list(Record, FieldList) when is_tuple(Record) -> 
	Record2 = to_list(Record),
	[X || X <- Record2, lists:member(element(1, X), FieldList)];
to_list(ListRecord, FieldList) -> 
	to_list(ListRecord, FieldList, []).

to_list([], _, Result) -> Result;
to_list([H|T], FieldList, Result) ->
	List = to_list(H, FieldList),
	to_list(T, FieldList, [List | Result]).



% to_json
-spec to_json(tuple() | map() | list(map()) | list() | binary()) -> binary().
to_json(Value) when is_tuple(Value) -> 
	ListTuple = to_list(Value),
	iolist_to_binary([<<"{"/utf8>>, to_json_rec(ListTuple, []), <<"}"/utf8>>]);
to_json(Value) when is_map(Value) -> 
	ems_util:json_encode(Value);
to_json([Map|_] = Value) when is_map(Map) -> 
	ems_util:json_encode(Value);
to_json(Value) when is_list(Value) -> 
	iolist_to_binary([<<"["/utf8>>, to_json_list(Value, []), <<"]"/utf8>>]);
to_json(Value) -> 
	Value.

	
to_json_rec([], L) -> lists:reverse(L);
to_json_rec([{F, V}|[]], L) -> 
    to_json_rec([], [[<<"\""/utf8>>, F, <<"\""/utf8>>, <<":"/utf8>>, to_value(V)] | L]);
to_json_rec([{F, V}|T], L) -> 
    to_json_rec(T, [[<<"\""/utf8>>, F, <<"\""/utf8>>, <<":"/utf8>>, to_value(V), <<","/utf8>>] | L]).

	
to_json_list([], L) ->	lists:reverse(L);
to_json_list([H|[]], L) ->
  to_json_list([], [to_json(H) | L]);
to_json_list([H|T], L) ->
	to_json_list(T, [[to_json(H), <<","/utf8>>] | L]).


to_value(<<>>) -> <<"null"/utf8>>;
to_value([]) -> <<"null"/utf8>>;
to_value(null) -> <<"null"/utf8>>;
to_value(undefined) -> <<"null"/utf8>>;
to_value("0.0") -> <<"0.0"/utf8>>;
to_value(true) -> <<"true"/utf8>>;
to_value(false) -> <<"false"/utf8>>;
to_value(<<"undefined">>) -> <<"null"/utf8>>;
to_value("undefined") -> <<"null"/utf8>>;
to_value(Data = {{_,_,_},{_,_,_}}) -> 
	ems_util:date_to_string(Data);
to_value(Value) when is_float(Value) -> list_to_binary(mochinum:digits(Value));
to_value(Value) when is_integer(Value) -> integer_to_binary(Value);
to_value(Value) when is_atom(Value) -> 
	[<<"\""/utf8>>, atom_to_binary(Value, utf8), <<"\""/utf8>>];
to_value(Value) when is_binary(Value) -> 
	[<<"\""/utf8>>, Value, <<"\""/utf8>>];
to_value([<<Key/binary>>, <<Value/binary>>]) -> 
	[<<"{\""/utf8>>, Key, <<"\":\""/utf8>>, Value, <<"\"}"/utf8>>];
to_value(Value) when is_list(Value) -> 
	case io_lib:printable_list(Value) of 
		true ->	
			ems_util:json_field_strip_and_escape(ems_util:utf8_list_to_string(Value));
		_ -> 
			to_json(list_to_tuple(Value))
	end;
to_value(Value) when is_map(Value) ->
	ems_util:json_encode(Value);
to_value(Value) when is_tuple(Value) ->	
	to_json(Value);
to_value(_) -> <<"null"/utf8>>.


prop_list_to_json(PropList) -> 
	Result = to_json_rec(PropList, []),
	iolist_to_binary([<<"{"/utf8>>, Result, <<"}"/utf8>>]).
	

new(service) -> #service{};
new(catalog) -> #service{};
new(service_owner) -> #service_owner{};
new(service_datasource) -> #service_datasource{};
new(catalog_schema) -> #catalog_schema{};
new(user) -> #user{};
new(user_permission) -> #user_permission{};
new(user_perfil) -> #user_perfil{};
new(user_email) -> #user_email{};
new(user_endereco) -> #user_endereco{};
new(user_telefone) -> #user_telefone{};
new(user_dados_funcionais) -> #user_dados_funcionais{};
new(schema_type) -> #schema_type{};
new(produto) -> #produto{};
new(client) -> #client{};
new(stat_counter_hist) -> #stat_counter_hist{};
new(_) -> erlang:error(einvalid_type).

new_(service) -> #service{_ = '_'};
new_(catalog) -> #service{_ = '_'};
new_(service_owner) -> #service_owner{_ = '_'};
new_(service_datasource) -> #service_datasource{_ = '_'};
new_(catalog_schema) -> #catalog_schema{_ = '_'};
new_(user) -> #user{_ = '_'};
new_(user_permission) -> #user_permission{_ = '_'};
new_(user_email) -> #user_email{_ = '_'};
new_(user_endereco) -> #user_endereco{_ = '_'};
new_(user_telefone) -> #user_telefone{_ = '_'};
new_(user_dados_funcionais) -> #user_dados_funcionais{_ = '_'};
new_(user_perfil) -> #user_perfil{_ = '_'};
new_(client) -> #client{_ = '_'};
new_(stat_counter_hist) -> #stat_counter_hist{_ = '_'};
new_(_) -> erlang:error(einvalid_type).
  
	
    



