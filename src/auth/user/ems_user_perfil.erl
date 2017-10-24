%%********************************************************************
%% @title Module ems_user_perfil
%% @version 1.0.0
%% @doc user_perfil class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_perfil).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([all/0, 
		 find_by_id/1,		 
		 find_by_codigo/1,
		 find_by_name/1, 
 		 new_from_map/2,
		 new_from_map/3,
		 get_table/1,
		 find/2,
		 all/1]).


-spec find_by_id(non_neg_integer()) -> {ok, #user_perfil{}} | {error, enoent}.
find_by_id(Id) -> 
	case ems_db:get(user_perfil_db, Id) of
		{ok, Record} -> {ok, setelement(1, Record, user_perfil)};
		_ -> case ems_db:get(user_perfil_fs, Id) of
				{ok, Record} -> {ok, setelement(1, Record, user_perfil)};
				_ -> {error, enoent}
			 end
	end.

-spec all() -> {ok, list()}.
all() -> 
	{ok, ListaUserDb} = ems_db:all(user_perfil_db),
	{ok, ListaUserFs} = ems_db:all(user_perfil_fs),
	{ok, ListaUserDb ++ ListaUserFs}.
	


-spec find_by_codigo(non_neg_integer()) -> {ok, #user_perfil{}} | {error, enoent}.
find_by_codigo(Codigo) when is_binary(Codigo) ->
	find_by_codigo(ems_util:binary_to_integer(Codigo));
find_by_codigo(Codigo) when is_list(Codigo) ->
	find_by_codigo(list_to_integer(Codigo));
find_by_codigo(Codigo) ->
	case mnesia:dirty_index_read(user_perfil_db, Codigo, #user_perfil.codigo) of
		[] -> case mnesia:dirty_index_read(user_perfil_fs, Codigo, #user_perfil.codigo) of
				[] -> {error, enoent};
				[Record|_] -> {ok, setelement(1, Record, user_perfil)}
			  end;
		[Record|_] -> {ok, setelement(1, Record, user_perfil)}
	end.



-spec find_by_name(binary() | string()) -> {ok, #user_perfil{}} | {error, enoent}.
find_by_name(<<>>) -> {error, enoent};
find_by_name("") -> {error, enoent};
find_by_name(undefined) -> {error, enoent};
find_by_name(Name) when is_list(Name) -> 
	find_by_name(list_to_binary(Name));
find_by_name(Name) -> 
	case ems_db:find_first(user_perfil_db, [{name, "==", Name}]) of
		{error, Reason} ->
			case ems_db:find_first(user_perfil_fs, [{name, "==", Name}]) of
				{error, Reason} -> {error, enoent};
				Record -> {ok, setelement(1, Record, user_perfil)}
			end;
		Record -> {ok, setelement(1, Record, user_perfil)}
	end.


-spec new_from_map(map(), #config{}) -> {ok, #user_perfil{}} | {error, atom()}.
new_from_map(Map, Conf) -> new_from_map(Map, Conf, undefined).
new_from_map(Map, _Conf, Id) ->
	try
		{ok, #user_perfil{id = Id,
						  codigo = maps:get(<<"codigo">>, Map, undefined),
						  codigo_usuario = maps:get(<<"codigo_usuario">>, Map, undefined),
						  codigo_cliente = maps:get(<<"codigo_cliente">>, Map, undefined),
						  name = ?UTF8_STRING(maps:get(<<"name">>, Map, <<>>)),
						  ctrl_path = maps:get(<<"ctrl_path">>, Map, <<>>),
						  ctrl_file = maps:get(<<"ctrl_file">>, Map, <<>>),
						  ctrl_modified = maps:get(<<"ctrl_modified">>, Map, undefined),
						  ctrl_hash = erlang:phash2(Map)
			}
		}
	catch
		_Exception:Reason -> 
			ems_logger:format_warn("ems_user parse invalid user_perfil specification: ~p\n\t~p.\n", [Reason, Map]),
			{error, Reason}
	end.


-spec get_table(fs | db) -> user_perfil_db | user_perfil_fs.
get_table(db) -> user_perfil_db;
get_table(fs) -> user_perfil_fs.

-spec find(user_perfil_fs | user_perfil_db, non_neg_integer()) -> {ok, #user_perfil{}} | {error, atom()}.
find(Table, Codigo) ->
	case ems_db:find_first(Table, [{codigo, "==", Codigo}]) of
		{error, Reason} -> {error, Reason};
		Record -> {ok, setelement(1, Record, user_perfil)}
	end.

-spec all(user_perfil_fs | user_perfil_db) -> list() | {error, atom()}.
all(Table) ->
	case ems_db:all(Table) of
		{ok, Records} -> 
			Records2 = [setelement(1, R, user_perfil) || R <- Records],
			{ok, Records2};
		Error -> Error
	end.

