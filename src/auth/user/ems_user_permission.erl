%%********************************************************************
%% @title Module ems_user_permission
%% @version 1.0.0
%% @doc user_permission class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_permission).

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
		 all/1,
		 find_by_hash/1, find_by_hash2/1, make_hash/2, has_grant_permission/3]).


-spec find_by_id(non_neg_integer()) -> {ok, #user_permission{}} | {error, enoent}.
find_by_id(Id) -> 
	case ems_db:get(user_permission_db, Id) of
		{ok, Record} -> {ok, Record};
		_ -> case ems_db:get(user_permission_fs, Id) of
				{ok, Record} -> {ok, Record};
				_ -> {error, enoent}
			 end
	end.

-spec all() -> {ok, list()}.
all() -> 
	{ok, ListaUserDb} = ems_db:all(user_permission_db),
	{ok, ListaUserFs} = ems_db:all(user_permission_fs),
	{ok, ListaUserDb ++ ListaUserFs}.
	


-spec find_by_codigo(non_neg_integer()) -> {ok, #user_permission{}} | {error, enoent}.
find_by_codigo(Codigo) when is_binary(Codigo) ->
	find_by_codigo(ems_util:binary_to_integer(Codigo));
find_by_codigo(Codigo) when is_list(Codigo) ->
	find_by_codigo(list_to_integer(Codigo));
find_by_codigo(Codigo) ->
	case mnesia:dirty_index_read(user_permission_db, Codigo, #user_permission.codigo) of
		[] -> case mnesia:dirty_index_read(user_permission_fs, Codigo, #user_permission.codigo) of
				[] -> {error, enoent};
				[Record|_] -> {ok, Record}
			  end;
		[Record|_] -> {ok, Record}
	end.



-spec find_by_name(binary() | string()) -> {ok, #user_permission{}} | {error, enoent}.
find_by_name(<<>>) -> {error, enoent};
find_by_name("") -> {error, enoent};
find_by_name(undefined) -> {error, enoent};
find_by_name(Name) when is_list(Name) -> 
	find_by_name(list_to_binary(Name));
find_by_name(Name) -> 
	case ems_db:find_first(user_permission_db, [{name, "==", Name}]) of
		{error, Reason} ->
			case ems_db:find_first(user_permission_fs, [{name, "==", Name}]) of
				{error, Reason} -> {error, enoent};
				Record -> {ok, Record}
			end;
		Record -> {ok, Record}
	end.


-spec new_from_map(map(), #config{}) -> {ok, #user_permission{}} | {error, atom()}.
new_from_map(Map, Conf) -> new_from_map(Map, Conf, undefined).
new_from_map(Map, _Conf, Id) ->
	try
		{ok, #user_permission{id = Id,
							  codigo = maps:get(<<"codigo">>, Map, undefined),
							  codigo_usuario = maps:get(<<"codigo_usuario">>, Map, undefined),
							  codigo_cliente = maps:get(<<"codigo_cliente">>, Map, undefined),
							  url = ?UTF8_STRING(maps:get(<<"url">>, Map, <<>>)),
							  name = ?UTF8_STRING(maps:get(<<"name">>, Map, <<>>)),
							  grant_get = ems_util:parse_bool(maps:get(<<"grant_get">>, Map, true)),
							  grant_post = ems_util:parse_bool(maps:get(<<"grant_post">>, Map, true)),
							  grant_put = ems_util:parse_bool(maps:get(<<"grant_put">>, Map, true)),
							  grant_delete = ems_util:parse_bool(maps:get(<<"grant_delete">>, Map, <<>>)),
							  ctrl_path = maps:get(<<"ctrl_path">>, Map, <<>>),
							  ctrl_file = maps:get(<<"ctrl_file">>, Map, <<>>),
							  ctrl_modified = maps:get(<<"ctrl_modified">>, Map, undefined),
							  ctrl_hash = erlang:phash2(Map)
			}
		}
	catch
		_Exception:Reason -> 
			ems_logger:format_warn("ems_user parse invalid user_permission specification: ~p\n\t~p.\n", [Reason, Map]),
			{error, Reason}
	end.


-spec get_table(fs | db) -> user_permission_db | user_permission_fs.
get_table(db) -> user_permission_db;
get_table(fs) -> user_permission_fs.

-spec find(user_permission_fs | user_permission_db, non_neg_integer()) -> {ok, #user_permission{}} | {error, enoent}.
find(Table, Codigo) ->
	case mnesia:dirty_index_read(Table, Codigo, #user_permission.codigo) of
		[] -> {error, enoent};
		[Record|_] -> {ok, Record}
	end.

-spec all(user_permission_fs | user_permission_db) -> list() | {error, atom()}.
all(Table) -> ems_db:all(Table).


find_by_hash(Hash) ->
	case mnesia:dirty_index_read(user_permission, Hash, #user_permission.hash) of
		[] -> {error, enoent};
		[Record] -> {ok, Record}
	end.


find_by_hash2(Hash) ->
	case mnesia:dirty_index_read(user_permission, Hash, #user_permission.hash2) of
		[] -> {error, enoent};
		[Record] -> {ok, Record}
	end.

make_hash(Rowid, PesId) -> erlang:phash2([Rowid, PesId]).

has_grant_permission(#service{oauth2_with_check_constraint = false}, _, _) -> true;
has_grant_permission(#service{oauth2_with_check_constraint = true},
					 #request{rowid = Rowid, type = Type}, 
					 #user{codigo = Codigo}) ->
	Hash = make_hash(Rowid, Codigo),
	case find_by_hash(Hash) of
		{ok, #user_permission{grant_get = GrantGet, 
							  grant_post = GrantPost, 
							  grant_put = GrantPut, 
							  grant_delete = GrantDelete}} ->
			case Type of
				<<"GET">> -> GrantGet == true;
				<<"POST">> -> GrantPost == true;
				<<"PUT">> -> GrantPut == true;
				<<"DELETE">> -> GrantDelete == true
			end;
		_ -> false
	end.

