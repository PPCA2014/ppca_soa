%%********************************************************************
%% @title Module ems_user
%% @version 1.0.0
%% @doc user permission class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_permission).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

-export([find_by_id/1, find_by_hash/1, find_by_hash2/1, make_hash/2, has_grant_permission/3]).


find_by_id(Id) -> ems_db:get(user_permission, Id).

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
				"GET" -> GrantGet == true;
				"POST" -> GrantPost == true;
				"PUT" -> GrantPut == true;
				"DELETE" -> GrantDelete == true
			end;
		_ -> false
	end.

