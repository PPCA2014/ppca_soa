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

-export([find_by_id/1, find_by_hash/1, make_hash/2]).


find_by_id(Id) -> ems_db:get(user_permission, Id).

find_by_hash(Hash) ->
	case mnesia:dirty_index_read(user_permission, Hash, #user_permission.hash) of
		[] -> {error, enoent};
		[Record] -> {ok, Record}
	end.

make_hash(Rowid, CodigoPessoa) -> erlang:phash2([Rowid, CodigoPessoa]).
