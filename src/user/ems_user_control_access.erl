%%********************************************************************
%% @title Module ems_user
%% @version 1.0.0
%% @doc user control_access class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_control_access).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

-export([find_by_id/1, update/1, delete/1, all/0, find_by_codigo/1]).


find_by_id(Id) -> ems_db:get(user_control_access, Id).

insert(Access) -> ems_db:insert(Access).

update(Access) -> ems_db:update(Access).
	
all() -> ems_db:all(user_control_access).

delete(Id) -> ems_db:delete(user_control_access, Id).

-spec find_by_codigo(binary() | list() | integer()) -> #user_control_access{} | {error, enoent}.
find_by_codigo(Codigo) when is_integer(Codigo) ->
	find_by_codigo(erlang:integer_to_binary(Codigo));
find_by_codigo(Codigo) when is_list(Codigo) ->
	find_by_codigo(list_to_binary(Codigo));
find_by_codigo(Codigo) ->
	case mnesia:dirty_index_read(user_control_access, Codigo, #user_control_access.codigo) of
		[] -> {error, enoent};
		[Record|_] -> {ok, Record}
	end.


