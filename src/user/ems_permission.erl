%%********************************************************************
%% @title Module ems_user
%% @version 1.0.0
%% @doc Manages information about users
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_permission).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([get/1, insert/1, update/1, all/0, delete/1, 
		 find_by_login/1 ]).

get(Id) -> ems_db:get(permission, Id).

insert(Permission) -> 
	case valida(Permission, insert) of
		ok -> ems_db:insert(Permission);
		Error -> 
			Error
	end.

update(Permission) -> 
	case valida(Permission, update) of
		ok -> ems_db:update(Permission);
		Error -> Error
	end.

all() -> ems_db:all(permission).

delete(Id) -> ems_db:delete(permission, Id).

valida(_Permission, _Operation) -> ok.


-spec find_by_login(binary()) -> #permission{} | {error, enoent}.
find_by_login(<<>>) -> {error, enoent};	
find_by_login("") -> {error, enoent};	
find_by_login(Login) when is_list(Login) ->	find_by_login(list_to_binary(Login));
find_by_login(Login) ->
	case mnesia:dirty_index_read(permission, Login, #permission.login) of
		[] -> 
			[];
		[Record|_] -> {ok, Record}
	end.

