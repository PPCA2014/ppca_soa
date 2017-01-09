%%********************************************************************
%% @title Module ems_user
%% @version 1.0.0
%% @doc Manages information about users
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([get/1, insert/1, update/1, all/0, delete/1, 
		 authenticate_login_password/2, 
		 find_by_login/1, 
		 find_by_name/1, 
		 find_by_login_and_password/2]).

get(Id) -> ems_db:get(user, Id).

insert(User) -> 
	case valida(User, insert) of
		ok -> ems_db:insert(User);
		Error -> 
			Error
	end.

update(User) -> 
	case valida(User, update) of
		ok -> ems_db:update(User);
		Error -> Error
	end.

all() -> ems_db:all(user).

delete(Id) -> ems_db:delete(user, Id).

valida(_User, _Operation) -> ok.

authenticate_login_password(Login, Password) ->
	case find_by_login(Login) of
		{ok, #user{password = PasswordUser}} -> 
			PasswordUser =:= ems_util:criptografia_sha1(Password);
		_ -> false
	end.

find_by_login(Login) when is_list(Login) ->	
	find_by_login(list_to_binary(Login));
find_by_login(Login) ->
	case mnesia:dirty_index_read(user, Login, #user.login) of
		[] -> {error, enoent};
		[Record|_] -> {ok, Record}
	end.

find_by_name(Name) -> ems_db:find_first(user, [{"name", "==", Name}]).
	
find_by_login_and_password(Login, Password) ->
	case find_by_login(Login) of
		{ok, User = #user{password = PasswordUser}} -> 
			case PasswordUser =:= ems_util:criptografia_sha1(Password) of
				true -> {ok, User};
				false -> {error, enoent}
			end;
		_ -> {error, enoent}
	end.
