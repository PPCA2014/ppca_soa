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

-export([get/1, insert/1, update/1, all/0, delete/1, find_by_username_and_password/2, find_by_login/1, find_by_name/1]).

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

find_by_username_and_password(Username, Password) ->
	Query = fun() ->
		  qlc:e(
			 qlc:q([R || R <- mnesia:table(user), 
						 R#user.name == Username,
						 R#user.password == Password])
		  )
	   end,
	case mnesia:transaction(Query) of
		{atomic, []} -> {error, enoent};
		{atomic, [Record|_]} -> {ok, Record};
		{aborted, _Reason} -> {error, aborted}
	end.


find_by_login(Login) when is_list(Login) ->	
	find_by_login(list_to_binary(Login));
find_by_login(Login) ->
	case mnesia:dirty_index_read(user, Login, #user.login) of
		[] -> {error, enoent};
		[Record|_] -> {ok, Record}
	end.

	
find_by_name(Name) when is_list(Name) ->
	find_by_name(list_to_binary(Name));
find_by_name(Name) ->
	Query = fun() ->
		  qlc:e(
			 qlc:q([R || R <- mnesia:table(user), 
						 R#user.name == Name])
		  )
	   end,
	case mnesia:transaction(Query) of
		{atomic, []} -> {error, enoent};
		{atomic, [Record|_]} -> {ok, Record};
		{aborted, _Reason} -> {error, aborted}
	end.
	
