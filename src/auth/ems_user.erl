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

-export([get/1, insert/1, update/1, all/0, delete/1, find_by_username_and_password/2]).

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

valida(User, insert) ->
	case ems_consist:mensagens([ems_consist:msg_campo_obrigatorio("name", User#user.name),
								   ems_consist:msg_campo_obrigatorio("email", User#user.email),
								   ems_consist:msg_campo_obrigatorio("password", User#user.password)]) of
		[] -> 
			case ems_consist:msg_email_invalido("email", User#user.email) of
				[] ->
					case ems_consist:msg_registro_ja_existe({user, '_', User#user.name, '_', '_'}, 
																<<"O name do usuário já está cadastrado."/utf8>>) of
						[] -> 
							case ems_consist:msg_registro_ja_existe({user, '_', '_', User#user.email, '_'}, 
																	    <<"O email do usuário já está cadastrado."/utf8>>) of
								[] -> ok; 
								Msg -> {error, Msg}
							end;
						Msg -> {error, Msg}
					end;
				Msg -> {error, Msg}
			end;
		Msgs -> {error, Msgs}
	end;

valida(User, update) ->	valida(User, insert);

valida(_User, delete) -> ok.	

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

	
