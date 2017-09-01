%%********************************************************************
%% @title Module ems_user
%% @version 1.0.0
%% @doc user perfil
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_perfil).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

-export([find_by_id/1, find_by_user/1]).


find_by_id(Id) -> ems_db:get(user_perfil, Id).

find_by_user(#user{codigo = UserId}) ->
	case mnesia:dirty_index_read(user_perfil, UserId, #user_perfil.user_id) of
		[] -> {ok, []};
		Records -> {ok, Records}
	end;
find_by_user(_) -> {ok, []}.

