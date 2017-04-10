%%********************************************************************
%% @title Module ems_client
%% @version 1.0.0
%% @doc client class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_client).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([insert/1, update/1, all/0, delete/1, 
		 find_by_id/1,		 
		 find_by_codigo/1,
		 find_by_name/1]).

find_by_id(Id) -> ems_db:get(client, Id).

insert(Client) -> 
	case valida(Client, insert) of
		ok -> ems_db:insert(Client);
		Error -> 
			Error
	end.

update(Client) -> 
	case valida(Client, update) of
		ok -> ems_db:update(Client);
		Error -> Error
	end.

all() -> ems_db:all(client).

delete(Id) -> ems_db:delete(client, Id).

valida(_Client, _Operation) -> ok.


-spec find_by_codigo(binary() | list() | integer()) -> #client{} | {error, enoent}.
find_by_codigo(Codigo) when is_integer(Codigo) ->
	find_by_codigo(erlang:integer_to_binary(Codigo));
find_by_codigo(Codigo) when is_list(Codigo) ->
	find_by_codigo(list_to_binary(Codigo));
find_by_codigo(Codigo) ->
	case mnesia:dirty_index_read(client, Codigo, #client.codigo) of
		[] -> {error, enoent};
		[Record|_] -> {ok, Record}
	end.



find_by_name(Name) -> ems_db:find_first(client, [{"name", "==", Name}]).
	
