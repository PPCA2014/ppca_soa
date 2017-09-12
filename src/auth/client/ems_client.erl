%%********************************************************************
%% @title Module ems_client
%% @version 1.0.0
%% @doc client class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_client).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([insert/1, update/1, all/0, delete/1, 
		 find_by_id/1,		 
		 find_by_codigo/1,
		 find_by_name/1,
		 find_by_codigo_and_secret/2,
		 to_json/1]).

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
find_by_codigo(Codigo) when is_binary(Codigo) ->
	find_by_codigo(ems_util:binary_to_integer(Codigo));
find_by_codigo(Codigo) when is_list(Codigo) ->
	find_by_codigo(list_to_integer(Codigo));
find_by_codigo(Codigo) ->
	io:format("aqui3 ~p\n", [Codigo]),
	case mnesia:dirty_index_read(client, Codigo, #client.codigo) of
		[] -> {error, enoent};
		[Record|_] -> 
			io:format("aqui4 ~p\n", [Record]),
			{ok, Record}
	end.
	

find_by_codigo_and_secret(Codigo, Secret) ->
	case find_by_codigo(Codigo) of
		{ok, Client = #client{secret = CliSecret}} -> 
			io:format("aqui5 secret ~p  secret2 ~p\n", [CliSecret, Secret]),
			case CliSecret =:= Secret orelse CliSecret =:= ems_util:criptografia_sha1(Secret)  of
				true -> {ok, Client};
				false -> {error, enoent}
			end;
		_ -> {error, enoent}
	end.



find_by_name(Name) -> ems_db:find_first(client, [{"name", "==", Name}]).

to_json(undefined) -> <<"{}"/utf8>>;
to_json(Client) ->
	iolist_to_binary([
		<<"{"/utf8>>,
			<<"\"id\""/utf8>>, <<":"/utf8>>, integer_to_binary(Client#client.id), <<","/utf8>>,
			<<"\"codigo\""/utf8>>, <<":"/utf8>>, integer_to_binary(Client#client.codigo), <<","/utf8>>,
			<<"\"description\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Client#client.description, <<"\""/utf8>>, 
		<<"}"/utf8>>
		]).

	
