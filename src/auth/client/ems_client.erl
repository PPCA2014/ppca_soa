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
		 to_json/1,
		 new_from_map/2,
		 new_from_map/3,
		 get_table/1,
		 find/2,
		 all/1]).


-spec find_by_id(non_neg_integer()) -> {ok, #client{}} | {error, enoent}.
find_by_id(Id) -> 
	case ems_db:get(client_db, Id) of
		{ok, Record} -> {ok, setelement(1, Record, client)};
		_ -> case ems_db:get(client_fs, Id) of
				{ok, Record} -> {ok, setelement(1, Record, client)};
				_ -> {error, enoent}
			 end
	end.


-spec all() -> {ok, list()}.
all() -> 
	{ok, ListaUserDb} = ems_db:all(client_db),
	{ok, ListaUserFs} = ems_db:all(client_fs),
	{ok, ListaUserDb ++ ListaUserFs}.


-spec find_by_codigo(non_neg_integer()) -> {ok, #user{}} | {error, enoent}.
find_by_codigo(Codigo) when is_binary(Codigo) ->
	find_by_codigo(ems_util:binary_to_integer(Codigo));
find_by_codigo(Codigo) when is_list(Codigo) ->
	find_by_codigo(list_to_integer(Codigo));
find_by_codigo(Codigo) ->
	case mnesia:dirty_index_read(client_db, Codigo, #client.codigo) of
		[] -> case mnesia:dirty_index_read(client_fs, Codigo, #client.codigo) of
				[] -> {error, enoent};
				[Record|_] -> {ok, setelement(1, Record, client)}
			  end;
		[Record|_] -> {ok, setelement(1, Record, client)}
	end.

	
-spec find_by_codigo_and_secret(non_neg_integer(), binary()) -> {ok, #client{}} | {error, enoent}.
find_by_codigo_and_secret(Codigo, Secret) ->
	case find_by_codigo(Codigo) of
		{ok, Client = #client{secret = CliSecret}} -> 
			case CliSecret =:= Secret orelse CliSecret =:= ems_util:criptografia_sha1(Secret)  of
				true -> {ok, Client};
				false -> {error, enoent}
			end;
		_ -> {error, enoent}
	end.



-spec find_by_name(binary() | string()) -> {ok, #client{}} | {error, enoent}.
find_by_name(<<>>) -> {error, enoent};
find_by_name("") -> {error, enoent};
find_by_name(undefined) -> {error, enoent};
find_by_name(Name) when is_list(Name) -> 
	find_by_name(list_to_binary(Name));
find_by_name(Name) -> 
	case ems_db:find_first(client_db, [{name, "==", Name}]) of
		{error, Reason} ->
			case ems_db:find_first(client_fs, [{name, "==", Name}]) of
				{error, Reason} -> {error, enoent};
				Record -> {ok, setelement(1, Record, client)}
			end;
		Record -> {ok, setelement(1, Record, client)}
	end.


to_json(undefined) -> <<"{}"/utf8>>;
to_json(Client) ->
	iolist_to_binary([
		<<"{"/utf8>>,
			<<"\"id\""/utf8>>, <<":"/utf8>>, integer_to_binary(Client#client.id), <<","/utf8>>,
			<<"\"codigo\""/utf8>>, <<":"/utf8>>, integer_to_binary(Client#client.codigo), <<","/utf8>>,
			<<"\"description\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Client#client.description, <<"\""/utf8>>, 
		<<"}"/utf8>>
		]).

	
-spec new_from_map(map(), #config{}) -> {ok, #client{}} | {error, atom()}.
new_from_map(Map, Conf) -> new_from_map(Map, Conf, undefined).
new_from_map(Map, _Conf, Id) ->
	try
		{ok, #client{id = Id,
				codigo = maps:get(<<"codigo">>, Map, undefined),
				name = ?UTF8_STRING(maps:get(<<"name">>, Map, <<>>)),
				secret = ?UTF8_STRING(maps:get(<<"secret">>, Map, <<>>)),
				redirect_uri = ?UTF8_STRING(maps:get(<<"redirect_uri">>, Map, <<>>)),
				description = ?UTF8_STRING(maps:get(<<"description">>, Map, <<>>)),
				scope = ?UTF8_STRING(maps:get(<<"scope">>, Map, <<>>)),
				active = ems_util:parse_bool(maps:get(<<"active">>, Map, true)),
				ctrl_path = maps:get(<<"ctrl_path">>, Map, <<>>),
				ctrl_file = maps:get(<<"ctrl_file">>, Map, <<>>),
				ctrl_modified = maps:get(<<"ctrl_modified">>, Map, undefined),
				ctrl_hash = erlang:phash2(Map)
			}
		}
	catch
		_Exception:Reason -> 
			ems_logger:format_warn("ems_client parse invalid client specification: ~p\n\t~p.\n", [Reason, Map]),
			{error, Reason}
	end.


-spec get_table(fs | db) -> client_db | client_fs.
get_table(db) -> client_db;
get_table(fs) -> client_fs.

-spec find(client_fs | client_db, non_neg_integer()) -> {ok, #client{}} | {error, atom()}.
find(Table, Codigo) ->
	case ems_db:find_first(Table, [{codigo, "==", Codigo}]) of
		{error, Reason} -> {error, Reason};
		Record -> {ok, setelement(1, Record, client)}
	end.

-spec all(client_fs | client_db) -> list() | {error, atom()}.
all(Table) ->
	case ems_db:all(Table) of
		{ok, Records} -> 
			Records2 = [setelement(1, R, client) || R <- Records],
			{ok, Records2};
		Error -> Error
	end.


%% middleware functions

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

delete(Id) -> ems_db:delete(client, Id).

valida(_Client, _Operation) -> ok.

