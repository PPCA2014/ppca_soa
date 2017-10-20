%%********************************************************************
%% @title Module ems_user_dados_funcionais
%% @version 1.0.0
%% @doc user class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_dados_funcionais).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([new_from_map/2,
		 new_from_map/3,
		 get_table/1,
		 find/2,
		 all/1]).


new_from_map(Map, Conf) -> new_from_map(Map, Conf, undefined).

%-spec new_from_map(map(), #config{}) -> {ok, #service{}} | {error, atom()}.
new_from_map(Map, _Conf, Id) ->
	try
		{ok, #user_dados_funcionais{
					id = Id,
					codigo = maps:get(<<"codigo">>, Map),
					type = maps:get(<<"type">>, Map, 0),
					subtype = maps:get(<<"subtype">>, Map, 0),
					active = maps:get(<<"active">>, Map, 0),
					matricula = ?UTF8_STRING(maps:get(<<"matricula">>, Map, <<>>)),
					ctrl_path = maps:get(<<"ctrl_path">>, Map, <<>>),
					ctrl_file = maps:get(<<"ctrl_file">>, Map, <<>>),
					ctrl_modified = maps:get(<<"ctrl_modified">>, Map, undefined),
					ctrl_hash = erlang:phash2(Map)
			}
		}
	catch
		_Exception:Reason -> 
			ems_logger:format_warn("ems_user_dados_funcionais parse invalid user specification: ~p\n\t~p.\n", [Reason, Map]),
			{error, Reason}
	end.


-spec get_table(fs | db) -> user_dados_funcionais_db | user_dados_funcionais_fs.
get_table(db) -> user_dados_funcionais_db;
get_table(fs) -> user_dados_funcionais_fs.

-spec find(user_dados_funcionais_fs | user_dados_funcionais_db, non_neg_integer()) -> {ok, #user{}} | {error, atom()}.
find(Table, Codigo) ->
	case ems_db:find_first(Table, [{codigo, "==", Codigo}]) of
		{error, Reason} -> {error, Reason};
		Record -> {ok, setelement(1, Record, user_dados_funcionais)}
	end.

-spec all(user_dados_funcionais_fs | user_dados_funcionais_db) -> list() | {error, atom()}.
all(Table) ->
	case ems_db:all(Table) of
		{ok, Records} -> 
			Records2 = [setelement(1, R, user_dados_funcionais) || R <- Records],
			{ok, Records2};
		Error -> Error
	end.

