%%********************************************************************
%% @title Module ems_user_email
%% @version 1.0.0
%% @doc user class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_email).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([new_from_map/2,
		 get_table/1,
		 find/2,
		 all/1]).


-spec new_from_map(map(), #config{}) -> {ok, #user_email{}} | {error, atom()}.
new_from_map(Map, #config{sufixo_email_institucional = SufixoEmailInstitucional}) ->
	try
		Email = ems_util:parse_email(?UTF8_STRING(maps:get(<<"email">>, Map))),
		{ok, #user_email{
					id = maps:get(<<"id">>, Map),
					codigo = maps:get(<<"codigo">>, Map),
					email = Email,
					type = is_email_institucional(SufixoEmailInstitucional, Email),
					ctrl_path = maps:get(<<"ctrl_path">>, Map, <<>>),
					ctrl_file = maps:get(<<"ctrl_file">>, Map, <<>>),
					ctrl_modified = maps:get(<<"ctrl_modified">>, Map, undefined),
					ctrl_hash = erlang:phash2(Map)
			}
		}
	catch
		_Exception:Reason -> 
			ems_db:inc_counter(edata_loader_invalid_user_email),
			ems_logger:warn("ems_user_email parse invalid email specification: ~p\n\t~p.\n", [Reason, Map]),
			{error, Reason}
	end.


-spec get_table(fs | db) -> user_email_db | user_email_fs.
get_table(db) -> user_email_db;
get_table(fs) -> user_email_fs.

-spec find(user_email_fs | user_email_db, non_neg_integer()) -> {ok, #user{}} | {error, enoent}.
find(Table, Id) ->
	case mnesia:dirty_read(Table, Id) of
		[] -> {error, enoent};
		[Record|_] -> {ok, Record}
	end.

-spec all(user_email_fs | user_email_db) -> list() | {error, atom()}.
all(Table) -> ems_db:all(Table).

-spec is_email_institucional(binary(), binary()) -> 1 | 2.
is_email_institucional("", _) -> 2;
is_email_institucional(SufixoEmailInstitucional, Email) ->
	case lists:suffix(SufixoEmailInstitucional, binary_to_list(Email)) of
		true -> 1;
		false -> 2
	end.
