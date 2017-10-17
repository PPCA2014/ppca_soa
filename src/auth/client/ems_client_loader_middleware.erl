%%********************************************************************
%% @title Module ems_client_loader_middleware
%% @version 1.0.0
%% @doc Module responsible for load client from filesystem or db
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_client_loader_middleware).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([insert/4, update/4, is_empty/1, size_table/1, clear_table/1, reset_sequence/1, get_filename/0]).

-spec insert(map(), tuple(), #config{}, fs | db) -> {ok, #service{}, atom(), insert | update} | {ok, skip} | {error, atom()}.
insert(Map, CtrlInsert, Conf, SourceType) ->
	prepare_insert_or_update(Map, CtrlInsert, Conf, SourceType).


-spec update(tuple(), tuple(), #config{}, fs | db) -> {ok, #service{}, atom(), insert | update} | {ok, skip} | {error, atom()}.
update(Map, CtrlUpdate, Conf, SourceType) ->
	prepare_insert_or_update(Map, CtrlUpdate, Conf, SourceType).


-spec is_empty(fs | db) -> boolean().
is_empty(db) ->	mnesia:table_info(client_db, size) == 0;
is_empty(fs) ->	mnesia:table_info(client_fs, size) == 0.
	

-spec size_table(fs | db) -> non_neg_integer().
size_table(db) -> mnesia:table_info(client_db, size);
size_table(fs) -> mnesia:table_info(client_fs, size).
	

-spec clear_table(fs | db) -> ok | {error, efail_clear_ets_table}.
clear_table(db) ->	
	case mnesia:clear_table(client_db) of
		{atomic, ok} -> ok;
		_ -> {error, efail_clear_ets_table}
	end;
clear_table(fs) ->	
	case mnesia:clear_table(client_fs) of
		{atomic, ok} -> ok;
		_ -> {error, efail_clear_ets_table}
	end.
	
	
-spec reset_sequence(fs | db) -> ok.
reset_sequence(db) -> 
	ems_db:init_sequence(client_db, 0),
	ok;
reset_sequence(fs) ->	
	ems_db:init_sequence(client_fs, 0),
	ok.
	

%% internal functions

-spec get_filename() -> list(tuple()).
get_filename() -> 
	Conf = ems_config:getConfig(),
	Conf#config.client_path_search.
	
	
-spec prepare_insert_or_update(map() | tuple(), tuple(), #config{}, atom()) -> {ok, #service{}, atom(), insert | update} | {ok, skip} | {error, atom()}.
prepare_insert_or_update(Map, CtrlDate, Conf, SourceType) ->
	try
		case ems_client:new_client_from_map(Map, Conf) of
			{ok, NewClient = #client{codigo = Codigo, ctrl_modified = CtrlModified, ctrl_hash = CtrlHash}} -> 
				Table = ems_client:get_table(SourceType),
				case ems_client:find(Table, Codigo) of
					{error, enoent} -> 
						Id = ems_db:sequence(Table),
						Client = NewClient#client{id = Id,
											      ctrl_insert = CtrlDate},
						{ok, Client, Table, insert};
					{ok, CurrentClient = #client{ctrl_modified = CurrentCtrlModified, ctrl_hash = CurrentCtrlHash}} ->
						case CtrlHash =/= CurrentCtrlHash of
							true ->
								case CtrlModified == undefined orelse CtrlModified > CurrentCtrlModified of
									true ->
										?DEBUG("ems_client_loader_middleware update ~p from ~p.", [Map, SourceType]),
										Client = CurrentClient#client{
														 codigo = Codigo,
														 name = NewClient#client.name,
														 secret = NewClient#client.secret,
														 redirect_uri = NewClient#client.redirect_uri,
														 description = NewClient#client.description,
														 scope = NewClient#client.scope,
														 active = NewClient#client.active,
														 ctrl_path = NewClient#client.ctrl_path,
														 ctrl_file = NewClient#client.ctrl_file,
														 ctrl_update = CtrlDate,
														 ctrl_modified = CtrlModified,
														 ctrl_hash = CtrlHash
													},
										{ok, Client, Table, update};
									false -> {ok, skip}
								end;
							false -> {ok, skip}
						end
				end;
			Error -> Error
		end

	catch
		_Exception:Reason -> {error, Reason}
	end.

