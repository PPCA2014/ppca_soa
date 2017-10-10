%%********************************************************************
%% @title Module ems_catalog_loader_fs
%% @version 1.0.0
%% @doc Module responsible for load catalog services from filesystem
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_loader_fs).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([insert/3, update/3, is_empty/0, size_table/0, clear_table/0, reset_sequence/0]).

-spec insert(map(), tuple(), #config{}) -> {ok, #service{}, atom()} | {error, atom()}.
insert(Map, CtrlInsert, Conf) ->
	try
		?DEBUG("ems_catalog_loader_fs insert catalog ~p from filesystem.", [Map]),
		case ems_catalog:new_service_from_map(Map, Conf) of
			{ok, Catalog} -> 
				EtsTable = ets_table(Catalog#service.type),
				Id = ets_sequence(Catalog#service.type),
				Catalog2 = Catalog#service{id = Id,
										   ctrl_insert = CtrlInsert},
				{ok, Catalog2, EtsTable};
			Error -> Error
		end
	catch
		_Exception:Reason -> {error, Reason}
	end.

-spec update(tuple(), tuple(), #config{}) -> {ok, #service{}, atom()} | {error, atom()}.
update(Map = #{<<"type">> := Type, <<"rowid">> := Rowid}, CtrlUpdate, Conf) ->
	try
		EtsTable = ets_table(Type),
		case ems_db:find_first(EtsTable, [{rowid, "==", Rowid}]) of
			{error, enoent} -> insert(Map, CtrlUpdate, Conf);
			CurrentCatalog ->
				?DEBUG("ems_catalog_loader_fs update catalog ~p from filesystem.", [Map]),
				case ems_catalog:new_service_from_map(Map, Conf) of
					{ok, Catalog} -> 
						Catalog2 = Catalog#service{ctrl_update = CtrlUpdate},
						Catalog3 = maps:merge(CurrentCatalog, Catalog2),
						{ok, Catalog3, EtsTable};
					Error -> Error
				end
		end
	catch
		_Exception:Reason -> {error, Reason}
	end.


-spec is_empty() -> boolean().
is_empty() ->	
	mnesia:table_info(catalog_get_fs, size) == 0 andalso
	mnesia:table_info(catalog_post_fs, size) == 0 andalso
	mnesia:table_info(catalog_put_fs, size) == 0 andalso
	mnesia:table_info(catalog_delete_fs, size) == 0 andalso
	mnesia:table_info(catalog_options_fs, size) == 0 andalso
	mnesia:table_info(catalog_kernel_fs, size) == 0.


-spec size_table() -> non_neg_integer().
size_table() ->	
	mnesia:table_info(catalog_get_fs, size) +
	mnesia:table_info(catalog_post_fs, size) +
	mnesia:table_info(catalog_put_fs, size) +
	mnesia:table_info(catalog_delete_fs, size) +
	mnesia:table_info(catalog_options_fs, size) +
	mnesia:table_info(catalog_kernel_fs, size).
	

-spec clear_table() -> ok | {error, efail_clear_ets_table}.
clear_table() ->	
	case mnesia:clear_table(catalog_get_fs) of
		{atomic, ok} -> 
			case mnesia:clear_table(catalog_post_fs) of
				{atomic, ok} -> 
					case mnesia:clear_table(catalog_put_fs) of
						{atomic, ok} ->
							case mnesia:clear_table(catalog_delete_fs) of
								{atomic, ok} -> 
									case mnesia:clear_table(catalog_options_fs) of
										{atomic, ok} -> 									
											case mnesia:clear_table(catalog_kernel_fs) of
												{atomic, ok} -> ok;
												_ -> {error, efail_clear_ets_table}
											end;
										_ -> {error, efail_clear_ets_table}
									end;
								_ -> {error, efail_clear_ets_table}
							end;
						_ -> {error, efail_clear_ets_table}
					end;
				_ -> {error, efail_clear_ets_table}
			end;
		_ -> {error, efail_clear_ets_table}
	end.
	
	
-spec reset_sequence() -> ok.
reset_sequence() ->	
	ems_db:init_sequence(catalog_get_fs, 0),
	ems_db:init_sequence(catalog_post_fs, 0),
	ems_db:init_sequence(catalog_put_fs, 0),
	ems_db:init_sequence(catalog_delete_fs, 0),
	ems_db:init_sequence(catalog_options_fs, 0),
	ems_db:init_sequence(catalog_options_fs, 0),
	ems_db:init_sequence(catalog_kernel_fs, 0),
	ok.
	

%% internal functions

-spec ets_table(binary()) -> catalog_get_fs | catalog_post_fs | catalog_put_fs | catalog_delete_fs | catalog_options_fs.
ets_table(<<"GET">>) -> catalog_get_fs;
ets_table(<<"POST">>) -> catalog_post_fs;
ets_table(<<"PUT">>) -> catalog_put_fs;
ets_table(<<"DELETE">>) -> catalog_delete_fs;
ets_table(<<"OPTIONS">>) -> catalog_options_fs;
ets_table(<<"KERNEL">>) -> catalog_kernel_fs.


-spec ets_sequence(binary()) -> non_neg_integer.
ets_sequence(<<"GET">>) -> ems_db:sequence(catalog_get_fs);
ets_sequence(<<"POST">>) -> ems_db:sequence(catalog_post_fs);
ets_sequence(<<"PUT">>) -> ems_db:sequence(catalog_put_fs);
ets_sequence(<<"DELETE">>) -> ems_db:sequence(catalog_delete_fs);
ets_sequence(<<"OPTIONS">>) -> ems_db:sequence(catalog_options_fs);
ets_sequence(<<"KERNEL">>) -> ems_db:sequence(catalog_kernel_fs).

