%%********************************************************************
%% @title Module ems_catalog_loader_db
%% @version 1.0.0
%% @doc Module ems_catalog_loader_db
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_loader_db).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([insert/3, update/3, is_empty/0, size_table/0, clear_table/0, reset_sequence/0]).

-spec insert(tuple(), tuple(), #config{}) -> {ok, #service{}, atom()} | {error, atom()}.
insert(RecordTuple, CtrlInsert, Conf) ->
	try
		Map = to_map(RecordTuple),
		?DEBUG("ems_catalog_loader_db insert catalog ~p from database.", [Map]),
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
update([Codigo | _] = RecordTuple, CtrlUpdate, Conf) ->
	try
		case ems_db:find_first(catalog_get, [{codigo, "==", Codigo}]) of
			{error, enoent} -> insert(RecordTuple, CtrlUpdate, Conf);
			CurrentCatalog ->
				Map = to_map(RecordTuple),
				?DEBUG("ems_catalog_loader_db update catalog ~p from database.", [Map]),
				case ems_catalog:new_service_from_map(Map, Conf) of
					{ok, Catalog} -> 
						EtsTable = ets_table(Catalog#service.type),
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
	mnesia:table_info(catalog_get, size) == 0 andalso
	mnesia:table_info(catalog_post, size) == 0 andalso
	mnesia:table_info(catalog_put, size) == 0 andalso
	mnesia:table_info(catalog_delete, size) == 0 andalso
	mnesia:table_info(catalog_options, size) == 0.

-spec size_table() -> non_neg_integer().
size_table() ->	
	mnesia:table_info(catalog_get, size) +
	mnesia:table_info(catalog_post, size) +
	mnesia:table_info(catalog_put, size) +
	mnesia:table_info(catalog_delete, size) +
	mnesia:table_info(catalog_options, size).
	

-spec clear_table() -> ok | {error, efail_clear_ets_table}.
clear_table() ->	
	case mnesia:clear_table(catalog_get) of
		{atomic, ok} -> 
			case mnesia:clear_table(catalog_post) of
				{atomic, ok} -> 
					case mnesia:clear_table(catalog_put) of
						{atomic, ok} ->
							case mnesia:clear_table(catalog_delete) of
								{atomic, ok} -> 
									case mnesia:clear_table(catalog_options) of
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
	end.
	
	
-spec reset_sequence() -> ok.
reset_sequence() ->	
	ems_db:init_sequence(catalog_get, 0),
	ems_db:init_sequence(catalog_post, 0),
	ems_db:init_sequence(catalog_put, 0),
	ems_db:init_sequence(catalog_delete, 0),
	ems_db:init_sequence(catalog_options, 0),
	ok.
	

%% internal functions

-spec ets_table(binary()) -> catalog_get | catalog_post | catalog_put | catalog_delete | catalog_options.
ets_table(<<"GET">>) -> catalog_get;
ets_table(<<"POST">>) -> catalog_post;
ets_table(<<"PUT">>) -> catalog_put;
ets_table(<<"DELETE">>) -> catalog_delete;
ets_table(<<"OPTIONS">>) -> catalog_options.

-spec ets_sequence(binary()) -> non_neg_integer.
ets_sequence(<<"GET">>) -> ems_db:sequence(catalog_get);
ets_sequence(<<"POST">>) -> ems_db:sequence(catalog_post);
ets_sequence(<<"PUT">>) -> ems_db:sequence(catalog_put);
ets_sequence(<<"DELETE">>) -> ems_db:sequence(catalog_delete);
ets_sequence(<<"OPTIONS">>) -> ems_db:sequence(catalog_options).

to_map(RecordTuple) ->
		ems_util:tuple_to_maps_with_keys(RecordTuple, [<<"codigo">>,
														<<"name">>,
														<<"url">>,
														<<"use_re">>,
														<<"type">>,
														<<"service">>,
														<<"comment">>,
														<<"version">>,
														<<"owner">>,
														<<"result_cache">>,
														<<"authorization">>,
														<<"lang">>,
														<<"timeout">>,
														<<"enable">>,
														<<"content_type">>,
														<<"async">>,
														<<"host">>,
														<<"node">>,
														<<"debug">>,
														<<"schema_in">>,
														<<"schema_out">>,
														<<"middleware">>,
														<<"cache_control">>,
														<<"expires">>,
														<<"public">>,
														<<"path">>,
														<<"redirect_url">>,
														<<"tcp_listen_address">>,
														<<"tcp_allowed_address">>,
														<<"tcp_max_connections">>,
														<<"tcp_port">>,
														<<"tcp_is_ssl">>,
														<<"tcp_ssl_cacertfile">>,
														<<"tcp_ssl_certfile">>,
														<<"tcp_ssl_keyfile">>,
														<<"oauth2_with_check_constraint">>]).
