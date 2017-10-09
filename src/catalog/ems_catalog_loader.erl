%%********************************************************************
%% @title Module ems_catalog_loader
%% @version 1.0.0
%% @doc Module responsible for load catalog services from disk
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_loader).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([start/0]).

start() ->
	Conf = ems_config:getConfig(),
	ListCatalog = ems_json_loader:scan(Conf#config.cat_path_search, Conf),
	parse_catalog(ListCatalog, [], [], [],  1, Conf),
	ok.
	

make_ets_rest_catalog([]) -> ok;
make_ets_rest_catalog([H = {_Rowid, #service{type = Type}}|T]) -> 
	case Type of
		<<"GET">> -> ets:insert(ets_get, H);
		<<"POST">> -> ets:insert(ets_post, H);
		<<"PUT">> -> ets:insert(ets_put, H);
		<<"DELETE">> -> ets:insert(ets_delete, H);
		<<"OPTIONS">> -> ets:insert(ets_options, H)
	end,
	make_ets_rest_catalog(T). 	


%% @doc Faz o parser dos contratos de serviços no catálogo de serviços
parse_catalog([], CatREST, CatRE, CatKernel, _Id, _Conf) ->
	ets:new(ets_ems_catalog, [set, named_table, public, {read_concurrency, true}]),
	ets:new(ets_get, [ordered_set, named_table, public, {read_concurrency, true}]),
	ets:new(ets_post, [ordered_set, named_table, public, {read_concurrency, true}]),
	ets:new(ets_put, [ordered_set, named_table, public, {read_concurrency, true}]),
	ets:new(ets_delete, [ordered_set, named_table, public, {read_concurrency, true}]),
	ets:new(ets_options, [ordered_set, named_table, public, {read_concurrency, true}]),
	make_ets_rest_catalog(CatREST),
	ems_util:list_to_ets(CatREST, ets_rest, [ordered_set, public, {read_concurrency, true}]),
	ets:insert(ets_ems_catalog, {cat, {CatREST, CatRE, CatKernel}});

	
parse_catalog([H|T], CatREST, CatRE, CatKernel, Id, Conf) ->
	?DEBUG("Parse catalog ~p.", [H]),
	case ems_catalog:new_service_from_map(H, Conf) of
		{ok, Service = #service{type = Type,
								rowid = Rowid}} ->
			case Type of
				<<"KERNEL">> -> parse_catalog(T, CatREST, CatRE, [Service|CatKernel], Id+1, Conf);
				_ -> parse_catalog(T, [{Rowid, Service}|CatREST], CatRE, CatKernel, Id+1, Conf)
			end;
		{error, _Reason} -> parse_catalog(T, CatREST, CatRE, CatKernel, Id, Conf)
	end.


	
	

	
	



