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
	?DEBUG("ems_catalog_loader scan catalogs."),
	ListCatalog = scan_catalogs(Conf#config.cat_path_search, Conf, []),
	?DEBUG("ems_catalog_loader scan catalogs ok."),
	parse_catalog(ListCatalog, [], [], [],  1, Conf),
	ok.
	

-spec scan_catalogs(list(tuple()), #config{}, list()) -> list().
scan_catalogs([], _, Result) -> Result;
scan_catalogs([{CatName, FileName}|Rest], Conf, Result) ->
	case parse_filename_catalog(FileName, ?CATALOGO_PATH) of
		{ok, FileName2} ->
			io:format("ems_catalog_loader loading ~p from ~p.\n", [binary_to_list(CatName), FileName2]),
			Result2 = scan_catalog(FileName2, Conf, Result),
			scan_catalogs(Rest, Conf, Result2);
		{error, FileName2} ->
			ems_logger:format_warn("ems_catalog_loader failed to scan invalid catalog ~p. Ignoring this catalog.\n", [FileName2])
	end.
		
		
-spec scan_catalog(FileName :: string(), Conf :: #config{}, Result :: list(map())) -> list(map()) | {error, atom()}.
scan_catalog(FileName, Conf, Result) ->
	CurrentDir = filename:dirname(FileName),
	case ems_util:read_file_as_map(FileName) of
		{ok, CatList} when is_list(CatList) -> 
			scan_catalog_entry(CatList, Conf, CurrentDir, FileName, Result);
		{ok, CatMap} -> 
			scan_catalog_entry([CatMap], Conf, CurrentDir, FileName, Result);
		{error, enoent} ->
			ems_logger:format_warn("ems_catalog_loader catalog ~p does not exist, ignoring this catalog.\n", [FileName]),
			Result;
		_ -> 
			ems_logger:format_warn("ems_catalog_loader failed to read invalid catalog ~p. Ignoring this catalog.\n", [FileName]),
			Result
	end.
	
-spec scan_catalog_entry(list(), Conf :: #config{}, string(), string(), list()) -> list().
scan_catalog_entry([], _, _, _, Result) -> 
	Result;
scan_catalog_entry([Cat|CatTail], Conf, CurrentDir, CurentFileNameCat, Result) ->
	case maps:is_key(<<"file">>, Cat) of
		true -> 
			case parse_filename_catalog(maps:get(<<"file">>, Cat), CurrentDir) of
				{ok, FileName} ->
					?DEBUG("ems_catalog_loader scan ~p.", [FileName]),
					Result2 = scan_catalog(FileName, Conf, Result),
					scan_catalog_entry(CatTail, Conf, CurrentDir, CurentFileNameCat, Result2);			
				{error, FileName} ->
					ems_logger:format_warn("ems_catalog_loader scan invalid catalog ~p. Ignoring this catalog.\n", [FileName]),
					?DEBUG("~p: ~p.", [FileName, Cat]),
					scan_catalog_entry(CatTail, Conf, CurrentDir, CurentFileNameCat, Result)
			end;
		false -> 
			Cat2 = Cat#{<<"catalog_path">> => CurrentDir,
						<<"catalog_file">> => CurentFileNameCat},
			scan_catalog_entry(CatTail, Conf, CurrentDir, CurentFileNameCat, [Cat2 | Result])
	end.

-spec parse_filename_catalog(map(), string()) -> {ok, string()} | {error, string()}.
parse_filename_catalog(FileName, CurrentDir) when is_binary(FileName) ->
	parse_filename_catalog(binary_to_list(FileName), CurrentDir);
parse_filename_catalog(FileName, CurrentDir) ->
	Ch = string:substr(FileName, 1, 1),
	Ch2 = string:substr(FileName, 2, 1),
	case Ch =:= "/" orelse (ems_util:is_letter(Ch) andalso Ch2 =:= ":")   of
		true -> {ok, FileName};  
		false ->
			case Ch == "~" of
				true -> 
					case init:get_argument(home) of
						{ok, [[HomePath]]} ->
							{ok, ems_util:replace(FileName, "~", HomePath)};
						_Error -> {error, FileName}
					end;
				_ -> 
					case Ch == "." of
						true -> {ok, CurrentDir ++ "/" ++ string:substr(FileName, 3)};
						false -> {ok, CurrentDir ++ "/" ++ FileName}
					end
			end
	end.


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


	
	

	
	



