%%********************************************************************
%% @title Module ems_catalog_lookup
%% @version 1.0.0
%% @doc Module responsible for catalog lookup
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_lookup).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([lookup/1, 
		 lookup/2,
		 find/2,  
		 all/1,
		 list_kernel_catalog/0, 
		 list_re_catalog/0]).

-spec find(catalog_get_fs | catalog_post_fs | catalog_put_fs | catalog_delete_fs | catalog_options_fs | catalog_kernel_fs |
		   catalog_get_db | catalog_post_db | catalog_put_db | catalog_delete_db | catalog_options_db | catalog_kernel_db,
		   non_neg_integer()) -> {ok, #service{}} | {error, atom()}.
find(Table, Rowid) ->
	case ems_db:find_first(Table, [{rowid, "==", Rowid}]) of
		{error, Reason} -> {error, Reason};
		Record -> {ok, setelement(1, Record, service)}
	end.

-spec all(catalog_get_fs | catalog_post_fs | catalog_put_fs | catalog_delete_fs | catalog_options_fs | catalog_kernel_fs |
		   catalog_get_db | catalog_post_db | catalog_put_db | catalog_delete_db | catalog_options_db | catalog_kernel_db) -> list() | {error, atom()}.
all(Table) ->
	case ems_db:all(Table) of
		{ok, Records} -> 
			Records2 = [setelement(1, R, service) || R <- Records],
			{ok, Records2};
		Error -> Error
	end.

-spec lookup(#request{}) -> {error, enoent} | {#service{}, map(), map()}.
lookup(Request) ->	
	case lookup(Request, false, db) of
		{error, enoent} -> 
			case lookup(Request, false, fs) of
				{error, enoent} -> 
					case lookup(Request, true, db) of
						{error, enoent} -> lookup(Request, true, fs);
						Result -> Result
					end;
				Result -> Result
			end;			
		Result -> Result
	end.


-spec lookup(#request{}, boolean(), atom()) -> {error, enoent} | {#service{}, map(), map()}.
lookup(Request = #request{type = Type, rowid = Rowid, params_url = ParamsMap}, FindWithRE, SourceType) ->	
	Table = ems_catalog:get_table(Type, FindWithRE, SourceType),
	case FindWithRE of
		false ->
			case find(Table, Rowid) of
				{ok, Service} -> 
					Querystring = ems_util:parse_request_querystring(Service, Request),
					{Service, ParamsMap, Querystring};
				_ -> {error, enoent}
			end;
		true ->
			case all(Table) of
				{ok, Records} ->
					case lookup_re(Request, Records) of
						{error, enoent} = Error -> Error;
						{Service, ParamsMapRE} -> 
							Querystring = ems_util:parse_request_querystring(Service, Request),
							{Service, ParamsMapRE, Querystring}
					end;
				_ -> {error, enoent}
			end
	end.

lookup(Method, Uri) ->
	case ems_util:encode_request(Method, Uri) of
		{ok, Request} -> lookup(Request);
		Error -> Error
	end.

-spec list_kernel_catalog() -> list(tuple()).
list_kernel_catalog() ->
	case mnesia:table_info(catalog_kernel_fs, size) == 0 of
		true -> 
			Conf = ems_config:getConfig(),
			case ems_json_scan:scan_with_filter(Conf#config.cat_path_search, Conf, <<"type">>, <<"KERNEL">>) of
				{ok, CatKernel} -> 
					CatKernel2 = [ems_catalog:new_service_from_map(Map, Conf) || Map <- CatKernel],
					CatKernel3 = [Cat || {Reason, Cat} <- CatKernel2, Reason == ok],
					CatKernel3;
				Error -> Error
			end;
		false ->
			case ems_db:all(catalog_kernel_fs) of
				{ok, CatKernel} -> [setelement(1, Cat, service) || Cat <- CatKernel];
				Error -> Error
			end
	end.

list_re_catalog() ->
	case ets:lookup(ets_ems_catalog, cat) of
		[] -> {error, enoent};
		[{cat, {_, CatRE, _}}] -> CatRE
	end.
    
    
%%====================================================================
%% Funções internas
%%====================================================================

lookup_re(_, []) -> {error, enoent};
lookup_re(Request = #request{type = Type, url = Url}, [H|T]) ->
	try
		RE = H#service.id_re_compiled,
		PatternKey = ems_util:make_rowid_from_url(Url, Type),
		case re:run(PatternKey, RE, [{capture,all_names,binary}]) of
			match -> {H, #{}};
			{match, Params} -> 
				{namelist, ParamNames} = re:inspect(RE, namelist),
				ParamsMap = maps:from_list(lists:zip(ParamNames, Params)),
				{H, ParamsMap};
			nomatch -> lookup_re(Request, T);
			{error, _ErrType} -> {error, enoent}
		end
	catch 
		_Exception:_Reason -> {error, enoent}
	end.



