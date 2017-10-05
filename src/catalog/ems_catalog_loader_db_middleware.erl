%%********************************************************************
%% @title Module ems_catalog_loader_db_middleware
%% @version 1.0.0
%% @doc Module ems_catalog_loader_db_middleware
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_loader_db_middleware).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([insert/2, update/2, is_empty/0, size_table/0, clear_table/0, lock_table/0, reset_sequence/0]).

-spec insert(tuple(), tuple()) -> {ok, #service{}, atom()} | {error, atom()}.
insert({Codigo, Name, Url,  Type, Service, Comment, Version, Owner, 
		ResultCache, Authorization, Lang, Timeout, 
		Enable, ContentType}, CtrlInsert) ->
	try
		Url2 = ems_util:parse_url_service(Url),
		Rowid = ems_util:make_rowid(Url2),		
		Type2 = ems_util:parse_type_service(Type),
		EtsTable = ets_table(Type2),
		{ModuleName, ModuleNameCanonical, FunctionName} = ems_util:parse_service_service(Service),
		Id = ets_sequence(Type2),
		Name2 = ems_util:parse_name_service(Name),
		Service2 = ?UTF8_STRING(Service),
		Comment2 = ?UTF8_STRING(Comment),
		Version2 = ?UTF8_STRING(Version),
		Owner2 = ?UTF8_STRING(Owner),
		Async = false,
		Querystring = [],
		{Querystring2, QtdQuerystringRequired} = ems_util:parse_querystring(Querystring),
		Catalog = ems_catalog:new_service(Rowid, 
										Id, 
										Name2, 
										Url2, 
										Service2,
										ModuleName,
										ModuleNameCanonical,
										FunctionName, 
										Type2, 
										Enable, 
										Comment2,
										Version2, 
										Owner2, 
										Async, 
										Querystring, QtdQuerystringRequired,
										Host, HostName, ResultCache,
										Authorization, Node, Lang,
										Datasource, Debug, SchemaIn, SchemaOut, 
										PoolSize, PoolMax, H, Page, 
										PageModule, Timeout, 
										Middleware, CacheControl, 
										ExpiresMinute, Public, 
										ContentType, Path, RedirectUrl,
										ListenAddress, ListenAddress_t, AllowedAddress, 
										AllowedAddress_t, Port, MaxConnections,
										IsSsl, SslCaCertFile, SslCertFile, SslKeyFile,
										OAuth2WithCheckConstraint, OAuth2TokenEncrypt, Protocol,
										CatalogPath, CatalogFile),
	
		Catalog = #service{
				id = ets_sequence(Type2),
				rowid = Rowid,
				codigo = Codigo,
				name = ems_util:parse_name_service(Name),
				url = Url2,
				type = Type2,
				service = Service,
			    module_name = ModuleName,
			    module_name_canonical = ModuleNameCanonical,
			    module = list_to_atom(ModuleName),
			    function_name = FunctionName,
			    function = list_to_atom(FunctionName),
				comment = ?UTF8_STRING(Comment),
				version = ?UTF8_STRING(Version),
				owner = ?UTF8_STRING(Owner),
				result_cache = ResultCache,
				authorization = ems_util:parse_authorization_type(Authorization),
				lang = ems_util:parse_lang(Lang),
				timeout = ems_util:parse_timeout(Timeout, ?SERVICE_MAX_TIMEOUT),
				enable = ems_util:parse_bool(Enable),
				content_type = ?UTF8_STRING(ContentType),
				ctrl_insert = CtrlInsert
			},
		{ok, Catalog, EtsTable}
	catch
		_Exception:Reason -> {error, Reason}
	end.

-spec update(tuple(), tuple()) -> {ok, #service{}, atom()} | {error, atom()}.
update({Codigo, Name, Url,  Type, Service, Comment, Version, Owner, 
		ResultCache, Authorization, Lang, Timeout, 
		Enable, ContentType} = Record, CtrlUpdate) ->
	try
		case ems_db:find_first(catalog_get, [{codigo, "==", Codigo}]) of
			{error, enoent} -> insert(Record, CtrlUpdate);
			Catalog ->
				Url2 = ems_util:parse_url_service(Url),
				Rowid = ems_util:make_rowid(Url2),		
				Type2 = ems_util:parse_type_service(Type),
				EtsTable = ets_table(Type2),
				{ModuleName, ModuleNameCanonical, FunctionName} = ems_util:parse_service_service(Service),
				Catalog2 = setelement(1, Catalog, service),  % define o tipo service
				Catalog3 = Catalog2#service{
								rowid = Rowid,
								name = ems_util:parse_name_service(Name),
								url = Url2,
								type = Type2,
								service = Service,
								module_name = ModuleName,
								module_name_canonical = ModuleNameCanonical,
								module = list_to_atom(ModuleName),
								function_name = FunctionName,
								function = list_to_atom(FunctionName),
								comment = ?UTF8_STRING(Comment),
								version =  ?UTF8_STRING(Version),
								owner =  ?UTF8_STRING(Owner),
								result_cache = ems_util:parse_result_cache(ResultCache),
								authorization = ems_util:parse_authorization_type(Authorization),
								lang = ems_util:parse_lang(Lang),
								timeout = ems_util:parse_timeout(Timeout),
								enable = ems_util:parse_bool(Enable),
								content_type =  ?UTF8_STRING(ContentType),
								ctrl_update = CtrlUpdate
							},
				{ok, Catalog3, EtsTable}
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
	
-spec lock_table() -> ok | {error, efail_lock_table}.
lock_table() ->	
	case mnesia:write_lock_table(catalog_get) of
		ok -> 
			case mnesia:write_lock_table(catalog_post) of
				ok -> 
					case mnesia:write_lock_table(catalog_put) of
						ok ->
							case mnesia:write_lock_table(catalog_delete) of
								ok -> 
									case mnesia:write_lock_table(catalog_options) of
										ok -> ok;
										_ -> {error, efail_lock_table}
									end;
								_ -> {error, efail_lock_table}
							end;
						_ -> {error, efail_lock_table}
					end;
				_ -> {error, efail_lock_table}
			end;
		_ -> {error, efail_lock_table}
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
