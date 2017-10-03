%%********************************************************************
%% @title Module ems_catalog_loader_get_middleware
%% @version 1.0.0
%% @doc Module ems_catalog_loader_get_middleware
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_loader_get_middleware).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([insert/2, update/2]).

insert({Codigo, Name, Url,  Type, Service, Comment, Version, Owner, 
		ResultCache, Authorization, Lang, Timeout, Enable, ContentType}, CtrlInsert) ->
	Url2 = ?UTF8_STRING(Url),
	Rowid = ems_util:make_rowid(Url2),		
	#service{
		id = ems_db:sequence(service),
		rowid = Rowid,
		codigo = Codigo,
		name = ?UTF8_STRING(Name),
		url = Url2,
		type = ?UTF8_STRING(Type),
		service = ?UTF8_STRING(Service),
		comment = ?UTF8_STRING(Comment),
		version = ?UTF8_STRING(Version),
		owner = ?UTF8_STRING(Owner),
		result_cache = ResultCache,
		authorization = ?UTF8_STRING(Authorization),
		lang = Lang,
		timeout = Timeout,
		enable = Enable,
		content_type = ?UTF8_STRING(ContentType),
		ctrl_insert = CtrlInsert
	}.


update({Codigo, Name, Url,  Type, Service, Comment, Version, Owner, 
		ResultCache, Authorization, Lang, Timeout, Enable, ContentType} = Record, CtrlUpdate) ->
	case ems_db:find_first(catalog_get, [{codigo, "==", Codigo}]) of
		{error, enoent} -> insert(Record, CtrlUpdate);
		Catalog ->
			Url2 = ?UTF8_STRING(Url),
			Rowid = ems_util:make_rowid(Url2),		
			Catalog2 = setelement(1, Catalog, service),  % define o tipo service
			Catalog2#service{
				rowid = Rowid,
				name = ?UTF8_STRING(Name),
				url = Url2,
				type = ?UTF8_STRING(Type),
				service = ?UTF8_STRING(Service),
				comment = ?UTF8_STRING(Comment),
				version = ?UTF8_STRING(Version),
				owner = ?UTF8_STRING(Owner),
				result_cache = ResultCache,
				authorization = ?UTF8_STRING(Authorization),
				lang = Lang,
				timeout = Timeout,
				enable = Enable,
				content_type = ?UTF8_STRING(ContentType),
				ctrl_update = CtrlUpdate
			}
	end.

