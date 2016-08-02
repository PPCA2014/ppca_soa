%%********************************************************************
%% @title Module ems_catalog
%% @version 1.0.0
%% @doc Manages information about catalog schemas
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_schema).

-export([all/0, get/1, insert/1, update/1, delete/1]).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").


get(Id) -> ems_db:get(catalog_schema, Id).

insert(Schema) -> 
	case valida(Schema, insert) of
		ok -> ems_db:insert(Schema);
		Error -> 
			Error
	end.

update(Schema) -> 
	case valida(Schema, update) of
		ok -> ems_db:update(Schema);
		Error -> Error
	end.

all() -> ems_db:all(catalog_schema).

delete(Id) -> 
	case valida(null, delete) of
		ok -> 
			ems_db:delete(catalog_schema, Id);
		Error -> Error
	end.

valida(Schema, insert) -> ok;
valida(Schema, update) -> ok;
valida(Schema, delete) -> ok.	


	
