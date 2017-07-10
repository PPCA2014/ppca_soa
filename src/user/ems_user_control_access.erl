%%********************************************************************
%% @title Module ems_user
%% @version 1.0.0
%% @doc user control_access class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_control_access).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

-export([find_by_id/1, update/1, delete/1, all/0]).


find_by_id(Id) -> ems_db:get(control_access, Id).

insert(Access) -> ems_db:insert(Access).

update(Access) -> ems_db:update(Access).
	
all() -> ems_db:all(control_access).

delete(Id) -> ems_db:delete(control_access, Id).


