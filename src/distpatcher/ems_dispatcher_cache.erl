%%********************************************************************
%% @title Module ems_dispatcher_cache_get
%% @version 1.0.0
%% @doc Responsible for forwarding the requests to services.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_dispatcher_cache).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Client API
-export([start/0, lookup/2, add/4, invalidate/0]).

start() -> 
	ets:new(dispatcher_cache_get, [set, named_table, public, {read_concurrency, true}]),
	ets:new(dispatcher_cache_options, [set, named_table, public, {read_concurrency, true}]).

lookup(ReqHash, Timestamp2) ->
	try
		case ets:lookup(dispatcher_cache_get, ReqHash) of
			[] -> false;
			[{_, Timestamp, _, ResultCache}] when Timestamp2 - Timestamp > ResultCache -> false;
			[{_, _, Request, _}] -> {true, Request}
		end
	catch
		_Exception:_Reason ->
			ems_logger:warn("ems_dispatcher_cache recreate ets dispatcher_cache_get."),
			ets:new(dispatcher_cache_get, [set, named_table, public, {read_concurrency, true}]),
			lookup(ReqHash, Timestamp2)
	end.

add(ReqHash, Timestamp, Request, ResultCache) -> ets:insert(dispatcher_cache_get, {ReqHash, Timestamp, Request, ResultCache}).

invalidate() ->
	?DEBUG("ems_dispatcher_cache invalidate request get cache after POST, PUT or DELETE operation."),
	ets:delete_all_objects(dispatcher_cache_get).
