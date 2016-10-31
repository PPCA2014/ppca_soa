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
-include("../include/ems_http_messages.hrl").

%% Client API
-export([start/0, lookup/2, add/3, lookup_options/0, add_options/1]).

start() -> 
	ets:new(dispatcher_cache_get, [set, named_table, public, {read_concurrency, true}]),
	ets:new(dispatcher_cache_options, [set, named_table, public, {read_concurrency, true}]).

lookup(Rowid, Timestamp2) ->
	case ets:lookup(dispatcher_cache_get, Rowid) of
		[] -> false;
		[{_, Timestamp, _}] when Timestamp2 - Timestamp > ?TIMEOUT_DISPATCHER_CACHE -> 
			io:format("timeout!!!\n"),
			false;
		[{_, _, Response}] -> {true, Response}
	end.

lookup_options() ->
	case ets:lookup(dispatcher_cache_options, 1) of
		[] -> false;
		[{1, Response}] -> {true, Response}
	end.

add(Rowid, Timestamp, Response) -> ets:insert(dispatcher_cache_get, {Rowid, Timestamp, Response}).

add_options(Response) -> ets:insert(dispatcher_cache_options, {1, Response}).
