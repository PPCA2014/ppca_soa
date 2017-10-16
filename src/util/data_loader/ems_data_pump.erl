%%********************************************************************
%% @title Module ems_data_pump
%% @version 1.0.0
%% @doc Module responsible for load records into mnesia tables
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_data_pump).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([data_pump/15]).


-spec data_pump(list(tuple()), list(tuple()), non_neg_integer(), tuple(), 
			    #config{}, binary(), binary(), insert | update, 
			    non_neg_integer(), non_neg_integer(), non_neg_integer(), 
			    non_neg_integer(), non_neg_integer(),
			    fs | db, list()) -> {ok, non_neg_integer(), non_neg_integer(), non_neg_integer()}.
data_pump(Records, L, Count, CtrlData, Conf, Name, Middleware, Operation, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount, SourceType, Fields) when Count == 100 orelse (length(Records) == 0 andalso length(L) > 0) -> 
	F = fun() -> do_visit_records_persist(L, CtrlData, Conf, Name, Middleware, Operation, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount, SourceType, Fields) end,
	{ok, InsertCount2, UpdateCount2, ErrorCount2, DisabledCount2, SkipCount2} = mnesia:activity(transaction, F),
	data_pump(Records, [], 1, CtrlData, Conf, Name, Middleware, Operation, InsertCount2, UpdateCount2, ErrorCount2, DisabledCount2, SkipCount2, SourceType, Fields);
data_pump([], _, _, _, _, _, _, _, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount, _, _) -> {ok, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount};
data_pump([H|T], L, Count, CtrlData, Conf, Name, Middleware, Operation, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount, SourceType, Fields) ->
	data_pump(T, [H|L], Count+1, CtrlData, Conf, Name, Middleware, Operation, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount, SourceType, Fields).
	

-spec do_visit_records_persist(list(tuple()), tuple(), #config{}, binary(), binary(), 
									insert | update, non_neg_integer(), non_neg_integer(), 
									non_neg_integer(), non_neg_integer(), non_neg_integer(),
									fs | db, list()) -> ok.
do_visit_records_persist([], _, _, _, _, _, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount, _, _) -> {ok, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount};
do_visit_records_persist([H|T], CtrlDate, Conf, Name, Middleware, insert, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount, SourceType, Fields) ->
	case do_insert_record(H, CtrlDate, Conf, Name, Middleware, SourceType, Fields) of
		{ok, insert} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, insert, InsertCount+1, UpdateCount, ErrorCount, DisabledCount, SkipCount, SourceType, Fields);
		{ok, skip} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, insert, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount+1, SourceType, Fields);
		{error, edisabled} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, insert, InsertCount, UpdateCount, ErrorCount, DisabledCount+1, SkipCount+1, SourceType, Fields);
		_Error -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, insert, InsertCount, UpdateCount, ErrorCount+1, DisabledCount, SkipCount, SourceType, Fields)
	end;
do_visit_records_persist([H|T], CtrlDate, Conf, Name, Middleware, update, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount, SourceType, Fields) ->
	case do_update_record(H, CtrlDate, Conf, Name, Middleware, SourceType, Fields) of
		{ok, insert} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update, InsertCount+1, UpdateCount, ErrorCount, DisabledCount, SkipCount, SourceType, Fields);
		{ok, update} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update, InsertCount, UpdateCount+1, ErrorCount, DisabledCount, SkipCount, SourceType, Fields);
		{ok, skip} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update, InsertCount, UpdateCount, ErrorCount, DisabledCount, SkipCount+1, SourceType, Fields);
		{error, edisabled} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update, InsertCount, UpdateCount, ErrorCount, DisabledCount+1, SkipCount+1, SourceType, Fields);
		_Error -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update, InsertCount, UpdateCount, ErrorCount+1, DisabledCount, SkipCount, SourceType, Fields)
	end.


-spec do_insert_record(tuple(), tuple(), #config{}, binary(), binary(), fs | db, list()) -> ok | {error, atom()}.
do_insert_record(Record, CtrlInsert, Conf, Name, Middleware, SourceType, Fields) when is_tuple(Record) ->
	Map = ems_util:tuple_to_maps_with_keys(Record, Fields),
	do_insert_record(Map, CtrlInsert, Conf, Name, Middleware, SourceType, Fields);
do_insert_record(Map, CtrlInsert, Conf, Name, Middleware, SourceType, _Fields) ->
	case apply(Middleware, insert, [Map, CtrlInsert, Conf, SourceType]) of
		{ok, NewMap, Table, insert} ->
			NewMap2 = setelement(1, NewMap, Table),
			mnesia:write(NewMap2),
			{ok, insert};
		{ok, skip} -> {ok, skip};
		{error, edisabled} -> {error, edisabled};
		{error, Reason} = Error ->
			ems_logger:error("~s data insert error: ~p.", [Name, Reason]),
			Error
	end.


-spec do_update_record(list(), tuple(), #config{}, binary(), binary(), fs | db, list()) -> ok | {error, atom()}.
do_update_record(Record, CtrlUpdate, Conf, Name, Middleware, SourceType, Fields) when is_tuple(Record) ->
	Map = ems_util:tuple_to_maps_with_keys(Record, Fields),
	do_update_record(Map, CtrlUpdate, Conf, Name, Middleware, SourceType, Fields);
do_update_record(Map, CtrlUpdate, Conf, Name, Middleware, SourceType, _Fields) ->
	case apply(Middleware, update, [Map, CtrlUpdate, Conf, SourceType]) of
		{ok, UpdatedMap, Table, Operation} ->
			UpdatedMap2 = setelement(1, UpdatedMap, Table),
			mnesia:write(UpdatedMap2),
			{ok, Operation};
		{ok, skip} -> {ok, skip};
		{error, edisabled} -> {error, edisabled};
		{error, Reason} = Error ->	
			ems_logger:error("~s data update error: ~p.", [Name, Reason]),
			Error
	end.





