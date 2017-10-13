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

-export([data_pump/12]).


-spec data_pump(list(tuple()), list(tuple()), non_neg_integer(), tuple(), #config{}, binary(), binary(), insert | update, non_neg_integer(), non_neg_integer(), non_neg_integer(), fs | db) -> {ok, non_neg_integer(), non_neg_integer(), non_neg_integer()}.
data_pump(Records, L, Count, CtrlData, Conf, Name, Middleware, Operation, InsertCount, UpdateCount, ErrorCount, SourceType) when Count == 100 orelse (length(Records) == 0 andalso length(L) > 0) -> 
	F = fun() -> do_visit_records_persist(L, CtrlData, Conf, Name, Middleware, Operation, InsertCount, UpdateCount, ErrorCount, SourceType) end,
	{ok, InsertCount2, UpdateCount2, ErrorCount2} = mnesia:activity(transaction, F),
	data_pump(Records, [], 1, CtrlData, Conf, Name, Middleware, Operation, InsertCount2, UpdateCount2, ErrorCount2, SourceType);
data_pump([], _, _, _, _, _, _, _, InsertCount, UpdateCount, ErrorCount, _) -> {ok, InsertCount, UpdateCount, ErrorCount};
data_pump([H|T], L, Count, CtrlData, Conf, Name, Middleware, Operation, InsertCount, UpdateCount, ErrorCount, SourceType) ->
	data_pump(T, [H|L], Count+1, CtrlData, Conf, Name, Middleware, Operation, InsertCount, UpdateCount, ErrorCount, SourceType).
	

-spec do_visit_records_persist(list(tuple()), tuple(), #config{}, binary(), binary(), insert | update, non_neg_integer(), non_neg_integer(), non_neg_integer(), fs | db) -> ok.
do_visit_records_persist([], _, _, _, _, _, InsertCount, UpdateCount, ErrorCount, _) -> {ok, InsertCount, UpdateCount, ErrorCount};
do_visit_records_persist([H|T], CtrlDate, Conf, Name, Middleware, insert, InsertCount, UpdateCount, ErrorCount, SourceType) ->
	case do_insert_record(H, CtrlDate, Conf, Name, Middleware, SourceType) of
		{ok, insert} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, insert, InsertCount+1, UpdateCount, ErrorCount, SourceType);
		{ok, skip} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, insert, InsertCount, UpdateCount, ErrorCount, SourceType);
		{error, edisabled} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, insert, InsertCount, UpdateCount, ErrorCount, SourceType);
		_Error -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, insert, InsertCount, UpdateCount, ErrorCount+1, SourceType)
	end;
do_visit_records_persist([H|T], CtrlDate, Conf, Name, Middleware, update, InsertCount, UpdateCount, ErrorCount, SourceType) ->
	case do_update_record(H, CtrlDate, Conf, Name, Middleware, SourceType) of
		{ok, insert} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update, InsertCount+1, UpdateCount, ErrorCount, SourceType);
		{ok, update} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update, InsertCount, UpdateCount+1, ErrorCount, SourceType);
		{ok, skip} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update, InsertCount, UpdateCount, ErrorCount, SourceType);
		{error, edisabled} -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update, InsertCount, UpdateCount, ErrorCount, SourceType);
		_Error -> do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update, InsertCount, UpdateCount, ErrorCount+1, SourceType)
	end.


-spec do_insert_record(tuple(), tuple(), #config{}, binary(), binary(), fs | db) -> ok | {error, atom()}.
do_insert_record(Record, CtrlInsert, Conf, Name, Middleware, SourceType) ->
	case apply(Middleware, insert, [Record, CtrlInsert, Conf, SourceType]) of
		{ok, NewRecord, Table, insert} ->
			NewRecord2 = setelement(1, NewRecord, Table),
			mnesia:write(NewRecord2),
			{ok, insert};
		{ok, skip} -> {ok, skip};
		{error, edisabled} -> {error, edisabled};
		{error, Reason} = Error ->
			ems_logger:error("~s data insert error: ~p.", [Name, Reason]),
			Error
	end.


-spec do_update_record(list(), tuple(), #config{}, binary(), binary(), fs | db) -> ok | {error, atom()}.
do_update_record(Record, CtrlUpdate, Conf, Name, Middleware, SourceType) ->
	case apply(Middleware, update, [Record, CtrlUpdate, Conf, SourceType]) of
		{ok, UpdatedRecord, Table, Operation} ->
			UpdatedRecord2 = setelement(1, UpdatedRecord, Table),
			mnesia:write(UpdatedRecord2),
			{ok, Operation};
		{ok, skip} -> {ok, skip};
		{error, edisabled} -> {error, edisabled};
		{error, Reason} = Error ->	
			ems_logger:error("~s data update error: ~p.", [Name, Reason]),
			Error
	end.





