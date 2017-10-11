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

-export([data_pump/8]).


-spec data_pump(list(tuple()), list(tuple()), non_neg_integer(), tuple(), #config{}, binary(), binary(), insert | update) -> ok.
data_pump(Records, L, Count, CtrlData, Conf, Name, Middleware, Operation) when Count == 100 orelse (length(Records) == 0 andalso length(L) > 0) -> 
	F = fun() -> do_visit_records_persist(L, CtrlData, Conf, Name, Middleware, Operation) end,
	mnesia:activity(transaction, F),
	data_pump(Records, [], 1, CtrlData, Conf, Name, Middleware, Operation);
data_pump([], _, _, _, _, _, _, _) -> ok;
data_pump([H|T], L, Count, CtrlData, Conf, Name, Middleware, Operation) ->
	data_pump(T, [H|L], Count+1, CtrlData, Conf, Name, Middleware, Operation).
	

-spec do_visit_records_persist(list(tuple()), tuple(), #config{}, binary(), binary(), insert | update) -> ok.
do_visit_records_persist([], _, _, _, _, _) -> ok;
do_visit_records_persist([H|T], CtrlDate, Conf, Name, Middleware, insert) ->
	do_insert_record(H, CtrlDate, Conf, Name, Middleware),
	do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, insert);
do_visit_records_persist([H|T], CtrlDate, Conf, Name, Middleware, update) ->
	do_update_record(H, CtrlDate, Conf, Name, Middleware),
	do_visit_records_persist(T, CtrlDate, Conf, Name, Middleware, update).


-spec do_insert_record(tuple(), tuple(), #config{}, binary(), binary()) -> ok | {error, atom()}.
do_insert_record(Record, CtrlInsert, Conf, Name, Middleware) ->
	case apply(Middleware, insert, [Record, CtrlInsert, Conf]) of
		{ok, NewRecord, Table} ->
			NewRecord2 = setelement(1, NewRecord, Table),
			mnesia:write(NewRecord2),
			ok;
		{ok, skip} -> 
			ok;
		{error, Reason} = Error ->
			ems_logger:error("~s data insert error: ~p.", [Name, Reason]),
			Error
	end.

-spec do_update_record(list(), tuple(), #config{}, binary(), binary()) -> ok | {error, atom()}.
do_update_record(Record, CtrlUpdate, Conf, Name, Middleware) ->
	case apply(Middleware, update, [Record, CtrlUpdate, Conf]) of
		{ok, UpdatedRecord, Table} ->
			UpdatedRecord2 = setelement(1, UpdatedRecord, Table),
			mnesia:write(UpdatedRecord2),
			ok;
		{ok, skip} -> 
			ok;
		{error, Reason} = Error ->	
			ems_logger:error("~s data update error>>>>>: ~p.", [Name, Reason]),
			Error
	end.





