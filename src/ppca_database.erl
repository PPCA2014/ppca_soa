%%********************************************************************
%% @title Módulo database
%% @version 1.0.0
%% @doc Módulo database
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(ppca_database).

-export([start/0]).

-include("../include/ppca_schema.hrl").

start() ->
	create_database([node()]).
	
create_database(Nodes) ->
	mnesia:create_schema(Nodes),
	mnesia:start(),

    mnesia:create_table(user, [{type, set},
							   {disc_copies, Nodes},
							   {attributes, record_info(fields, user)}]),

    mnesia:create_table(sequence, [{type, set},
									{disc_copies, Nodes},
									{attributes, record_info(fields, sequence)}]),

	ok.



	
