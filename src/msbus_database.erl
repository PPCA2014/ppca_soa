%%********************************************************************
%% @title Módulo database
%% @version 1.0.0
%% @doc Módulo database
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_database).

-export([start/0]).

-include("../include/msbus_schema.hrl").

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

    mnesia:create_table(request, [{type, set},
									 {disc_copies, Nodes},
									 {attributes, record_info(fields, request)}]),

	ok.



	
