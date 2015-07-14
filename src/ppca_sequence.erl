%%********************************************************************
%% @title Módulo ppca_sequence
%% @version 1.0.0
%% @doc Módulo responsável por gerar sequence de inteiros
%%      Implementado a partir de http://erlang.org/pipermail/erlang-questions/2005-August/016667.html
%% @end 
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(ppca_sequence).

-export([sequence/1, init_sequence/2]).

-include("../include/ppca_schema.hrl").

%% Inits or resets a sequence to Value
init_sequence(Name, Value) ->
     {atomic, ok} =
	mnesia:transaction(fun() ->
				   mnesia:write(#sequence{key=Name, index=Value})
			   end),
     ok.

%% Returns current value for sequence Name and increments
%% Sequence is created if not exists, and initial value 0 is returned.
sequence(Name) ->
     sequence(Name, 1).

%% increment sequence with Inc
sequence(Name, Inc) ->
     mnesia:dirty_update_counter(sequence, Name, Inc).
     
     
