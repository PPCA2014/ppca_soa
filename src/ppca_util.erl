%% ---
%%  PPCA_SOA
%%  Biblioteca de utilitários
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---
-module(ppca_util).

-export([sleep/1]).

sleep(T) ->
    receive
	    after T ->
	       true
    end.




