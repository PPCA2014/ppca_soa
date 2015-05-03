%% ---
%%  PPCA_SOA
%%  Biblioteca de utilitários
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---
-module(ppca_util).

-export([sleep/1,
		 timestamp_str/0,
		 json_encode/1,
		 json_decode/1,
		 hd_or_empty/1]).

sleep(T) ->
    receive
	    after T ->
	       true
    end.

timestamp_str() ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("~p/~p/~p ~p:~p:~p", [Dia, Mes, Ano, Hora, Min, Seg])).
	
json_encode(_Json)->
	%jiffy:encode(Json).
	"".
	
json_decode(_IoData) ->
	%jiffy:decode(IoData).
	"".

hd_or_empty(List) when length(List) > 0 -> 
	hd(List);
	
hd_or_empty(_) -> [].

