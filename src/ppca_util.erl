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
		 encode_json/1,
		 decode_json/1]).

sleep(T) ->
    receive
	    after T ->
	       true
    end.


timestamp_str() ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("~p/~p/~p ~p:~p:~p", [Dia, Mes, Ano, Hora, Min, Seg])).
	%%"30/03/2015 11:43:00".
	
	
encode_json(Json)->
	jiffy:encode(Json).

	
decode_json(IoData) ->
	jiffy:decode(IoData).


