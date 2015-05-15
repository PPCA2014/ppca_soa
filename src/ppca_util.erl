%% ---
%%  PPCA_SOA
%%  Biblioteca de utilitários
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Aluno: Everton de Vargas Agilar (evertonagilar@gmail.com)
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
	
json_encode(JSON)->
	jsx:encode(JSON).
	
json_decode(JSON) ->
	try
		Result = jsx:decode(JSON),
		{ok, Result}
	catch
		_Exception:_Reason -> 
			_Reason,
			
			{error, einvalid_json}
	end.

hd_or_empty(List) when length(List) > 0 -> 
	hd(List);
	
hd_or_empty(_) -> [].

