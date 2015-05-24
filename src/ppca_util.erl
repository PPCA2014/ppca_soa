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
		 hd_or_empty/1,
		 json_decode_as_map/1,
		 remove_backslash/1]).

%% @doc Dorme por um determinado tempo
sleep(T) ->
    receive
	    after T -> true
    end.

%% @doc Retorna o timestamp em formato texto
timestamp_str() ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("~p/~p/~p ~p:~p:~p", [Dia, Mes, Ano, Hora, Min, Seg])).

%% @doc Converte dados Erlang para JSON
json_encode(JSON)->
	jsx:encode(JSON).

%% @doc Converte um JSON para dados Erlang usando map
json_decode_as_map(JSON) ->
	try
		Result = jsx:decode(JSON, [return_maps]),
		{ok, Result}
	catch
		_Exception:Reason -> {error, Reason}
	end.

%% @doc Converte um JSON para dados Erlang
json_decode(JSON) ->
	try
		Result = jsx:decode(JSON),
		{ok, Result}
	catch
		_Exception:Reason -> {error, Reason}
	end.
	
%% @doc Retorna o primeiro item da lista ou vazio
hd_or_empty(List) when length(List) > 0 -> 
	hd(List);

%% @doc Retorna o primeiro item da lista ou vazio	
hd_or_empty(_) -> [].

%% @doc Retorna a string com aspas
% quote(Str) -> [$", Str, $"].

%% @doc Remove o último backslash da string
remove_backslash(List) -> remove_backslash2(List, []).

remove_backslash2([], _) -> [];
remove_backslash2([H|T], L) when T == "/" -> L;
remove_backslash2([H|T], L) -> remove_backslash2(T, [H|L]).




