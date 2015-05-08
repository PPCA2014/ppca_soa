%%---
%%  PPCA_SOA
%%  Arquivo de log
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---

-module(ppca_log).
-export([mensagem_erro/2, 
		 mensagem_info/2, 
		 mensagem_warn/2		
		 ]).

mensagem_erro(Msg, Sistema) ->
	Msg1 = "erro:" ++ timestamp_str() ++ ":" ++ Sistema ++ ":" ++ Msg,
    file:write_file("log.txt", Msg1 ++ "\n", [append]),
	io:format(Msg1).

mensagem_info(Msg, Sistema) ->
	Msg1 = "info:" ++ timestamp_str() ++ ":" ++ Sistema ++ ":" ++ Msg,
	file:write_file("log.txt", Msg1 ++ "\n", [append]),
	io:format(Msg1).

mensagem_warn(Msg, Sistema) ->
	Msg1 = "warn:" ++ timestamp_str() ++ ":" ++ Sistema ++ ":" ++ Msg,
	file:write_file("log.txt", Msg1 ++ "\n", [append]),
	io:format(Msg1).

timestamp_str() ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("~p-~p-~p_~p-~p-~p", [Dia, Mes, Ano, Hora, Min, Seg])).
