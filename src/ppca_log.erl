%%---
%%  PPCA_SOA
%%  Arquivo de log
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---

-module(ppca_log).
-export([mensagem_info/2, 
         mensagem_warn/2,
         mensagem_error/2]).

% Api do modulo
mensagem_warn(Msg, Servico) ->
  mensagem(warn, Msg, Servico).

mensagem_info(Msg, Servico) ->
  mensagem(info, Msg, Servico).

mensagem_error(Msg, Servico) ->
  mensagem(error, Msg, Servico).


% funcoes interna

mensagem(Tipo, Msg, Servico) ->
	Msg1 = atom_to_list(Tipo) ++ ":" ++ timestamp_str() ++ ":" ++ Servico ++ ":" ++ Msg,
    file:write_file("log.txt", Msg1 ++ "\n", [append]),
	io:format(Msg1).

timestamp_str() ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("~p-~p-~p_~p-~p-~p", [Dia, Mes, Ano, Hora, Min, Seg])).
