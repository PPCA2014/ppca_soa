%% ---
%%  PPCA_SOA_TESTS
%%  Biblioteca de utilitários
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Alunos: Eliene do Carmo Vieira (elienevie@gmail.com)
%%			Everton de Vargas Agilar (evertonagilar@gmail.com)
%%---

% Test ppca_soa
-module(ppca_soa_tests).

-export([start/0]).

-include_lib("eunit/include/eunit.hrl").

%% @doc start todos os mdulos de testes
start() ->
	io:format("\n\n********** TESTE PPCA_SOA **********\n"),


	% inicia o barramento e abre um listener na porta default definida no arquivo de configuração
	ppca_soa:start(),

	db_tests:start().


	%io:format("\n\n********** TESTE PPCA_SOA **********\n"),
	%ppca_soa_test(),
	%eunit:test(ppca_soa),


	%io:format("\n\n********** TESTE PPCA_LOGGER **********\n\n"),
	%ppca_logger_tests:logger_test().
	%eunit:test(ppca_logger).
	% outros aqui
	% ppca_logger_tests:ppca_logger_test() 


ppca_soa_test() ->
	ok.
	
	% inicia outro listener em uma porta específica
	%ppca_soa:start_listen(2302),
	% para o listener 2305
	%ppca_soa:stop_listen(2302).

