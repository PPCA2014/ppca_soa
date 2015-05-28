%% ---
%%  PPCA_SOA_TESTS
%%  Biblioteca de utilitários
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Aluna: Eliene do Carmo Vieira (elienevie@gmail.com)

%%---

% Test ppca_soa
-module(ppca_soa_tests).
-include_lib("eunit/include/eunit.hrl").
ppca_soa_test() ->

	% inicia o barramento e abre um listener na porta default definida no arquivo de configuração
	ppca_soa:start(),
	% inicia outro listener em uma porta específica
	ppca_soa:start_listen(2302),
	% para o listener 2305
	ppca_soa:stop_listen(2302).

