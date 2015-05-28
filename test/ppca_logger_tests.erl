%% ---
%%  PPCA_LOGGER_TESTS
%%  Biblioteca de utilitários
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Aluna: Eliene do Carmo Vieira (elienevie@gmail.com)
%%---

% Test ppca_logger
-module(ppca_logger_tests).


-export([logger_test/0]).

-include_lib("eunit/include/eunit.hrl").

logger_test() ->

	% inicia o módulo ppca_logger
	ppca_logger:start(),
	% mensagem info sem parâmetro
	ppca_logger:info_msg("Mensagem info sem parâmetro."),
	% mensagem info com parâmetro
	ppca_logger:info_msg("Mensagem info ~p.", ["com parâmetro"]),
	% mensagem error sem parâmetro
	ppca_logger:error_msg("Mensagem error sem parâmetro."),
	% mensagem error com parâmetro
	ppca_logger:error_msg("Mensagem error ~p.", ["com parâmetro"]),
	% mensagem warn sem parâmetro
	ppca_logger:warn_msg("Mensagem warn sem parâmetro."),
	% mensagem warn com parâmetro
	ppca_logger:warn_msg("Mensagem warn ~p.", ["com parâmetro"]),
	% mensagem warn sem parâmetro
	ppca_logger:warn_msg(<<"Mensagem warn sem parâmetro.">>),
	% mensagem binária com parâmetro
	ppca_logger:warn_msg(<<"Mensagem warn ~p.">>, [<<"com parâmetro">>]),
	% faz sync para descarregar buffer de forma manual
	ppca_logger:sync().

