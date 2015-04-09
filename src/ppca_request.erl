%% ---
%%  PPCA_SOA
%%  Tratador de requisições
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%---
-module(ppca_request).

-export([init/0]).

init() ->
	io:format("Request handler carregado.~n"),
	loop().


loop() ->
	receive
		_ -> io:format("message~n", [])
	end,
	loop().
	



