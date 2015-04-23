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
	loop().


loop() ->
	receive
		{ From, { processa_request, {_Metodo, _Url, _Payload}}} ->
			io:format("fazer lookup tabela de rotas passando metodo e url e retonar rota", []),
			% Response = chamar funcao associada metodo/url
			% enviar mensagem de volta para From
			Response = "hello",
			From ! { ok, Response};
		
		_ -> io:format("message~n", [])
	end,
	loop().
	



