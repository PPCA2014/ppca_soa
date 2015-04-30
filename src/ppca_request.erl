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
			
			processa_servico(From,_Metodo, _Url, _Payload);
			
		_ -> io:format("message~n", [])
	end.
	%loop().
	
processa_servico(From,_Metodo, _Url, _Payload) ->
	
		ServManager = spawn(ppca_route, init, []),
		ServManager ! {self(), { test_route, { _Metodo, _Url, _Payload}}},
			
		receive
		{From1,{route_deleted,ok,UrlToRemove,From}}  -> 
				Response = "servi�o | " ++ UrlToRemove ++" | excluido~n",
				From ! { ok, Response};
			
		{From1,{route_added,ok,From} } -> 
				Response = "servi�o adicionado~n",
				From ! { ok, Response};
			
		{From1,{route_founded,Method,Url,Target}}  ->
				io:format("par ", []),		
				Module = lists:nth(1,string:tokens(Target, ":")),
				Function = lists:nth(2,string:tokens(Target, ":")),
				PidModulo = spawn(list_to_atom(Module), list_to_atom(Function), [Url,Method,From]);	
				
		{From1,{route_updated,Url,ModuleFunction,From}}  -> 
				Response = "teste passou - rota "++ " atualizada " "para " ++ ModuleFunction ++"~n",
				From ! { ok, Response};
			
		{From1,{route_normal,Method,Url}}  -> 
				Response = "Hello" ++"~n",
				From ! { ok, Response}
		end.
