%% ---
%%  PPCA_SOA
%%  Publish and subscribe message queue
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Alunos: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%          Drausio Gomes dos Santos (drausiogs@gmail.com)
%%---
-module(ppca_request).

-export([init/0]).

init() ->
	register(ppca_route, spawn(fun() -> ppca_route:init() end)),
	loop().


loop() ->
	
	receive
		{ From, { processa_request, {HeaderDict, Payload}}} ->
			
			processa_servico(From,HeaderDict,Payload),
			loop();
			
		{From,_} -> Response = "OK" ,
				From ! { ok, Response}
	end.
	
	
processa_servico(From, HeaderDict, Payload) ->
	Metodo = dict:fetch("Metodo", HeaderDict),
	Url = dict:fetch("Url", HeaderDict),
	ppca_route ! {self(), {Metodo, Url, Payload,HeaderDict}},
	receive
		{_From1, {route_deleted, ok, UrlToRemove, From}}  -> 
				Response = "serviço | " ++ UrlToRemove ++" | excluido~n",
				From ! { ok, Response};
			
		{_From1, {route_added, ok, From} } -> 
				Response = "serviço adicionado~n",
				From ! { ok, Response};
			
		{_From1, {service_found, HeaderDict, Target}}  -> 
				if  Target == [] ->
					Response = "Not found" ,
					From ! { ok, Response};
				true -> 
					Module = lists:nth(1,string:tokens(Target, ":")),
					Function = lists:nth(2,string:tokens(Target, ":")),
					
					% Apenas envia mensagem para o serviço (não cria outro serviço)
					apply(list_to_atom(Module), list_to_atom(Function), [HeaderDict,From])
					
					% Cria outro serviço e envia mensagem
					%PidModulo = spawn(list_to_atom(Module), list_to_atom(Function), [HeaderDict,From])	
				end;
				
		{_From1, {route_updated, Url, ModuleFunction, From}}  -> 
				Response = "teste passou - rota "++ " atualizada " "para " ++ ModuleFunction ++"~n",
				From ! { ok, Response};
			
		{_From1, {route_normal, _Method, Url}}  -> 
				Response = "Hello" ,
				From ! { ok, Response}
	end.
