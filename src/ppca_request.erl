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
		{ From, { processa_request, {HeaderDict, Payload}}} ->
			
			processa_servico(From,HeaderDict,Payload),
			loop();
			
		{From,_} -> Response = "OK" ,
				From ! { ok, Response}
	end.
	
	
processa_servico(From, HeaderDict, Payload) ->
	
		Metodo = dict:fetch("Metodo", HeaderDict),
	    Url = dict:fetch("Url", HeaderDict),
		ServManager = spawn(ppca_route, init, []),
		ServManager ! {self(), { Metodo, Url, Payload,HeaderDict}},
			
		receive
		{From1,{route_deleted,ok,UrlToRemove,From}}  -> 
				Response = "servi�o | " ++ UrlToRemove ++" | excluido~n",
				From ! { ok, Response};
			
		{From1,{route_added,ok,From} } -> 
				Response = "servi�o adicionado~n",
				From ! { ok, Response};
			
		{From1,{service_found,HeaderDict,Target}}  -> 
				if  Target == [] ->
            		Response = "Not found" ,
					From ! { ok, Response};
        		true -> 
            		Module = lists:nth(1,string:tokens(Target, ":")),
					Function = lists:nth(2,string:tokens(Target, ":")),
					PidModulo = spawn(list_to_atom(Module), list_to_atom(Function), [HeaderDict,From])	
				end;
			    
		{From1,{route_updated,Url,ModuleFunction,From}}  -> 
				Response = "teste passou - rota "++ " atualizada " "para " ++ ModuleFunction ++"~n",
				From ! { ok, Response};
			
		{From1,{route_normal,Method,Url}}  -> 
				Response = "Hello" ,
				From ! { ok, Response}
		
		
		end.
