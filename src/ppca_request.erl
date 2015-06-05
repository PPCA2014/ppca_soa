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
			processa_request(From, HeaderDict, Payload),
			loop();
		{From,_} -> Response = "OK" ,
				From ! { ok, Response}
	end.

%% @doc Processa a requisição solicitada
processa_request(From, HeaderDict, Payload) ->
	Metodo = dict:fetch("Metodo", HeaderDict),
	Url = dict:fetch("Url", HeaderDict),
	ppca_route ! {self(), {From,Metodo, Url, Payload, HeaderDict}},
	receive
		{_From1, {route_deleted, ok, UrlToRemove, From}}  -> 
			Response = "serviço | " ++ UrlToRemove ++" | excluido~n",
			From ! { ok, Response};
		{_From1, {route_added, ok, From} } -> 
			Response = "serviço adicionado~n",
			From ! { ok, Response};
		{ok, Target}  when Metodo == "GET"-> 
			processa_servico(Target, From, [HeaderDict, From]);
		{ok, Target}  when Metodo == "POST"-> 
			processa_servico(Target, From, [HeaderDict, Payload, From]);
		{ok, Target}  when Metodo == "PUT"-> 
			processa_servico(Target, From, [HeaderDict, Payload, From]);
		{ok, Target}  when Metodo == "DELETE"-> 
			processa_servico(Target, From, [HeaderDict, From]);
		{error, servico_nao_encontrado, ErroInterno} -> 
			From ! {error, servico_nao_encontrado, ErroInterno};
		{_From1, {route_updated, Url, ModuleFunction, From}}  -> 
			Response = "teste passou - rota "++ " atualizada " "para " ++ ModuleFunction ++"~n",
			From ! { ok, Response};
		{_From1, {route_normal, _Method, Url}}  -> 
			Response = "Hello" ,
			From ! { ok, Response}
	end.

%% @doc Processa o serviço solicitado
processa_servico(Target, From, Params) ->
	case executa_servico(Target, Params) of
		em_andamento -> ok;	%% o serviço se encarrega de enviar mensagem quando estiver pronto
		Error -> From ! Error
	end.

%% @doc Executa o serviço de forma assíncrona invocando o módulo correto
executa_servico(Target, Params) ->
	[NomeModule, NomeFunction] = string:tokens(Target, ":"),
	Module = list_to_atom(NomeModule),
	Function = list_to_atom(NomeFunction),
	try
		apply(Module, Function, Params),
		em_andamento
	catch
		_Exception:ErroInterno ->  {error, servico_falhou, ErroInterno}
	end.

