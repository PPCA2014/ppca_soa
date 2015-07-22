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

-export([get_property_request/2, 
		 get_param_url/3, 
		 get_querystring/3,
		 encode_request/4]).

-include("../include/ppca_config.hrl").

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
	ppca_route ! {self(), {From, Metodo, Url, Payload, HeaderDict}},
	receive
		{ok, Target}  when Metodo == "GET"-> 
			processa_servico(Target, From, [HeaderDict, From]);
		{ok, Target}  when Metodo == "POST"-> 
			processa_servico(Target, From, [HeaderDict, Payload, From]);
		{ok, Target}  when Metodo == "PUT"-> 
			processa_servico(Target, From, [HeaderDict, Payload, From]);
		{ok, Target}  when Metodo == "DELETE"-> 
			processa_servico(Target, From, [HeaderDict, From]);
		{error, servico_nao_encontrado, ErroInterno} -> 
			From ! {error, servico_nao_encontrado, ErroInterno}
	end.

%% @doc Processa o serviço solicitado
processa_servico(Target, From, Params) ->
	case executa_servico(Target, Params) of
		em_andamento -> ok;	%% o serviço se encarrega de enviar mensagem quando estiver pronto
		Error -> From ! Error
	end.

%% @doc Executa o serviço de forma assíncrona invocando o módulo correto
executa_servico(Target, Params) when is_map(Target) ->
	Module = ppca_catalogo:get_value(<<"module">>, Target),
	Function = ppca_catalogo:get_value(<<"function">>, Target),
	executa_servico_registrado(Module, Function, Params);

%% @doc Executa o serviço de forma assíncrona invocando o módulo correto
executa_servico(Target, Params) ->
	[NomeModule, NomeFunction] = string:tokens(Target, ":"),
	Module = list_to_atom(NomeModule),
	Function = list_to_atom(NomeFunction),
	executa_servico_registrado(Module, Function, Params).
	
executa_servico_registrado(Module, Function, Params) ->
	try
		case whereis(Module) of
			undefined -> 
				Module:start(),
				apply(Module, Function, Params);
			Pid -> 
				apply(Module, Function, Params)
		end,
		em_andamento
	catch
		_Exception:ErroInterno ->  {error, servico_falhou, ErroInterno}
	end.

%% @doc Retorna a URL do request
get_property_request(<<"url">>, Request) ->
	dict:fetch("Url", Request#request.http_headers);

%% @doc Retorna o payload do request
get_property_request(<<"payload">>, Request) ->
	Request#request.payload.

%% @doc Retorna um parâmetro do request
get_param_url(NomeParam, Default, Request) ->
	ParamsUrl = Request#request.params_url,
	NomeParam2 = iolist_to_binary(NomeParam),
	Value = maps:get(NomeParam2, ParamsUrl, Default),
	binary_to_list(Value).

%% @doc Retorna uma querystring do request
get_querystring(QueryName, Default, Request) ->
	HttpHeaders = Request#request.http_headers,
	case dict:find("Query", HttpHeaders) of
		{ok, Query} -> maps:get(QueryName, Query, Default);
		error ->  Default
	end.


%% @doc Gera um objeto request com os dados da requisição
encode_request(HeaderDict, Payload, Servico, ParamsUrl) ->
	Request = #request{http_headers = HeaderDict,
					   payload = Payload,
					   servico = Servico,
					   params_url = ParamsUrl},
	Request.


