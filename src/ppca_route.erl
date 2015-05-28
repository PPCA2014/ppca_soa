%% ---
%%  PPCA_SOA
%%  Modulo responsável por armazenar, remover e recuperar rotas
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Alunos:	   Drausio Gomes dos Santos (drausiogs@gmail.com)
%%             Everton de Vargas Agilar (evertonagilar@gmail.com)
%%             Eliene do Carmo Vieira	(elienev@gmail.com) 
%%---

-module(ppca_route).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0,lookup_route/2,execute/2]).
-import(string, [sub_string/3]).

-include("../include/http_messages.hrl").


%% ====================================================================
%% Internal functions
%% ====================================================================
%%-spec add_route(Route::[{route,string()},{module,string()},{function,string()}]) 
%%								-> ok | {error, Reason::string()}.
init() ->
	ppca_logger:info_msg("ppca_rota iniciado."),
	TableRoute=dets:open_file(routes, [{type,set}]),
	%% @todo Recuperar todas as rotas do serviço de persistencia e
	%% e incluir no ets routes.
	%% Substituir abaixo
	rotas_servico(TableRoute),

	dets:insert(routes,{"/login","test_ppca_route:execute"}),
	dets:insert(routes,{"/route/login","ppca_route:execute"}),
	dets:insert(routes,{"/alunos","test_ppca_route:execute"}),

	% Adicionado por Everton
	dets:insert(routes,{"/", "ppca_info_service:execute"}),	
	dets:insert(routes,{"/info", "ppca_info_service:execute"}),	
	dets:insert(routes,{"/favicon.ico", "ppca_favicon_service:execute"}),	
	dets:insert(routes,{"/hello_world", "helloworld_service:execute"}),
	dets:insert(routes,{"/catalogo", "ppca_catalogo_service:lista_catalogo"}),


  	%%
      %% Rota abaixo para o autenticador inserida por Marçal
	%%
      dets:insert(routes,{"/autentica","ppca_auth_user:autentica"}),
	%% Fim

	loop(TableRoute).


loop(TableRoute) ->
	receive
		{  Client, {From, "PUT",Url="/route"++ _, Payload,HeadrDict}} ->
			Client ! {self(), update_route(Url,Payload,TableRoute,From)},
			ppca_logger:info_msg("recebendo PUT.");
		{  Client, {From,Method="GET",Url="/route"++ _, Payload,HeaderDict}}  ->
			ppca_logger:info_msg("recebendo GET."),
			Client ! lookup_route(HeaderDict, TableRoute);			
		{  Client, {From,"POST","/route"++ _, Payload,HeadrDict}} ->			
			Client ! {self(), add_route(Payload,TableRoute,From)},
			ppca_logger:info_msg("recebendo POST.");
		{  Client, {From, "DELETE",Url="/route"++ _, Payload,HeadrDict}} ->
			Client ! {self(), remove_route(Url,TableRoute,From)},
			ppca_logger:info_msg("recebendo DELETE.");
		{ Client, {From,Method, Url, Payload,HeaderDict}} ->
			ppca_logger:info_msg("looking for service."),
			Reply = lookup_route(HeaderDict, TableRoute),
			Client ! Reply
	end,
	loop(TableRoute).



-spec add_route(Payload::string(),TableRoute::pid(),From::pid()) -> {route_added,ok} | {error, Reason::string()}.
add_route(Payload,TableRoute,From) ->
	 Key = getUrl(Payload),
	 Value = getModuleFunction(Payload),
	 ppca_logger:info_msg("---"++Key),
	 dets:insert(routes,{Key,Value}),
	 Target = dets:lookup(routes, Key),
	 ModuleFunction = element(2,lists:nth(1, Target)),
	 io:format("~p~n", [ModuleFunction]),
	 %% @todo Incluir rota de forma persistente em dets ou serviço de persistencia
	 {route_added,ok,From}.		
	
%-spec remove_route(Url::string()) -> {route_deleted,ok,Url::string()} | {error, Reason::string()}.
remove_route(Url,TableRoute,From) -> 
	 UrlToRemove = string:sub_string(Url, string:len("/route")+1, string:len(Url)),
	 dets:delete(routes, UrlToRemove),
	 {route_deleted,ok,UrlToRemove,From}.	


%-spec lookup_route(HeaderDict::dict, TableRoute::pid()) -> {service_found, Target:string()} | not_found.
lookup_route(HeaderDict, TableRoute) ->
	Url = dict:fetch("Url", HeaderDict),
	Target = dets:lookup(routes, Url),
	io:format("URL. ~p~n", [Target]),
	if  Target == [] ->
            ErroInterno = io_lib:format(?MSG_SERVICO_NAO_ENCONTRADO, [Url]),
            {error, servico_nao_encontrado, ErroInterno};
        true -> 
			ModuleFunction = element(2, lists:nth(1, Target)),
			{ok, ModuleFunction}
	end.
	


update_route(Url,Payload,TableRoute,From) ->
	Target = dets:lookup(routes, Url),
	ModuleFunction = element(2,lists:nth(1, Target)),
	dets:insert(routes,{Url,getModuleFunction(Payload)}),
	{route_updated,Url,ModuleFunction,From}.

getUrl(Payload) ->
			%IsJson = jsx:is_json(<<[Payload]>>),
			%io:format("Se JSON:~p~n", [IsJson]),
			%io:format("Se JSON:~p~n", [Payload]),
			%ppca_logger:info_msg("---"++ IsJson ),
			%{ok, Cat} = ppca_util:json_decode_as_map(Payload),
			%Id=maps:get(<<"id">>, Cat),
			%Id.
            "/login".
getModuleFunction(Payload) ->
			"ppca_login:do_login".

execute(HeaderDict,From) ->
	Metodo = dict:fetch("Metodo", HeaderDict),
	Url = dict:fetch("Url", HeaderDict),
	Target = dets:lookup(routes, Url),
	ModuleFunction = element(2,lists:nth(1, Target)),
	Response= ppca_util:json_encode([{<<"id">>,Url},{<<"servico">>,ModuleFunction}]),
	From ! { ok, Response},
	ppca_logger:info_msg("rota atingida " ++ Url ++" metodo "++ Metodo ).
	



%% @doc Preenche a tabela com as rotas de serviço disponíveis
rotas_servico(TableRoute) ->
	%ListaCatalogo = ppca_catalogo_service:get_catalogo(),
	%lists:foreach(fun(S) -> add_rota_servico(S, TableRoute) end, ListaCatalogo),
	ok.
	
add_rota_servico(Cat, TableRoute) ->
	io:format("Rota \n").






