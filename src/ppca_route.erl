%% ---
%%  PPCA_SOA
%%  Modulo responsável por armazenar, remover e recuperar rotas
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Alunos:    Drausio Gomes dos Santos (drausiogs@gmail.com)
%%			   Everton de Vargas Agilar (evertonagilar@gmail.com)
%%             Eliene do Carmo Vieira	(elienev@gmail.com) 
%%---

-module(ppca_route).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0,lookup_route/2,execute/2, load_catalogo/0, lista_catalogo/2]).
-import(string, [sub_string/3]).


%% ====================================================================
%% Internal functions
%% ====================================================================
%%-spec add_route(Route::[{route,string()},{module,string()},{function,string()}]) 
%%								-> ok | {error, Reason::string()}.
init() ->
	ppca_logger:info_msg("ppca_rota iniciado."),
	TableRoute=ets:new(routes, [ordered_set]),
	%% @todo Recuperar todas as rotas do serviço de persistencia e
	%% e incluir no ets routes.
	%% Substituir abaixo
	ets:insert(TableRoute,{"/login","test_ppca_route:execute"}),
	ets:insert(TableRoute,{"/route/login","ppca_route:execute"}),
	ets:insert(TableRoute,{"/alunos","test_ppca_route:execute"}),

	% Adicionado por Everton
	ets:insert(TableRoute,{"/", "info_service:execute"}),	
	ets:insert(TableRoute,{"/info", "info_service:execute"}),	
	ets:insert(TableRoute,{"/hello_world", "helloworld_service:execute"}),
	ets:insert(TableRoute,{"/catalogo", "ppca_route:lista_catalogo"}),


  	%%
      %% Rota abaixo para o autenticador inserida por Marçal
	%%
      ets:insert(TableRoute,{"/autentica","ppca_auth_user:autentica"}),
	%% Fim


	% Inicializa os serviços
	info_service:start(),
	helloworld_service:start(),
	
	loop(TableRoute).


loop(TableRoute) ->
	receive
		{  Client, {Router,From, "PUT",Url="/route"++ _, Payload,HeadrDict}} ->
			Client ! {self(), update_route(Url,Payload,TableRoute,From)},
			ppca_logger:info_msg("recebendo PUT.");
		{  Client, {Method="GET",Url="/route"++ _, Payload,HeaderDict}}  ->
			ppca_logger:info_msg("recebendo GET."),
			Client ! {self(), lookup_route(HeaderDict,TableRoute)};			
		{  Client, {Router,From, "POST","/route"++ _, Payload,HeadrDict}} ->			
			Client ! {self(), add_route(Payload,TableRoute,From)},
			ppca_logger:info_msg("recebendo POST.");
		{  Client, {Router,From, "DELETE",Url="/route"++ _, Payload,HeadrDict}} ->
			Client ! {self(), remove_route(Url,TableRoute,From)},
			ppca_logger:info_msg("recebendo DELETE.");
		{ Client, {Method, Url, Payload,HeaderDict}} ->
			ppca_logger:info_msg("looking for service."),
			Client ! {self(), lookup_route(HeaderDict,TableRoute)}
 			
			
			
		
	end,
	loop(TableRoute).



-spec add_route(Payload::string(),TableRoute::pid(),From::pid()) -> {route_added,ok} | {error, Reason::string()}.
add_route(Payload,TableRoute,From) ->
	 Key = getUrl(Payload),
	 Value = getModuleFunction(Payload),
	 ets:insert(TableRoute,{Key,Value}),
	 Target = ets:lookup(TableRoute, Key),
	 ModuleFunction = element(2,lists:nth(1, Target)),
	 io:format("~p~n", [ModuleFunction]),
	 %% @todo Incluir rota de forma persistente em dets ou serviço de persistencia
	 {route_added,ok,From}.		
	
%-spec remove_route(Url::string()) -> {route_deleted,ok,Url::string()} | {error, Reason::string()}.
remove_route(Url,TableRoute,From) -> 
	 UrlToRemove = string:sub_string(Url, string:len("/route")+1, string:len(Url)),
	 ppca_logger:info_msg(UrlToRemove ++ " URL."),
	 TargetA = ets:lookup(TableRoute, UrlToRemove),
	 io:format("Antes exclusao ~p~n", [TargetA]),
	 ets:delete(TableRoute, UrlToRemove),
	 TargetD = ets:lookup(TableRoute, UrlToRemove),
	 io:format("Depois exclusao ~p~n", [TargetD]),
	  %% @todo Remover rota de forma persistente em dets ou serviço de persistencia
	 {route_deleted,ok,UrlToRemove,From}.	



lookup_route(HeaderDict,TableRoute) ->
	Target = ets:lookup(TableRoute, dict:fetch("Url", HeaderDict)),
	io:format(" URL. ~p~n", [Target]),
	if  Target == [] ->
            ModuleFunction = "";
        true -> 
            ModuleFunction = element(2,lists:nth(1, Target))
	end,
	{service_found,HeaderDict,ModuleFunction}.


update_route(Url,Payload,TableRoute,From) ->
	Target = ets:lookup(TableRoute, Url),
	ModuleFunction = element(2,lists:nth(1, Target)),
	ets:insert(TableRoute,{Url,getModuleFunction(Payload)}),
	{route_updated,Url,ModuleFunction,From}.

getUrl(Payload) ->
			"/login".

getModuleFunction(Payload) ->
			"ppca_login:do_login".

execute(HeaderDict,From) ->
	Metodo = dict:fetch("Metodo", HeaderDict),
	Url = dict:fetch("Url", HeaderDict),
	%Response= ppca_util:json_encode([{<<"id">>,<<"Url">>}]),
	Response = "url "++ Url ++" roteada  ",
	From ! { ok, Response},
	ppca_logger:info_msg("rota atingida " ++ Url ++" metodo "++ Metodo ).
	

load_catalogo() ->
	{ok, Dados} = file:read_file("./conf/catalogo.json"),
	{ok, Cat} = ppca_util:json_decode_as_map(Dados),
	Cat.

%% @doc Serviço que lista o catálogo no browser (URL: /catalogo)
lista_catalogo(_HeaderDict, From) ->
	Response = load_catalogo(),
	From ! { ok, Response}.


% Eliene, como acessar um campo
%  maps:get(<<"querystring">>, Cat).

	

	
