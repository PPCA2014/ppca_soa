%% @author Drausio
%% @doc  Modulo responsável por armazenar, remover e recuperar.


-module(ppca_route).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0,lookup_route/3,execute/2]).
-import(string, [sub_string/3]).


%% ====================================================================
%% Internal functions
%% ====================================================================
%%-spec add_route(Route::[{route,string()},{module,string()},{function,string()}]) 
%%								-> ok | {error, Reason::string()}.
init() ->
	io:format("Modulo de rotas carregado.~n"),
	TableRoute=ets:new(routes, [ordered_set]),
	%% @todo Recuperar todas as rotas do serviço de persistencia e
	%% e incluir no ets routes.
	%% Substituir abaixo
	ets:insert(TableRoute,{"/login","test_ppca_route:execute"}),
	ets:insert(TableRoute,{"/route/login","ppca_route:execute"}),
	ets:insert(TableRoute,{"/alunos","test_ppca_route:execute"}),
	%% Fim
	loop(TableRoute).


loop(TableRoute) ->
	receive
		{  Client, { test_route, {Router, "POST",Url="/route"++ _, Payload} } } ->
			Client ! {self(), update_route(Url,Payload,TableRoute)},
			io:format("Recebendo POST.~n");
		{  Client, { test_route, {Router, Method="GET",Url="/route"++ _, Payload} } } ->
			io:format("Recebendo GET.~n"),
			Client ! {self(), lookup_route(Url,Method,TableRoute)};			
		{  Client, { test_route, {Router, "PUT","/route"++ _, Payload} } } ->			
			Client ! {self(), add_route(Payload,TableRoute)},
			io:format("Recebendo PUT.~n");
		{  Client, { test_route, {Router, "DELETE",Url="/route"++ _, Payload} } } ->
			Client ! {self(), remove_route(Url,TableRoute)},
			io:format("Recebendo DELETE.~n");
		{ Client, { test_route, {Router, Method, Url, Payload} } } ->
			Client ! {self(), lookup_route(Url, Method,TableRoute)},
 			io:format("Localizando Rota.~n")
			
			
		
	end,
	loop(TableRoute).



-spec add_route(Payload::string(),TableRoute::pid()) -> {route_added,ok} | {error, Reason::string()}.
add_route(Payload,TableRoute) ->
	 Key = getUrl(Payload),
	 Value = getModuleFunction(Payload),
	 ets:insert(TableRoute,{Key,Value}),
	 Target = ets:lookup(TableRoute, Key),
	 ModuleFunction = element(2,lists:nth(1, Target)),
	 io:format("~p~n", [ModuleFunction]),
	 %% @todo Incluir rota de forma persistente em dets ou serviço de persistencia
	 {route_added,ok}.		
	
%-spec remove_route(Url::string()) -> {route_deleted,ok,Url::string()} | {error, Reason::string()}.
remove_route(Url,TableRoute) -> 
	 UrlToRemove = string:sub_string(Url, string:len("/route")+1, string:len(Url)),
	 io:format(UrlToRemove ++ " URL.~n"),
	 TargetA = ets:lookup(TableRoute, UrlToRemove),
	 io:format("Antes exclusao ~p~n", [TargetA]),
	 ets:delete(TableRoute, UrlToRemove),
	 TargetD = ets:lookup(TableRoute, UrlToRemove),
	 io:format("Depois exclusao ~p~n", [TargetD]),
	  %% @todo Remover rota de forma persistente em dets ou serviço de persistencia
	 {route_deleted,ok,UrlToRemove}.	



lookup_route(Url,Method,TableRoute) ->
	Target = ets:lookup(TableRoute, Url),
	ModuleFunction = element(2,lists:nth(1, Target)),
	{route_founded,Method,Url,ModuleFunction}.


update_route(Url,Payload,TableRoute) ->
	Target = ets:lookup(TableRoute, Url),
	ModuleFunction = element(2,lists:nth(1, Target)),
	ets:insert(TableRoute,{Url,getModuleFunction(Payload)}),
	{route_updated,Url,ModuleFunction}.

getUrl(Payload) ->
			"/login".

getModuleFunction(Payload) ->
			"ppca_login:do_login".

execute(Url,Method) ->
	io:format("rota atingida " ++ Url ++" metodo "++ Method ++"~n").