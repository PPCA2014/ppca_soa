%% @author Claudia
%% @doc @todo Add description to test_ppca_route.


-module(test_ppca_route).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test_route/0,test_route/1,test_lookup/1,execute/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

test_route() ->
	io:format("Iniciando testes~n"),
	Router = spawn(ppca_route, init, []),
	Url="/route/login",
	Payload="{route:{url:/login,modulo:ppca_login,funcao:login}}",
	Router ! {self(), { test_route, {Router, "PUT", Url, Payload}}},
	%Router ! {self(), { test_route, {Router, "GET", Url, Payload}}},
	%Router ! {self(), { test_route, {Router, "POST", Url, Payload}}},
	%Router ! {self(), { test_route, {Router, "GET", Url, Payload}}},
	%Router = spawn(ppca_route, init, []),
	Router ! {self(), { test_route, {Router, "DELETE", Url, Payload}}},
	%Router ! {self(), { test_route, {Router, "GET", Url, Payload}}},
	receive
		{Router,{route_deleted,ok,UrlToRemove}}  -> 
				io:format("Teste passou  - rota | " ++ UrlToRemove ++" | excluida~n");
		{Router,{route_added,ok} } -> io:format("Teste passou - rota adicionada~n");
		{Router,{route_founded,ok}}  -> io:format("Teste passou - rota localizada~n");
		{Router,{route_updated,ok}}  -> io:format("Teste passou - rota atualizada~n");
		{Router,{_,ok} } -> io:format("Teste passou - rota distribuida~n")
	end.

test_route(Method) ->
	io:format("Iniciando testes~n"),
	Router = spawn(ppca_route, init, []),
	Url="/route/login",
	Payload="{route:{url:/login,modulo:ppca_login,funcao:login}}",
	Router ! {self(), { test_route, {Router, Method, Url, Payload}}},
	receive
		{Router,{route_deleted,ok,UrlToRemove}}  -> 
				io:format("teste passou  - rota | " ++ UrlToRemove ++" | excluida~n");
		{Router,{route_added,ok} } -> io:format("teste passou - rota adicionada~n");
		{Router,{route_founded,Method,Url,Target}}  ->
				Module = lists:nth(1,string:tokens(Target, ":")),
				Function = lists:nth(2,string:tokens(Target, ":")),
				PidModulo = spawn(list_to_atom(Module), list_to_atom(Function), [Url,Method]),	
				io:format("teste passou - url "++ Url ++"roteada para "++ Target++"~n");
		{Router,{route_updated,Url,ModuleFunction}}  -> 
			io:format("teste passou - rota "++ " atualizada " "para " ++ ModuleFunction ++"~n");
		{Router,{_,ok} } -> io:format("Teste passou - rota distribuida~n")
	end.

test_lookup(Url) ->
	io:format("Iniciando teste lookup~n"),
	Router = spawn(ppca_route, init, []),
	Router ! {self(), { test_route, {Router, "GET", Url, "Payload"}}},
	receive
		{Router,{route_founded,Method,Url,Target}}  -> 
			%%io:format("rx "++ Target ++"~n"),
			Module = lists:nth(1,string:tokens(Target, ":")),
			Function = lists:nth(2,string:tokens(Target, ":")),
			%%io:format("modulo "++ Module ++"~n"),
			PidModulo = spawn(list_to_atom(Module), list_to_atom(Function), [Url,Method]),				
			io:format("teste passou - url "++ Url ++" roteada para "++ Target++"~n")		
	end.

execute(Url,Method) ->
	io:format("rota atingida " ++ Url ++" metodo "++ Method ++"~n").		