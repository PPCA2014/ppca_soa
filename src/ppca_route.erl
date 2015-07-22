%% ---
%%  PPCA_SOA
%%  Modulo responsável por armazenar, remover e recuperar rotas
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Alunos:	   Drausio Gomes dos Santos (drausiogs@gmail.com)
%%             Everton de Vargas Agilar (evertonagilar@gmail.com)
%%---

-module(ppca_route).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0,lookup_route/2]).
-import(string, [sub_string/3]).

-include("../include/ppca_http_messages.hrl").


%% ====================================================================
%% Internal functions
%% ====================================================================
%%-spec add_route(Route::[{route,string()},{module,string()},{function,string()}]) 
%%								-> ok | {error, Reason::string()}.
init() ->
	ppca_logger:info_msg("ppca_rota iniciado."),
	TableRoute = ets:new(routes, [ordered_set]),
	rotas_servico(TableRoute),

	ets:insert(TableRoute,{"/login","test_ppca_route:execute"}),
	ets:insert(TableRoute,{"/route/login","ppca_route:execute"}),
	ets:insert(TableRoute,{"/alunos","test_ppca_route:execute"}),

	% Adicionado por Everton
	ets:insert(TableRoute,{"/", "ppca_info_service:execute"}),	
	ets:insert(TableRoute,{"/info", "ppca_info_service:execute"}),	
	ets:insert(TableRoute,{"/favicon.ico", "ppca_favicon_service:execute"}),	
	ets:insert(TableRoute,{"/catalogo", "ppca_catalogo_service:lista_catalogo"}),

	ets:insert(TableRoute,{"/logs/server.log", "static_file_service:execute"}),
    
    %% serviço do portal %
    ets:insert(TableRoute,{"/portal/login.html", "static_file_service:execute"}),
    ets:insert(TableRoute,{"/portal/static/css/fpc_concat.css", "static_file_service:execute"}),
    ets:insert(TableRoute,{"/portal/static/css/fpc_concat.js", "static_file_service:execute"}),
    %%

  	%%
      %% Rota abaixo para o autenticador inserida por Marçal
	%%
      ets:insert(TableRoute,{"/autentica","ppca_auth_user:autentica"}),
	%% Fim

	loop(TableRoute).


loop(TableRoute) ->
	receive
		{  Client, {From, "PUT",Url="/route"++ _, Payload, _HeadrDict}} ->
			Client ! {self(), update_route(Url, Payload, TableRoute, From)},
			ppca_logger:info_msg("recebendo PUT.");
		{  Client, {_From, _Method="GET", _Url="/route"++ _, _Payload, HeaderDict}}  ->
			ppca_logger:info_msg("recebendo GET."),
			Client ! lookup_route(HeaderDict, TableRoute);			
		{  Client, {From,"POST","/route"++ _, Payload, _HeadrDict}} ->			
			Client ! {self(), add_route(Payload, TableRoute, From)},
			ppca_logger:info_msg("recebendo POST.");
		{  Client, {From, "DELETE",Url="/route"++ _, _Payload, _HeadrDict}} ->
			Client ! {self(), remove_route(Url, TableRoute, From)},
			ppca_logger:info_msg("recebendo DELETE.");
		{ Client, {_From, _Method, _Url, _Payload, HeaderDict}} ->
			Reply = lookup_route(HeaderDict, TableRoute),
			Client ! Reply
	end,
	loop(TableRoute).



-spec add_route(Payload::string(),TableRoute::pid(),From::pid()) -> {route_added,ok} | {error, Reason::string()}.
add_route(Payload,TableRoute,From) ->
	 Key = getUrl(Payload),
	 Value = getModuleFunction(Payload),
	 ppca_logger:info_msg("---"++Key),
	 ets:insert(TableRoute,{Key,Value}),
	 Target = ets:lookup(TableRoute, Key),
	 ModuleFunction = element(2,lists:nth(1, Target)),
	 io:format("~p~n", [ModuleFunction]),
	 %% @todo Incluir rota de forma persistente em ets ou serviço de persistencia
	 {route_added,ok,From}.		
	
%-spec remove_route(Url::string()) -> {route_deleted,ok,Url::string()} | {error, Reason::string()}.
remove_route(Url,TableRoute,From) -> 
	 UrlToRemove = string:sub_string(Url, string:len("/route")+1, string:len(Url)),
	 ets:delete(TableRoute, UrlToRemove),
	 {route_deleted,ok,UrlToRemove,From}.	


%-spec lookup_route(HeaderDict::dict, TableRoute::pid()) -> {service_found, Target:string()} | not_found.
lookup_route(HeaderDict, TableRoute) ->
	Url = dict:fetch("Url", HeaderDict),
	Service = ets:lookup(TableRoute, Url),
	lookup_route_target(Service, Url).

lookup_route_target([], Url) ->
	ErroInterno = io_lib:format(?MSG_SERVICO_NAO_ENCONTRADO, [Url]),
	{error, servico_nao_encontrado, ErroInterno};

lookup_route_target(Service, _Url) when is_map(Service) ->
	{ok, Service};

lookup_route_target(Service, _Url) ->
	ModuleFunction = element(2, lists:nth(1, Service)),
	{ok, ModuleFunction}.

update_route(Url,Payload,TableRoute,From) ->
	Target = ets:lookup(TableRoute, Url),
	ModuleFunction = element(2,lists:nth(1, Target)),
	ets:insert(TableRoute,{Url,getModuleFunction(Payload)}),
	{route_updated,Url,ModuleFunction,From}.

getUrl(_Payload) ->
			%IsJson = jsx:is_json(<<[Payload]>>),
			%io:format("Se JSON:~p~n", [IsJson]),
			%io:format("Se JSON:~p~n", [Payload]),
			%ppca_logger:info_msg("---"++ IsJson ),
			%{ok, Cat} = ppca_util:json_decode_as_map(Payload),
			%Id=maps:get(<<"id">>, Cat),
			%Id.
            "/login".
getModuleFunction(_Payload) ->
			"ppca_login:do_login".


%% @doc Preenche a tabela com as rotas de serviço disponíveis
rotas_servico(TableRoute) ->
	Cat = ppca_catalogo_service:get_catalogo(),
	%ppca_logger:info_msg("Iniciando tabela de rotas:"),
	lists:foreach(fun(S) -> add_rota_servico(S, TableRoute) end, Cat),
	ok.
	
add_rota_servico(S, TableRoute) ->
	Url = ppca_catalogo:get_value(<<"url">>, S),
	{Module, Function} = ppca_catalogo:get_value(<<"service">>, S),
	Rota = #{<<"url">> => Url,
			 <<"module">> => Module,
			 <<"function">> => Function},
	ets:insert(TableRoute, {Url, Rota}).
	%ppca_logger:info_msg("\trota ~p ok.", [Url]).	






