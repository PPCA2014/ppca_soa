%% ---
%%  PPCA_SOA
%%  Módulo para manipular uma rota
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Aluno: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%---


-module(ppca_catalogo).

-include("../include/ppca_config.hrl").

-export([lookup_re/2, 
		 new_rota/3, 
		 new_rota_re/3, 
		 get_querystring/2, 
		 get_value/2, test/0]).

get_value(<<"service">>, Cat) ->
	Service = binary_to_list(maps:get(<<"service">>, Cat)),
	[NomeModule, NomeFunction] = string:tokens(Service, ":"),
	Module = list_to_atom(NomeModule),
	Function = list_to_atom(NomeFunction),
	{Module, Function};

get_value(Key, Cat) ->
	V1 = maps:get(Key, Cat),
	case is_binary(V1) of
		true ->	binary_to_list(V1);
		false -> V1
	end.

get_querystring(Cat, <<QueryName/binary>>) ->	
	[Query] = [Q || Q <- get_value(<<"querystring">>, Cat), get_value(<<"comment">>, Q) == QueryName],
	Query.
	
lookup(Url, Table) ->
	case maps:find(Url, Table) of
		{ok, Servico} -> {ok, Servico};
		error -> notfound
	end.

lookup_re(_Url, []) ->
	notfound;

lookup_re(Url, [H|T]) ->
	RE = maps:get(url_re_compiled, H),
	case re:run(Url, RE, [{capture,all_names,binary}]) of
		match -> {ok, H, []};
		{match, Params} -> 
			{namelist, ParamNames} = re:inspect(RE, namelist),
			ParamsMap = lists:zip(ParamNames, Params),
			{ok, H, ParamsMap};
		nomatch -> lookup_re(Url, T);
		{error, _ErrType} -> nofound
	end.

new_rota_re(Url, Module, Function) ->
	{ok, Url_re_compiled} = re:compile(Url),
	Rota = #{url => Url,
			 module => Module,
			 function => Function,
			 url_re_compiled => Url_re_compiled},
	Rota.

new_rota(Url, Module, Function) ->
	Rota = #{url => Url,
			 module => Module,
			 function => Function},
	Rota.

test() ->
	%%  {T1, T2, R1, R2, R3} = rota_table:test().

	R1 = new_rota_re("^/aluno/lista_formandos/(?P<tipo>(sintetico|analitico))$", aluno_service_report, function),
	R2 = new_rota_re("^/portal/[a-zA-Z0-9-_\.]+\.(html|js|css)$", static_file_service, function),
	R4 = new_rota_re("^/portal/", aluno_service_report, function),
	
	R3 = new_rota("/log/server.log", static_file_service, function),
	
	
	T1 = [R1, R2, R4],
	T2 = maps:from_list([{"/log/server.log", R3}]),
	
	io:format("\n\n"),
	
	lookup_re("/aluno/lista_formandos/tipo", T1),
	lookup_re("/portal/index.html", T1),
	lookup("/logs/server.log", T2),

	{T1, T2, R1, R2, R3}.

	
	
	


