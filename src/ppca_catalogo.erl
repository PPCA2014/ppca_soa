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

-export([get_comment/1, 
		 get_owner/1,
		 get_version/1, 
		 get_url/1, 
		 get_async/1, 
		 get_url_callback/1, 
		 get_type/1,
		 get_querystring/1,
		 get_querystring/2,
		 get_querystring_comment/1,
		 get_querystring_name/1,
		 get_querystring_type/1]).

get_comment(Cat) ->	
	maps:get(<<"comment">>, Cat).
	
get_owner(Cat) ->	
	maps:get(<<"owner">>, Cat).
	
get_version(Cat) ->	
	maps:get(<<"version">>, Cat).
	
get_url(Cat) ->	
	maps:get(<<"url">>, Cat).
	
get_async(Cat) ->	
	maps:get(<<"async">>, Cat).
	
get_url_callback(Cat) ->	
	maps:get(<<"url_callback">>, Cat).
	
get_type(Cat) ->	
	maps:get(<<"type">>, Cat).
	
get_querystring(Cat) ->	
	maps:get(<<"querystring">>, Cat).
	
get_querystring(Cat, <<QueryName/binary>>) ->	
	[Query] = [Q || Q <- get_querystring(Cat), get_querystring_name(Q) == QueryName],
	Query.
	
get_querystring_comment(Query) ->
	maps:get(<<"comment">>, Query).
	
get_querystring_name(Query) ->
	maps:get(<<"name">>, Query).
	
get_querystring_type(Query) ->
	maps:get(<<"type">>, Query).
	

	
	
	


