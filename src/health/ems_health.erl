%%********************************************************************
%% @title Módulo ems_health
%% @version 1.0.0
%% @doc Disponibiliza informações da saúde do servidor
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_health).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% Client API
-export([start/0,
		 get_top_services/3, 
		 get_top_services_by_type/3, 
		 get_qtd_requests_by_date/3,
		 groupBy/2, 
		 get_requests_periodo/1, 
		 get_requests_periodo_by_group/2,
		 count/3,
		 select/0]).


 
start() -> create_cache_req_sub().
    
%% @doc Retorna a lista de requisições de um período
get_requests_periodo(Periodo) ->
	ems_cache:get(health_req_sub, 60000, Periodo, 
					fun() -> 
						ems_request:get_requests_periodo(Periodo)
					end).

%% @doc Retorna a lista de requisições de um período agrupado por FieldsGroup
get_requests_periodo_by_group(Periodo, FieldsGroup) ->
	Requests = get_requests_periodo(Periodo),
	maps:keys(groupBy(FieldsGroup, Requests)).

%% @doc Retorna os serviços mais acessados de um período
get_top_services(Top, Periodo, Sort) ->
    Fields = fun(X) -> case X#request.service of
						  undefined -> {<<"não encontrado"/utf8>>};
						  _ -> {X#request.service#service.name} 
					   end
			 end,
	Requests = get_requests_periodo(Periodo), 
	Urls = maps:keys(groupBy(Fields, Requests)),
	Urls2 = count(Fields, Urls, Requests),
	case Sort of
		"url" -> Urls3 = sort_first(Urls2);
		"qtd" -> Urls3 = sort_last(Urls2)
	end,
	top(Urls3, Top).
	
%% @doc Retorna os serviços mais acessados por tipo de verbo de um período
get_top_services_by_type(Top, Periodo, Sort) ->
    Fields = fun(X) -> case X#request.service of
							undefined -> {"?", <<"não encontrado"/utf8>>};
							_ -> {X#request.service#service.type, 
								  X#request.service#service.name} 
					   end
			 end,
	Requests = get_requests_periodo(Periodo), 
	Urls = maps:keys(groupBy(Fields, Requests)),
	Urls2 = count(Fields, Urls, Requests),
	case Sort of
		"url" -> Urls3 = sort_first(Urls2);
		"qtd" -> Urls3 = sort_last(Urls2)
	end,
	top(Urls3, Top).

%% @doc Retorna a quantidade de requisições por data de um período
get_qtd_requests_by_date(Top, Periodo, Sort) ->
    Fields = fun(X) -> {ems_util:date_to_string(X#request.timestamp)} end,
	Requests = get_requests_periodo(Periodo),
	Requests1 = maps:keys(groupBy(Fields, Requests)),
	Requests2 = count(Fields, Requests1, Requests),
	case Sort of
		"date" -> Requests3 = sort_first(Requests2);
		"qtd"  -> Requests3 = sort_last(Requests2)
	end,
	top(Requests3, Top).

	
groupBy(F, L) -> lists:foldr(fun({K,V}, D) -> maps:put(K, V, D) end , maps:new(), [ {F(X), X} || X <- L ]).
	
count(F, G, L) -> 
	CountFunc = fun(Fn, X) -> length([V || V <- L, Fn(V) == X]) end,
	[ erlang:tuple_to_list(X) ++ [CountFunc(F, X)] || X <- G].
	
sort_last(L) -> lists:sort(fun(X, Y) -> lists:last(X) >= lists:last(Y) end, L).

sort_first(L) -> lists:sort(fun(X, Y) -> hd(X) >= hd(Y) end, L).
	
top(L, T) -> 
	lists:sublist(L, T).

select() ->
	Query = fun() ->
		  qlc:e(
			 qlc:q([X || X <- mnesia:table(request)])
		  )
	   end,
	{atomic, Requests} = mnesia:transaction(Query),
	{ok, Requests}.

create_cache_req_sub() ->
	try
		ems_cache:new(health_req_sub)
	catch
		_Exception:_Reason ->  ok
	end.

