%%********************************************************************
%% @title Módulo ppca_request
%% @version 1.0.0
%% @doc Módulo para manipular o objeto Request de uma requisição HTTP
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(ppca_request).

-export([get_property_request/2, 
		 get_param_url/3,
		 get_querystring/3,
		 encode_request/7]).

-include("../include/ppca_config.hrl").

%% @doc Retorna a URL do request
get_property_request(<<"url">>, Request) ->
	Request#request.url;

%% @doc Retorna a URL do request
get_property_request(<<"metodo">>, Request) ->
	Request#request.metodo;

%% @doc Retorna a URL do request
get_property_request(<<"http_version">>, Request) ->
	Request#request.versao_http;

%% @doc Retorna o payload/body do request
get_property_request(<<"payload">>, Request) ->
	Request#request.payload_map;

%% @doc Retorna o payload/body do request
get_property_request(<<"body">>, Request) ->
	Request#request.payload_map.

%% @doc Retorna um parâmetro do request
get_param_url(NomeParam, Default, Request) ->
	ParamsUrl = Request#request.params_url,
	NomeParam2 = iolist_to_binary(NomeParam),
	Value = maps:get(NomeParam2, ParamsUrl, Default),
	binary_to_list(Value).

%% @doc Retorna uma querystring do request
get_querystring(QueryName, Default, Request) ->
	maps:get(QueryName, Request#request.querystring_map, Default).

%% @doc Gera um objeto request com os dados da requisição
encode_request(RID, Metodo, Url, Versao_HTTP, Querystring, QuerystringMap, HeaderMap) ->
   #request{rid = RID,
			metodo = Metodo,
			url = Url,
			versao_http = Versao_HTTP,
			querystring = Querystring,
			querystring_map = QuerystringMap,
			http_headers = HeaderMap,
			content_length = maps:get("content-length", HeaderMap, 0)
	}.

