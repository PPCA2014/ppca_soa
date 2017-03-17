%%********************************************************************
%% @title Module ems_web_service_correios
%% @version 1.1.0
%% @doc It provides information about location.
%% @author Renato Carauta Ribeiro <rcarauta6@gmail.com>
%% @copyright ErlangMS Team
%%********************tn************************************************

-module(ems_web_service_correios).

-include("../include/ems_schema.hrl").
-include("../include/ems_config.hrl").

-export([busca_cep/1]).
  
busca_cep(Request = #request{service = #service{properties = Props}}) -> 
	Cep = ems_request:get_param_url(<<"id">>, 0, Request),
	UrlCorreio = binary_to_list(maps:get(<<"url_correio">>, Props, <<>>)),
	ContentType = binary_to_list(maps:get(<<"content_type">>,Props,<<>>)),
	UrlBuscaCep =  lists:concat([UrlCorreio]), 
	Body = lists:concat(["relaxation=",Cep,"&tipoCEP=ALL&semelhante=N"]), 
	case httpc:request(post, {UrlBuscaCep, [], ContentType, Body}, [], []) of
		{ok, {_, _, Result}} ->

			PositionInitTable = string:str(Result,"<table class=\"tmptabela\""),
			Table = string:sub_string(Result, PositionInitTable, 12495),
			PosInitTds = string:str(Table, "</tr>"),
			Tds = string:sub_string(Table,PosInitTds+10),
			TdEndIni = string:str(Tds,"<td width=\"150\">"),
			TdEndEnd = string:str(Tds,"</td>"),
			Endereco = string:sub_string(Tds,TdEndIni+17,TdEndEnd-7),
			TdsSemEndereco = string:sub_string(Tds,TdEndEnd+3),
			TdBairroIni = string:str(TdsSemEndereco,"<td width=\"90\">"),
			TdBairroEnd = string:str(TdsSemEndereco,"</td>"),
			Bairro = string:sub_string(TdsSemEndereco,TdBairroIni+15,TdBairroEnd-7),
			TdsSemBairro = string:sub_string(TdsSemEndereco,TdBairroEnd+3),
			TdLogradouroIni = string:str(TdsSemBairro,"<td width=\"80\">"),
			TdLogradouroEnd = string:str(TdsSemBairro,"</td>"),
			Logradouro = string:sub_string(TdsSemBairro,TdLogradouroIni+15,TdLogradouroEnd-4),
			
			{ok, Request#request{code = 200, 
								 response_data = ems_schema:prop_list_to_json([{<<"endereco">>,Endereco},{<<"bairro">>,Bairro},{<<"logradouro">>,Logradouro}])}
			};
		Error -> 
			{error, Request#request{code = 400, 
									response_data = Error}
			}
	end.

