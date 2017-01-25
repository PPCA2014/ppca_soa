%%********************************************************************
%% @title Module ems_web_service_correios
%% @version 1.0.0
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
	UrlBuscaCep =  lists:concat([UrlCorreio,"/",Cep]),  
	case httpc:request(get, {UrlBuscaCep, []}, [], []) of
		{ok, {_, _, Result}} ->
			{ok, Request#request{code = 200, 
								response_data = list_to_binary(Result)}
			};
		Error -> 
			{error, Request#request{code = 400, 
							response_data = Error}
			}
	end.
