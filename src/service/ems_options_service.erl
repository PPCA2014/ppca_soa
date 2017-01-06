%%********************************************************************
%% @title Módulo ems_options_service
%% @version 1.0.0
%% @doc Serviço que responde o verbo OPTIONS
%% @end	    
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_options_service).


-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

-export([execute/1]).

execute(Request) ->
	{ok, Request#request{code = 200,
						 response_data = <<>>,
						 response_header = #{<<"cache-control">> => <<"max-age=290304000, public">>}}
	}.



