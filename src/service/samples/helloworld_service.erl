%%********************************************************************
%% @title Módulo helloworld_service
%% @version 1.0.0
%% @doc Módulo de serviço para o famoso hello world!!!
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************
-module(helloworld_service).

-include("../include/ems_schema.hrl").

-export([execute/1]).
 
execute(Request) -> 
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"message\": \"Hello World!!!\"}">>}
	}.
	
