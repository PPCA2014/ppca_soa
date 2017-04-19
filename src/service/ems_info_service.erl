%%********************************************************************
%% @title Module info
%% @version 1.0.0
%% @doc It provides information about the bus in runtime.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_info_service).

-include("include/ems_schema.hrl").

-export([info/1]).
  
info(Request) -> 
	{ok, Request#request{code = 200, 
						 reason = ok,
						 response_data = <<"{\"message\": \"It works!!!\"}">>}
	}.

