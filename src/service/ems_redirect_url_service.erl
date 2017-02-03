%%********************************************************************
%% @title Module ems_redirect_url_service
%% @version 1.0.0
%% @doc It redirect urls :)
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_redirect_url_service).

-include("../include/ems_schema.hrl").
-include("../include/ems_config.hrl").

-export([execute/1]).
  
execute(Request = #request{service = #service{redirect_url = RedirectUrl}}) -> 
	?DEBUG("Redirect to ~p.", [RedirectUrl]),
	{ok, Request#request{code = 302, 
							 response_data = <<>>,
							 response_header = #{
													<<"location">> => RedirectUrl
												}
							}
	}.

