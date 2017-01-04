-module(oauth2ems_recurso).

-export([execute/1]).

-include("../include/ems_schema.hrl").


execute(Request) -> 
	Token = maps:get(<<"token">>, Request#request.querystring_map, []),
	oauth2:verify_access_token(Token, []).
