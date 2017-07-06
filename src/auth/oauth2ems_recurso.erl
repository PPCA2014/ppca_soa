-module(oauth2ems_recurso).

-export([execute/1]).

-include("../include/ems_schema.hrl").

% teste de token de acesso
execute(Request) -> 

	AccessToken = Request#request.access_token,
	
	% valida o token recebido
	Result = oauth2:verify_access_token(AccessToken, []),
	case Result of
		{ok,{_,Auth}} -> 	{ok, Request#request{code = 200,  response_data = ems_schema:prop_list_to_json(Auth)} };
		Error -> {ok, Request#request{code = 401,  response_data = ems_schema:to_json(Error)}}
	end.
		
