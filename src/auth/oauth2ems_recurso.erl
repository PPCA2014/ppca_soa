-module(oauth2ems_recurso).

-export([execute/1]).

-include("../include/ems_schema.hrl").

% teste de token de acesso
execute(Request) -> 

	AccessToken = Request#request.access_token,
	
	% valida o token recebido
	Result = oauth2:verify_access_token(AccessToken, []),
	case Result of
		{ok,{_, [{<<"client">>, #client{ id = Id, codigo = Codigo, description = Description }},
				 {<<"resource_owner">>, ResourceOwner},
				 {<<"expiry_time">>,ExpireTime},
				 {<<"scope">>, Scope}] 
			}} -> 	
		
			ResponseData2 = iolist_to_binary([<<"{"/utf8>>,
															   <<"\"client\""/utf8>>, <<":"/utf8>>, 
																	<<"{"/utf8>>,
																		<<"\"id\""/utf8>>, <<":"/utf8>>, integer_to_binary(Id), <<","/utf8>>,
																		<<"\"codigo\""/utf8>>, <<":"/utf8>>, integer_to_binary(Codigo), <<","/utf8>>,
																		<<"\"description\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Description, <<"\""/utf8>>, 
																	<<"}"/utf8>>,
															   <<","/utf8>>,
															   <<"\"expiry_time\""/utf8>>, <<":"/utf8>>, integer_to_binary(ExpireTime), <<","/utf8>>,
															   <<"\"resource_owner\""/utf8>>, <<":"/utf8>>, ResourceOwner, <<","/utf8>>,
															   <<"\"scope\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Scope, <<"\""/utf8>>, 
														   <<"}"/utf8>>]),
			{ok, Request#request{code = 200, 
								 response_data = ResponseData2,
								 content_type = <<"application/json;charset=UTF-8">>}
			};		
		Error -> {ok, Request#request{code = 401,  response_data = ems_schema:to_json(Error)}}
	end.
		
