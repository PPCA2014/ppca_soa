-module(oauth2ems_recurso).

-export([execute/1]).

-include("../include/ems_schema.hrl").

execute(Request = #request{access_token = AccessToken}) -> 
	case oauth2:verify_access_token(AccessToken, []) of
		{ok,{_, [{<<"client">>, Client},
				 {<<"resource_owner">>, User},
				 {<<"expiry_time">>, ExpireTime},
				 {<<"scope">>, Scope}] 
			}} -> 	
			
			ClientJson = ems_client:to_json(Client),
			ResourceOwner = ems_user:to_resource_owner(User),
			ResponseData2 = iolist_to_binary([<<"{"/utf8>>,
												   <<"\"client\""/utf8>>, <<":"/utf8>>, ClientJson, <<","/utf8>>,
												   <<"\"expiry_time\""/utf8>>, <<":"/utf8>>, integer_to_binary(ExpireTime), <<","/utf8>>,
												   <<"\"resource_owner\""/utf8>>, <<":"/utf8>>, ResourceOwner, <<","/utf8>>,
												   <<"\"scope\""/utf8>>, <<":"/utf8>>, <<"\""/utf8>>, Scope, <<"\""/utf8>>, 
											   <<"}"/utf8>>]),
			{ok, Request#request{code = 200, 
								 response_data = ResponseData2,
								 content_type = <<"application/json;charset=UTF-8">>}
			};		
		{error, Reason} = Error -> 
			{error, Request#request{code = 401,  
									reason = Reason,
									response_data = ems_schema:to_json(Error)}
			}
	end.
		
