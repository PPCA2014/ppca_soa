-module(ems_oauth2_recurso).

-export([execute/1]).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

execute(Request = #request{access_token = AccessToken}) -> 
	case oauth2:verify_access_token(AccessToken, []) of
		{ok,{_, [{<<"client">>, Client},
				 {<<"resource_owner">>, User},
				 {<<"expiry_time">>, _ExpireTime},
				 {<<"scope">>, Scope}] 
			}} -> 	
			ClientJson = ems_client:to_json(Client),
			ResourceOwner = ems_user:to_resource_owner(User, Client#client.id),
			ResponseData2 = iolist_to_binary([<<"{"/utf8>>,
												   <<"\"client\":"/utf8>>, ClientJson, <<","/utf8>>,
												   <<"\"resource_owner\":"/utf8>>, ResourceOwner, <<","/utf8>>,
												   <<"\"scope\":\""/utf8>>,Scope, <<"\""/utf8>>, 
											   <<"}"/utf8>>]),
			{ok, Request#request{code = 200, 
								 response_data = ResponseData2,
								 content_type = <<"application/json; charset=UTF-8">>}
			};		
		_ -> 
			{error, Request#request{code = 401,  
									reason = access_denied,
									response_data = ?ACCESS_DENIED_JSON}
			}
	end.
		
