-module(oauth2ems_authorize).

-export([execute/1]).

-include("../include/ems_schema.hrl").


execute(Request = #request{type = Type}) -> 
	TypeAuth = case Type of
		"GET" -> ems_request:get_querystring(<<"response_type">>, "", Request);
		"POST" -> ems_request:get_querystring(<<"grant_type">>, "", Request)
	end,
    Result = case TypeAuth of
            "password" -> 
				password_grant(Request);
            "client_credentials" ->
				client_credentials_grant(Request);
			"token" ->
				authorization_request(Request);
			"code" ->
				authorization_request(Request);	
			"authorization_code" ->
				access_token_request(Request);	
			"code2" ->
				% Apenas para simulação
				authorization_request2(Request);				
             _ -> {error, invalid_request}
	end,  
	case Result of
		{ok, ResponseData} ->
			ResponseData2 = ems_schema:prop_list_to_json(ResponseData),
			{ok, Request#request{code = 200, 
								 response_data = ResponseData2}
			};
		{redirect, ClientId, RedirectUri} ->
			%LocationPath = lists:concat(["http://127.0.0.1:2301/authorize?response_type=code2&client_id=", ClientId, "&redirect_uri=", RedirectUri]),
			LocationPath = lists:concat(["http://127.0.0.1:2301/portal/index.html?response_type=code2&client_id=", ClientId, "&redirect_uri=", RedirectUri]),
			{ok, Request#request{code = 302, 
									 response_data = <<"{}">>,
									 response_header = #{
															<<"location">> => LocationPath
														}
									}
			};
		Error ->
			ResponseData = ems_util:json_encode(Error),
			{ok, Request#request{code = 400, 
								 response_data = ResponseData}
			}
	end.

	
%%%===================================================================
%%% Funções internas
%%%===================================================================

client_credentials_grant(Request) ->
	ClientId = ems_request:get_querystring(<<"client_id">>, "", Request),
	Secret = ems_request:get_querystring(<<"secret">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
    Auth = oauth2:authorize_client_credentials(ClientId, Secret, Scope, []),
	issue_token(Auth).
    
password_grant(Request) -> 
	Username = ems_request:get_querystring(<<"username">>, "", Request),
	Password = ems_request:get_querystring(<<"password">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
    Auth = oauth2:authorize_password(Username, Password, Scope, []),
	issue_token(Auth).

authorization_request(Request) ->
    %State       = ems_request:get_querystring(<<"state">>, [],Request),
    %Scope       = ems_request:get_querystring(<<"scope">>, [],Request),
    ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    Resposta = case oauth2ems_backend:verify_redirection_uri(ClientId, RedirectUri, []) of
		ok -> {redirect, ClientId, RedirectUri};
		Error -> Error
	end,			
    Resposta.

authorization_request2(Request) ->
    ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    Username    = ems_request:get_querystring(<<"username">>, [],Request),
    Password    = ems_request:get_querystring(<<"password">>, [],Request),
    %State       = ems_request:get_querystring(<<"state">>, [],Request),
    Scope       = ems_request:get_querystring(<<"scope">>, [],Request),
    Resposta 	= case oauth2ems_backend:verify_redirection_uri(ClientId, RedirectUri, [])  of
        ok ->
            case oauth2:authorize_password(Username, Password, Scope, []) of
                {ok, Auth} -> issue_code({ok, Auth});
                Error -> Error
			end; 
        Error2 -> Error2
	end,			
    Resposta.

access_token_request(Request) ->
	Code = list_to_binary(ems_request:get_querystring(<<"code">>, [],Request)),
	ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    ClientSecret = ems_request:get_querystring(<<"secret">>, [],Request),
    Result = case oauth2:authorize_code_grant(ClientId, ClientSecret, Code, RedirectUri, []) of
        {ok, Auth} -> issue_token({ok, Auth});
		Error -> Error
	end,
	Result. 
		

issue_token({ok, Auth}) ->
	Response = oauth2:issue_token(Auth, []),
	{ok, oauth2_response:to_proplist(Response)};
issue_token(Error) ->
    Error.
    
issue_code({ok, Auth}) ->
	Response = oauth2:issue_code(Auth, []),
	{ok, oauth2_response:to_proplist(Response)};
issue_code(Error) ->
    Error.

