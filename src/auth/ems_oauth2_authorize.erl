-module(ems_oauth2_authorize).

-export([execute/1]).
-export([code_request/1]).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").



execute(Request = #request{type = Type, protocol_bin = Protocol, port = Port, host = Host}) -> 
	try
		case Type of
			<<"GET">> -> GrantType = ems_util:get_querystring(<<"response_type">>, <<>>, Request);
			<<"POST">> -> GrantType = ems_util:get_querystring(<<"grant_type">>, <<>>, Request);
			_ -> GrantType = undefined
		end,
		Result = case GrantType of
				<<"password">> -> 
					ems_db:inc_counter(ems_oauth2_grant_type_password),
					password_grant(Request);
				<<"client_credentials">> ->
					ems_db:inc_counter(ems_oauth2_grant_type_client_credentials),
					client_credentials_grant(Request);
				<<"token">> -> 
					ems_db:inc_counter(ems_oauth2_grant_type_token),
					authorization_request(Request);
				<<"code">> ->	
					ems_db:inc_counter(ems_oauth2_grant_type_code),
					authorization_request(Request);	
				<<"authorization_code">> ->	
					ems_db:inc_counter(ems_oauth2_grant_type_authorization_code),
					access_token_request(Request);
				<<"refresh_token">> ->	
					ems_db:inc_counter(ems_oauth2_grant_type_refresh_token),
					refresh_token_request(Request);	
				 _ -> {error, access_denied}
		end,  
		case Result of
			{ok, [{<<"access_token">>,AccessToken},
				   {<<"expires_in">>, ExpireIn},
				   {<<"resource_owner">>, User},
				   {<<"scope">>, Scope},
				   {<<"refresh_token">>, RefreshToken},
				   {<<"refresh_token_expires_in">>, RefreshTokenExpireIn},
				   {<<"token_type">>, TokenType}
				  ]
			 } ->
					ems_db:inc_counter(binary_to_atom(iolist_to_binary([<<"ems_oauth2_singlesignon_user_">>, integer_to_binary(User#user.id)]), utf8)),
					ResourceOwner = ems_user:to_resource_owner(User),
					ResponseData2 = iolist_to_binary([<<"{"/utf8>>,
														   <<"\"access_token\":"/utf8>>, <<"\""/utf8>>, AccessToken, <<"\","/utf8>>,
														   <<"\"expires_in\":"/utf8>>, integer_to_binary(ExpireIn), <<","/utf8>>,
														   <<"\"resource_owner\":"/utf8>>, ResourceOwner, <<","/utf8>>,
														   <<"\"scope\":"/utf8>>, <<"\""/utf8>>, Scope, <<"\","/utf8>>,
														   <<"\"refresh_token\":"/utf8>>, <<"\""/utf8>>, case RefreshToken of
																															undefined -> <<>>;
																															_ -> RefreshToken
																													  end, <<"\","/utf8>>, 
														   <<"\"refresh_token_in\":"/utf8>>, case RefreshTokenExpireIn of 
																									undefined -> <<"0">>; 
																									_ -> integer_to_binary(RefreshTokenExpireIn) 
																							 end, <<","/utf8>>,
														   <<"\"token_type\":"/utf8>>, <<"\""/utf8>>, TokenType, <<"\""/utf8>>,
													   <<"}"/utf8>>]),
					{ok, Request#request{code = 200, 
										 response_data = ResponseData2,
										 oauth2_grant_type = GrantType,
										 oauth2_access_token = AccessToken,
										 oauth2_refresh_token = RefreshToken,
										 content_type = <<"application/json; charset=UTF-8">>}
					};		
			{redirect, ClientId, RedirectUri} ->
					ClientBin = integer_to_binary(ClientId),
					ems_db:inc_counter(binary_to_atom(iolist_to_binary([<<"ems_oauth2_singlesignon_client_">>, ClientBin]), utf8)),
					LocationPath = iolist_to_binary([Protocol, <<"://"/utf8>>, Host, <<":"/utf8>>, integer_to_binary(Port), 
													 <<"/login/index.html?response_type=code&client_id=">>, ClientBin, 
													 <<"&redirect_uri=">>, RedirectUri]),
					{ok, Request#request{code = 302, 
										 oauth2_grant_type = GrantType,
										 response_header = #{
																<<"location">> => LocationPath
															}
										}
					};
			_ ->
					{error, Request#request{code = 401, 
											reason = access_denied,
											oauth2_grant_type = GrantType,
											response_data = ?ACCESS_DENIED_JSON}
					}
		end
	catch
		_:_ ->
					{error, Request#request{code = 401, 
											reason = access_denied,
											response_data = ?ACCESS_DENIED_JSON}
					}
	end.

%% Requisita o código de autorização - seções 4.1.1 e 4.1.2 do RFC 6749.
%% URL de teste: GET http://127.0.0.1:2301/authorize?response_type=code2&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html&username=johndoe&password=A3ddj3w
code_request(Request = #request{authorization = Authorization}) ->
    try
		ClientId = parse_client_id(ems_util:get_querystring(<<"client_id">>, <<>>, Request)),
		case ClientId > 0 of
			true ->
				case ems_util:parse_basic_authorization_header(Authorization) of
					{ok, Username, Password} ->
						RedirectUri = ems_util:get_querystring(<<"redirect_uri">>, <<>>, Request),
						State = ems_util:get_querystring(<<"state">>, <<>>, Request),
						Scope = ems_util:get_querystring(<<"scope">>, <<>>, Request),
						Authz = oauth2:authorize_code_request({Username, list_to_binary(Password)}, ClientId, RedirectUri, Scope, []),
						case issue_code(Authz) of
							{ok, Response} ->
								Code = element(2, lists:nth(1, Response)),
								LocationPath = <<RedirectUri/binary,"?code=", Code/binary,"&state=", State/binary>>,
								{ok, Request#request{code = 200, 
													 response_data = <<"{}">>,
													 response_header = #{<<"location">> => LocationPath}}
								};
							_ ->
								{error, Request#request{code = 401, 
														reason = access_denied,
														response_data = ?ACCESS_DENIED_JSON}
								}
						end;
					_ ->
						{error, Request#request{code = 401, 
												reason = access_denied,
												response_data = ?ACCESS_DENIED_JSON}
						}
				end;
			false ->
				{error, Request#request{code = 401, 
										reason = access_denied,
										response_data = ?ACCESS_DENIED_JSON}
				}
		end
	catch
		_:_ ->
			{error, Request#request{code = 401, 
									reason = access_denied,
									response_data = ?ACCESS_DENIED_JSON}
			}
	end.

	
%%%===================================================================
%%% Funções internas
%%%===================================================================


%% Cliente Credencial Grant- seção 4.4.1 do RFC 6749. 
%% URL de teste: POST http://127.0.0.1:2301/authorize?grant_type=client_credentials&client_id=s6BhdRkqt3&secret=qwer
client_credentials_grant(Request = #request{authorization = Authorization}) ->
	try
		ClientId = parse_client_id(ems_util:get_querystring(<<"client_id">>, <<>>, Request)),
		Scope = ems_util:get_querystring(<<"scope">>, <<>>, Request),	
		case ClientId > 0 of
			true -> 
				Secret = ems_util:get_querystring(<<"client_secret">>, <<>>, Request),
				Auth = oauth2:authorize_client_credentials({ClientId, Secret}, Scope, []),
				issue_token(Auth);
			false ->
				% O ClientId também pode ser passado via header Authorization
				case Authorization =/= undefined of
					true ->
						case ems_util:parse_basic_authorization_header(Authorization) of
							{ok, Login, Password} ->
								ClientId2 = list_to_integer(Login),
								Secret = list_to_binary(Password),
								Auth = oauth2:authorize_client_credentials({ClientId2, Secret}, Scope, []),
								issue_token(Auth);
							_ -> {error, access_denied}
						end;
					false -> {error, access_denied}
				end
		end
	catch
		_:_ -> {error, access_denied}
	end.


%% Resource Owner Password Credentials Grant - seção 4.3.1 do RFC 6749.
%% URL de teste: POST http://127.0.0.1:2301/authorize?grant_type=password&username=johndoe&password=A3ddj3w
password_grant(Request) -> 
	try
		Username = ems_util:get_querystring(<<"username">>, <<>>, Request),
		case Username =/= <<>> of
			true ->
				Password = ems_util:get_querystring(<<"password">>, <<>>, Request),
				Scope = ems_util:get_querystring(<<"scope">>, <<>>, Request),	
				Authorization = oauth2:authorize_password({Username, Password}, Scope, []),
				issue_token(Authorization);
			false -> {error, access_denied}
		end
	catch
		_:_ -> {error, access_denied}
	end.

	
%% Verifica a URI do Cliente e redireciona para a página de autorização - Implicit Grant e Authorization Code Grant
%% URL de teste: GET http://127.0.0.1:2301/authorize?response_type=code&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html
authorization_request(Request) ->
    try
		ClientId = parse_client_id(ems_util:get_querystring(<<"client_id">>, <<>>, Request)),
		case ClientId > 0 of
			true ->
				%State = ems_util:get_querystring(<<"state">>, <<>>, Request),
				RedirectUri = ems_util:get_querystring(<<"redirect_uri">>, <<>>, Request),
				case ems_oauth2_backend:verify_redirection_uri(ClientId, RedirectUri, []) of
					{ok, _} -> {redirect, ClientId, RedirectUri};
					_ -> {error, access_denied}
				end;
			false -> {error, access_denied}
		end
	catch
		_:_ -> {error, access_denied}
	end.


%% Requisita o código de autorização - seções 4.1.1 e 4.1.2 do RFC 6749.
%% URL de teste: GET http://127.0.0.1:2301/authorize?response_type=code2&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html&username=johndoe&password=A3ddj3w
refresh_token_request(Request) ->
    try
		ClientId = parse_client_id(ems_util:get_querystring(<<"client_id">>, <<>>, Request)),
		case ClientId > 0 of
			true ->
				ClientSecret = ems_util:get_querystring(<<"client_secret">>, <<>>, Request),
				Reflesh_token = ems_util:get_querystring(<<"refresh_token">>, <<>>, Request),
				Scope = ems_util:get_querystring(<<"scope">>, <<>>, Request),
				Authorization = ems_oauth2_backend:authorize_refresh_token({ClientId, ClientSecret}, Reflesh_token, Scope),
				issue_token(Authorization);
			false -> {error, access_denied}
		end
	catch
		_:_ -> {error, access_denied}
	end.
		

%% Requisita o token de acesso com o código de autorização - seções  4.1.3. e  4.1.4 do RFC 6749.
%% URL de teste: POST http://127.0.0.1:2301/authorize?grant_type=authorization_code&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html&username=johndoe&password=A3ddj3w&secret=qwer&code=dxUlCWj2JYxnGp59nthGfXFFtn3hJTqx
access_token_request(Request = #request{authorization = Authorization}) ->
	try
		ClientId = parse_client_id(ems_util:get_querystring(<<"client_id">>, <<>>, Request)),
		Code = ems_util:get_querystring(<<"code">>, <<>>, Request),
		RedirectUri = ems_util:get_querystring(<<"redirect_uri">>, <<>>, Request),
		case ClientId > 0 of
			true -> 
				ClientSecret = ems_util:get_querystring(<<"client_secret">>, <<>>, Request),
				Authz = oauth2:authorize_code_grant({ClientId, ClientSecret}, Code, RedirectUri, []),
				issue_token_and_refresh(Authz);
			false ->
				% O ClientId também pode ser passado via header Authorization
				case Authorization =/= undefined of
					true ->
						case ems_util:parse_basic_authorization_header(Authorization) of
							{ok, Login, Password} ->
								ClientId2 = list_to_binary(Login),
								ClientSecret = list_to_binary(Password),
								Auth = oauth2:authorize_code_grant({ClientId2, ClientSecret}, Code, RedirectUri, []),
								issue_token_and_refresh(Auth);						
							_ -> {error, access_denied}
						end;
					false -> {error, access_denied}
				end
		end
	catch
		_:_ -> {error, access_denied}
	end.
	

issue_token({ok, {_, Auth}}) ->
	{ok, {_, {response, AccessToken, 
						undefined,
						ExpiresIn,
						User,
						Scope, 
						RefreshToken, 
						RefreshTokenExpiresIn,
						TokenType
             }
		}} = oauth2:issue_token(Auth, []),
	{ok, [{<<"access_token">>, AccessToken},
            {<<"expires_in">>, ExpiresIn},
            {<<"resource_owner">>, User},
            {<<"scope">>, Scope},
            {<<"refresh_token">>, RefreshToken},
            {<<"refresh_token_expires_in">>, RefreshTokenExpiresIn},
            {<<"token_type">>, TokenType}]};
issue_token(_) -> {error, access_denied}.
    

issue_token_and_refresh({ok, {_, Auth}}) ->
	{ok, {_, {response, AccessToken, 
						undefined,
						ExpiresIn,
						User,
						Scope, 
						RefreshToken, 
						RefreshTokenExpiresIn,
						TokenType
             }
		}} = oauth2:issue_token_and_refresh(Auth, []),
	{ok, [{<<"access_token">>, AccessToken},
            {<<"expires_in">>, ExpiresIn},
            {<<"resource_owner">>, User},
            {<<"scope">>, Scope},
            {<<"refresh_token">>, RefreshToken},
            {<<"refresh_token_expires_in">>, RefreshTokenExpiresIn},
            {<<"token_type">>, TokenType}]};
issue_token_and_refresh(_) -> {error, access_denied}.

issue_code({ok, {_, Auth}}) ->
	{ok, {_, Response}} = oauth2:issue_code(Auth, []),
	{ok, oauth2_response:to_proplist(Response)};
issue_code(_) -> {error, access_denied}.


-spec parse_client_id(binary()) -> non_neg_integer().
parse_client_id(<<>>) -> 0;
parse_client_id(Value) -> 
	try
		binary_to_integer(Value)
	catch
		_:_ -> 0
	end.
