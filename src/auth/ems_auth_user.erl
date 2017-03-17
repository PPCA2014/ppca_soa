%%********************************************************************
%% @title Module ems_auth_user
%% @version 1.0.0
%% @doc Module responsible for authenticating users.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_auth_user).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
    
-export([authenticate/2]).

authenticate(#service{authorization = AuthorizationMode}, Request) ->
	case AuthorizationMode of
		<<"Basic">> -> do_basic_authorization(Request);
		<<"Oauth2">> -> do_barer_authorization(Request);
		_ -> {ok, undefined}
	end.

do_basic_authorization(#request{authorization = Authorization}) ->
	case Authorization /= <<>> of
		true -> 
			case ems_http_util:parse_basic_authorization_header(Authorization) of
				{ok, Login, Password} ->
					case ems_user:find_by_login_and_password(list_to_binary(Login), list_to_binary(Password)) of
						{ok, User} -> 
							ems_logger:info("ems_auth_user authenticating ~p with HTTP Basic protocol: ok", [{Login, Password}]),
							{ok, User};
						_ -> 
							ems_logger:warn("ems_auth_user authenticating ~p with HTTP Basic protocol: no_authorization", [{Login, Password}]),
							{error, no_authorization}
					end;
				_Error -> 
					ems_logger:warn("ems_auth_user parse invalid authorization header to HTTP Basic protocol: ~p.", [Authorization]),
					{error, no_authorization}
			end;
		false -> 
			ems_logger:warn("ems_auth_user no HTTP Basic authorization user."),
			{error, no_authorization}
	end.
	
do_barer_authorization(#request{authorization = Authorization}) ->	
	CryptoText = ems_http_util:parse_barer_authorization_header(Authorization),
	PrivateKey = ems_util:open_file(?SSL_PATH ++  "/" ++ binary_to_list(<<"private_key.pem">>)),
	TextPlain = ems_util:decrypt_private_key(CryptoText,PrivateKey),
	?DEBUG("TextPlain ~p", [TextPlain]).
	

 	


