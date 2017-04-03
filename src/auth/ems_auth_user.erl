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

authenticate(Service = #service{authorization = AuthorizationMode}, Request) ->
	case AuthorizationMode of
		<<"Basic">> -> do_basic_authorization(Service, Request);
		<<"Oauth2">> -> do_barer_authorization(Service, Request);
		_ -> {ok, undefined}
	end.

do_basic_authorization(_, #request{authorization = undefined}) -> {error, no_authorization};
do_basic_authorization(_, #request{authorization = <<>>}) -> {error, no_authorization};
do_basic_authorization(Service, Req = #request{authorization = Authorization}) ->
	case ems_http_util:parse_basic_authorization_header(Authorization) of
		{ok, Login, Password} ->
			case ems_user:find_by_login_and_password(list_to_binary(Login), list_to_binary(Password)) of
				{ok, User} -> 
					case has_grant_permission(Service, Req, User) of
						true -> {ok, User};
						false -> {error, no_authorization}
					end;
				_ -> 
					ems_logger:warn("ems_auth_user does not grant access to user ~p with HTTP Basic protocol. Reason: no_authorization", [Login]),
					{error, no_authorization}
			end;
		_Error -> 
			ems_logger:warn("ems_auth_user does not grant access to user ~p with HTTP Basic protocol. Reason: parse invalid authorization header."),
			{error, no_authorization}
	end.
	
do_barer_authorization(_, #request{authorization = undefined}) -> {error, no_authorization};
do_barer_authorization(_, #request{authorization = <<>>}) -> {error, no_authorization};
do_barer_authorization(_Service, #request{authorization = Authorization}) ->	
	CryptoText = ems_http_util:parse_barer_authorization_header(Authorization),
	PrivateKey = ems_util:open_file(?SSL_PATH ++  "/" ++ binary_to_list(<<"private_key.pem">>)),
	TextPlain = ems_util:decrypt_private_key(CryptoText,PrivateKey),
	?DEBUG("TextPlain ~p", [TextPlain]).
	

has_grant_permission(#service{check_grant_permission = false}, _, _) -> true;
has_grant_permission(#service{check_grant_permission = true},
					 #request{rowid = Rowid, type = Type}, 
					 #user{codigo = Codigo}) ->
	Hash = ems_user_permission:make_hash(Rowid, Codigo),
	case ems_user_permission:find_by_hash(Hash) of
		{ok, #user_permission{grant_get = GrantGet, 
							  grant_post = GrantPost, 
							  grant_put = GrantPut, 
							  grant_delete = GrantDelete}} ->
			case Type of
				"GET" -> GrantGet == true;
				"POST" -> GrantPost == true;
				"PUT" -> GrantPut == true;
				"DELETE" -> GrantDelete == true
			end;
		_ -> false
	end.

