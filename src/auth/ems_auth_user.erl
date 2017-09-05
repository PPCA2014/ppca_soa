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

authenticate(Service = #service{authorization = AuthorizationMode}, 
			 Request = #request{type = Method}) ->
	case Method of
		"OPTIONS" -> {ok, public, public, <<>>, <<>>};
		"HEAD" -> {ok, public, public, <<>>, <<>>};
		_ -> 
			case AuthorizationMode of
				basic -> do_basic_authorization(Service, Request);
				oauth2 -> do_bearer_authorization(Service, Request);
				_ -> {ok, public, public, <<>>, <<>>}
			end
	end.



%%====================================================================
%% Internal functions
%%====================================================================


do_basic_authorization(_, #request{authorization = undefined}) -> {error, access_denied};
do_basic_authorization(_, #request{authorization = <<>>}) -> {error, access_denied};
do_basic_authorization(Service, Request = #request{authorization = Authorization}) ->
	case ems_http_util:parse_basic_authorization_header(Authorization) of
		{ok, Login, Password} ->
			case ems_user:find_by_login_and_password(list_to_binary(Login), list_to_binary(Password)) of
				{ok, User} -> do_check_grant_permission(Service, Request, public, User, <<>>, <<>>);
				Error -> Error
			end;
		_ -> do_bearer_authorization(Service, Request) 			% Quando ocorrer erro, tenta fazer via oauth2
	end.

	
do_bearer_authorization(_, #request{authorization = <<>>}) -> {error, access_denied};
do_bearer_authorization(Service, Request = #request{authorization = undefined}) ->
	AccessToken = ems_request:get_querystring(<<"token">>, <<"access_token">>, <<>>, Request), % a querystring pode ser token ou access_token
	do_oauth2_check_access_token(AccessToken, Service, Request);
do_bearer_authorization(Service, Request = #request{authorization = Authorization}) ->	
	case ems_http_util:parse_bearer_authorization_header(Authorization) of
		{ok, AccessToken} -> do_oauth2_check_access_token(AccessToken, Service, Request);
		Error -> Error
	end.

do_oauth2_check_access_token(<<>>, _, _) -> 
	{error, access_denied};
do_oauth2_check_access_token(AccessToken, Service, Req) ->
	case oauth2:verify_access_token(AccessToken, undefined) of
		{ok, {[], [{<<"client">>, Client}, 
				   {<<"resource_owner">>, User}, 
				   {<<"expiry_time">>, _ExpityTime}, 
				   {<<"scope">>, Scope}]}} -> 
			do_check_grant_permission(Service, Req, Client, User, AccessToken, Scope);
		Error -> Error
	end.
	

-spec do_check_grant_permission(#service{}, #request{}, #client{} | public, #user{}, binary(), binary()) -> {ok, #client{}, #user{}, binary(), binary()} | {error, access_denied}.
do_check_grant_permission(Service, Req, Client, User, AccessToken, Scope) ->
	case ems_user_permission:has_grant_permission(Service, Req, User) of
		true -> {ok, Client, User, AccessToken, Scope};
		false -> {error, access_denied}
	end.



