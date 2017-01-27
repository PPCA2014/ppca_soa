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
    
-export([autentica/2]).

autentica(#service{authorization = AuthorizationMode}, Request) ->
	try
		case AuthorizationMode of
			<<"Basic">> -> do_basic_authorization(Request);
			<<>> -> {ok, public}
		end
	catch
		_Exception:_Reason ->  {error, no_authorization} 
	end.

do_basic_authorization(#request{authorization = Authorization}) ->
	case Authorization /= <<>> of
		true -> 
			case ems_http_util:parse_basic_authorization_header(Authorization) of
				{ok, Login, Password} ->
					case ems_user:find_by_login_and_password(list_to_binary(Login), list_to_binary(Password)) of
						{ok, User} -> 
							?DEBUG("Authenticating ~p with HTTP Basic protocol: ok", [{Login, Password}]),
							{ok, User};
						_ -> 
							?DEBUG("Authenticating ~p with HTTP Basic protocol: no_authorization", [{Login, Password}]),
							{error, no_authorization}
					end;
				_Error -> 
					?DEBUG("Invalid authorization header to HTTP Basic protocol: ~p.", [Authorization]),
					{error, no_authorization}
			end;
		false -> {error, no_authorization}
	end.
 	


