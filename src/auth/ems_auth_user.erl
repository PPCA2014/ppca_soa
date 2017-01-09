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

autentica(Service, Request) ->
	try
		case Service#service.authentication of
			<<"Basic">> -> do_basic_authorization(Request);
			<<>> -> {ok, anonimo}
		end
	catch
		_Exception:_Reason ->  {error, no_authorization} 
	end.

do_basic_authorization(Request) ->
	case Request#request.authorization /= "" of
		true -> 
			[Authorization|[LoginAndPassword|_]] = string:tokens(Request#request.authorization, " "),
			case Authorization =:= "Basic" of
				true -> 
					LoginAndPassword2 = base64:decode_to_string(LoginAndPassword),
					[Login|[Password|_]] = string:tokens(LoginAndPassword2, ":"),
					case ems_user:find_by_login_and_password(list_to_binary(Login), list_to_binary(Password)) of
						{ok, User} -> {ok, User};
						_ -> {error, no_authorization}
					end;
				false -> {error, no_authorization}
			end;
		false -> {error, no_authorization}
	end.
 	


