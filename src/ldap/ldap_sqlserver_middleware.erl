%%********************************************************************
%% @title Module ldap_sqlserver_middleware
%% @version 1.0.0
%% @doc Middleware to UnB Ldap autentication
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ldap_sqlserver_middleware).

-export([autentica/3, find_user_by_login/2]).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

autentica(UserLogin, UserPassword, Datasource) ->
	try
		PasswordUserCrypto = criptografia(UserPassword),
		?DEBUG("Ldap autentica ~p ~p.", [UserLogin, PasswordUserCrypto]),
		case ems_db:get_connection(Datasource) of
			{ok, Datasource2} -> 
				Params = [{{sql_varchar, 100}, [binary_to_list(UserLogin)]},
						  {{sql_varchar, 30}, [PasswordUserCrypto]}],
				Result = case ems_odbc_pool:param_query(Datasource2, 
														sql_autentica(), 
														Params, 
														?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> {error, enoent};
					{_, _, _Record} -> ok;
					Error -> Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error -> Error
		end
	catch
		_Exception:Reason3 -> {error, Reason3}
	end.

find_user_by_login(UserLogin, Datasource) ->
	try
		?DEBUG("Ldap find user by login ~p.", [UserLogin]),
		case ems_db:get_connection(Datasource) of
			{ok, Datasource2} -> 
				Params = [{{sql_varchar, 100}, [binary_to_list(UserLogin)]}],
				Result = case ems_odbc_pool:param_query(Datasource2, 
														sql_find_user_by_login(), 
														Params, 
														?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> {error, enoent};
					{_, _, [UserRecord]} -> {ok, UserRecord};
					Error -> Error
				end,
				ems_db:release_connection(Datasource2),
				Result;
			Error -> Error
		end
	catch
		_Exception:Reason3 -> {error, Reason3}
	end.

criptografia(Password) -> 
	Password2 = binary_to_list(Password),
	binary_to_list(base64:encode(sha1:binstring(Password2))).

sql_autentica() -> 
	"select top 1 1
	 from BDPessoa.dbo.TB_Pessoa p join BDAcesso.dbo.TB_Usuario u on p.PesCodigoPessoa = u.UsuPesIdPessoa 
	 where u.UsuLogin = ? and u.UsuSenha = ?".
	 
sql_find_user_by_login() ->	 
	"select top 1 p.PesCodigoPessoa, p.PesNome, p.PesCpf, p.PesEmail, u.UsuSenha
	from BDPessoa.dbo.TB_Pessoa p join BDAcesso.dbo.TB_Usuario u on p.PesCodigoPessoa = u.UsuPesIdPessoa 
	where u.UsuLogin = ?".
