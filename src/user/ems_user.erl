%%********************************************************************
%% @title Module ems_user
%% @version 1.0.0
%% @doc user class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([insert/1, update/1, all/0, delete/1, 
		 authenticate_login_password/2, 
		 find_by_id/1,		 
		 find_by_codigo/1,
		 find_by_login/1, 
		 find_by_name/1, 
		 find_by_email/1, 
		 find_by_cpf/1, 
		 find_by_login_and_password/2,
		 to_resource_owner/1]).

find_by_id(Id) -> ems_db:get(user, Id).

insert(User) -> 
	case valida(User, insert) of
		ok -> ems_db:insert(User);
		Error -> 
			Error
	end.

update(User) -> 
	case valida(User, update) of
		ok -> ems_db:update(User);
		Error -> Error
	end.

all() -> ems_db:all(user).

delete(Id) -> ems_db:delete(user, Id).

valida(_User, _Operation) -> ok.

-spec authenticate_login_password(binary(), binary()) -> ok | {error, invalidCredentials}.
authenticate_login_password(Login, Password) ->
	case find_by_login(Login) of
		{ok, #user{password = PasswordUser}} -> 
			case PasswordUser =:= ems_util:criptografia_sha1(Password) orelse PasswordUser =:= Password of
				true -> ok;
				_ -> {error, invalidCredentials}
			end;
		_ -> {error, invalidCredentials}
	end.


-spec find_by_codigo(integer()) -> #user{} | {error, enoent}.
find_by_codigo(Codigo) ->
	case mnesia:dirty_index_read(user, Codigo, #user.codigo) of
		[] -> {error, enoent};
		[Record|_] -> {ok, Record}
	end.


-spec find_by_login(binary()) -> #user{} | {error, enoent}.
find_by_login(<<>>) -> {error, enoent};	
find_by_login("") -> {error, enoent};	
find_by_login(undefined) -> {error, enoent};	
find_by_login(Login) ->
	case is_list(Login) of
		true -> LoginStr = string:to_lower(Login);
		false -> LoginStr = string:to_lower(binary_to_list(Login))
	end,
	LoginBin = list_to_binary(LoginStr),
	case mnesia:dirty_index_read(user, LoginBin, #user.login) of
		[] -> 
			case find_by_email(LoginBin) of
				{ok, Record} -> {ok, Record};
				{error, enoent} -> find_by_cpf(Login)
			end;
		[Record|_] -> {ok, Record}
	end.


-spec find_by_email(binary()) -> #user{} | {error, enoent}.
find_by_email(<<>>) -> {error, enoent};	
find_by_email("") -> {error, enoent};	
find_by_email(undefined) -> {error, enoent};	
find_by_email(Email) -> 
	case is_list(Email) of
		true -> EmailStr = string:to_lower(Email);
		false -> EmailStr = string:to_lower(binary_to_list(Email))
	end,
	case string:rchr(EmailStr, $@) > 0 of
		true -> 
			EmailBin = list_to_binary(EmailStr),
			case mnesia:dirty_index_read(user, EmailBin, #user.email) of
				[] -> {error, enoent};
				[Record|_] -> {ok, Record}
			end;
		false -> {error, enoent}
	end.


-spec find_by_cpf(binary()) -> #user{} | {error, enoent}.
find_by_cpf(<<>>) -> {error, enoent};	
find_by_cpf("") -> {error, enoent};	
find_by_cpf(undefined) -> {error, enoent};	
find_by_cpf(Cpf) ->
	case is_list(Cpf) of
		true -> 
			CpfStr = Cpf,
			CpfBin = list_to_binary(Cpf);
		false ->
			CpfStr = binary_to_list(Cpf),
			CpfBin = Cpf
	end,
	LenCpf = string:len(CpfStr),
	case (LenCpf =:= 11 andalso ems_util:is_cpf_valid(CpfStr)) orelse
		 (LenCpf =:= 14 andalso ems_util:is_cnpj_valid(CpfStr)) of
		true ->
			case mnesia:dirty_index_read(user, CpfBin, #user.cpf) of
				[] -> {error, enoent};
				[Record|_] -> {ok, Record}
			end;
		false -> 
			% tenta inserir zeros na frente e refaz a pesquisa
			case LenCpf of
				10 -> Cpf2 = "0" ++ CpfStr;  
				 9 -> Cpf2 = "00" ++ CpfStr;
				 _ -> Cpf2 = CpfStr
			end,
			LenCpf2 = string:len(Cpf2),
			case (LenCpf2 =:= 11 orelse LenCpf2 =:= 14) of  % deve ser CPF ou CNPJ
				true ->
					case mnesia:dirty_index_read(user, list_to_binary(Cpf2), #user.cpf) of
								[] -> {error, enoent};
								[Record|_] -> {ok, Record}
					end;
				false -> {error, enoent}
			end
	end.


find_by_name(Name) -> ems_db:find_first(user, [{"name", "==", Name}]).
	
find_by_login_and_password(Login, Password) ->
	case find_by_login(Login) of
		{ok, User = #user{password = PasswordUser}} -> 
			case PasswordUser =:= ems_util:criptografia_sha1(Password) orelse PasswordUser =:= Password of
				true -> {ok, User};
				false -> {error, enoent}
			end;
		_ -> {error, enoent}
	end.


-spec to_resource_owner(#user{} | undefined) -> binary().
to_resource_owner(User) ->
	case User of
		undefined -> <<"{}"/utf8>>;
		_ -> ems_schema:to_json({<<"id">>, User#user.id,
								 <<"codigo">>, User#user.codigo,
								 <<"login">>, User#user.login, 
								 <<"name">>, User#user.name,
								 <<"matricula">>, User#user.matricula,
								 <<"email">>, User#user.email,
								 <<"type">>, User#user.type,
								 <<"lotacao">>, User#user.lotacao})
	end.

