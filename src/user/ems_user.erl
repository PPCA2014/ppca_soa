%%********************************************************************
%% @title Module ems_user
%% @version 1.0.0
%% @doc Manages information about users
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([get/1, insert/1, update/1, all/0, delete/1, 
		 authenticate_login_password/2, 
		 find_by_login/1, 
		 find_by_name/1, 
		 find_by_email/1, 
		 find_by_cpf/1, 
		 find_by_login_and_password/2]).

get(Id) -> ems_db:get(user, Id).

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

authenticate_login_password(Login, Password) ->
	case find_by_login(Login) of
		{ok, #user{password = PasswordUser}} -> 
			case PasswordUser =:= ems_util:criptografia_sha1(Password) of
				true -> ok;
				_ -> {error, invalidCredentials}
			end;
		_ -> {error, invalidCredentials}
	end.


-spec find_by_login(binary()) -> #user{} | {error, enoent}.
find_by_login(<<>>) -> {error, enoent};	
find_by_login("") -> {error, enoent};	
find_by_login(Login) when is_list(Login) ->	find_by_login(list_to_binary(Login));
find_by_login(Login) ->
	case mnesia:dirty_index_read(user, Login, #user.login) of
		[] -> 
			case find_by_email(Login) of
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
		true -> 
			EmailStr = string:to_lower(Email),
			EmailBin = list_to_binary(EmailStr);
		false ->
			EmailStr = string:to_lower(binary_to_list(Email)),
			EmailBin = list_to_binary(EmailStr)
	end,
	case string:rchr(EmailStr, $@) > 0 of
		true -> 
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
			case PasswordUser =:= ems_util:criptografia_sha1(Password) of
				true -> {ok, User};
				false -> {error, enoent}
			end;
		_ -> {error, enoent}
	end.
