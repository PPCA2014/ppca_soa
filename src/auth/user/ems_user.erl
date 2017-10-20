%%********************************************************************
%% @title Module ems_user
%% @version 1.0.0
%% @doc user class
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").
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
		 to_resource_owner/1,
		 add_user/2,
 		 new_from_map/2,
		 new_from_map/3,
		 get_table/1,
		 find/2,
		 all/1]).


-spec find_by_id(non_neg_integer()) -> {ok, #user{}} | {error, enoent}.
find_by_id(Id) -> 
	case ems_db:get(user_db, Id) of
		{ok, Record} -> {ok, setelement(1, Record, user)};
		_ -> case ems_db:get(user_fs, Id) of
				{ok, Record} -> {ok, setelement(1, Record, user)};
				_ -> {error, enoent}
			 end
	end.

-spec all() -> {ok, list()}.
all() -> 
	{ok, ListaUserDb} = ems_db:all(user_db),
	{ok, ListaUserFs} = ems_db:all(user_fs),
	{ok, ListaUserDb ++ ListaUserFs}.
	

-spec authenticate_login_password(binary(), binary()) -> ok | {error, invalidCredentials}.
authenticate_login_password(Login, Password) ->
	case find_by_login(Login) of
		{ok, #user{password = PasswordUser}} -> 
			case PasswordUser =:= ems_util:criptografia_sha1(Password) orelse PasswordUser =:= Password of
				true -> io:format("ok\n"), ok;
				_ -> {error, invalidCredentials}
			end;
		_ -> {error, invalidCredentials}
	end.


-spec find_by_codigo(non_neg_integer()) -> {ok, #user{}} | {error, enoent}.
find_by_codigo(Codigo) when is_binary(Codigo) ->
	find_by_codigo(ems_util:binary_to_integer(Codigo));
find_by_codigo(Codigo) when is_list(Codigo) ->
	find_by_codigo(list_to_integer(Codigo));
find_by_codigo(Codigo) ->
	case mnesia:dirty_index_read(user_db, Codigo, #user.codigo) of
		[] -> case mnesia:dirty_index_read(user_fs, Codigo, #user.codigo) of
				[] -> {error, enoent};
				[Record|_] -> {ok, setelement(1, Record, user)}
			  end;
		[Record|_] -> {ok, setelement(1, Record, user)}
	end.


-spec find_by_login(binary() | string()) -> #user{} | {error, enoent}.
find_by_login(<<>>) -> {error, enoent};	
find_by_login("") -> {error, enoent};	
find_by_login(undefined) -> {error, enoent};	
find_by_login(Login) ->
	case is_list(Login) of
		true -> LoginStr = string:to_lower(Login);
		false -> LoginStr = string:to_lower(binary_to_list(Login))
	end,
	LoginBin = list_to_binary(LoginStr),
	case mnesia:dirty_index_read(user_db, LoginBin, #user.login) of
		[] -> 
			case mnesia:dirty_index_read(user_fs, LoginBin, #user.login) of
				[] -> 
					case find_by_email(LoginBin) of
						{ok, Record} -> {ok, Record};
						_ -> find_by_cpf(Login)
					end;
				[Record|_] -> {ok, setelement(1, Record, user)}
			end;
		[Record|_] -> {ok, setelement(1, Record, user)}
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
			case mnesia:dirty_index_read(user_db, EmailBin, #user.email) of
				[] -> 
					case mnesia:dirty_index_read(user_fs, EmailBin, #user.email) of
						[] -> {error, enoent};
						[Record|_] -> {ok, Record}
					end;
				[Record|_] -> {ok, Record}
			end;
		false -> {error, enoent}
	end.


-spec find_by_cpf(binary() | string()) -> #user{} | {error, enoent}.
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
			case mnesia:dirty_index_read(user_db, CpfBin, #user.cpf) of
				[] -> 
					case mnesia:dirty_index_read(user_fs, CpfBin, #user.cpf) of
						[] -> {error, enoent};
						[Record|_] -> {ok, setelement(1, Record, user)}
					end;
				[Record|_] -> {ok, setelement(1, Record, user)}
			end;
		false -> 
			% tenta inserir zeros na frente e refaz a pesquisa
			case LenCpf of
				10 -> Cpf2 = "0" ++ CpfStr;  
				 9 -> Cpf2 = "00" ++ CpfStr;
				 _ -> Cpf2 = CpfStr
			end,
			LenCpf2 = string:len(Cpf2),
			Cpf2Bin = list_to_binary(Cpf2),
			case (LenCpf2 =:= 11 orelse LenCpf2 =:= 14) of  % deve ser CPF ou CNPJ
				true ->
					case mnesia:dirty_index_read(user_db, Cpf2Bin, #user.cpf) of
						[] -> 
							case mnesia:dirty_index_read(user_fs, Cpf2Bin, #user.cpf) of
								[] -> {error, enoent};
								[Record|_] -> {ok, setelement(1, Record, user)}
							end;
						[Record|_] -> {ok, setelement(1, Record, user)}
					end;
				false -> {error, enoent}
			end
	end.

-spec find_by_name(binary() | string()) -> {ok, #user{}} | {error, enoent}.
find_by_name(<<>>) -> {error, enoent};
find_by_name("") -> {error, enoent};
find_by_name(undefined) -> {error, enoent};
find_by_name(Name) when is_list(Name) -> 
	find_by_name(list_to_binary(Name));
find_by_name(Name) -> 
	case ems_db:find_first(user_db, [{name, "==", Name}]) of
		{error, Reason} ->
			case ems_db:find_first(user_fs, [{name, "==", Name}]) of
				{error, Reason} -> {error, enoent};
				Record -> {ok, setelement(1, Record, user)}
			end;
		Record -> {ok, setelement(1, Record, user)}
	end.

-spec find_by_login_and_password(binary() | list(), binary() | list()) -> {ok, #user{}} | {error, enoent}.	
find_by_login_and_password(Login, Password)  ->
	case find_by_login(Login) of
		{ok, User = #user{password = PasswordUser}} -> 
			PasswordBin = case is_list(Password) of
							true -> list_to_binary(Password);
							_ -> Password
						  end,
			case PasswordUser =:= ems_util:criptografia_sha1(Password) orelse PasswordUser =:= PasswordBin of
				true -> {ok, User};
				false -> {error, enoent}
			end;
		_ -> {error, enoent}
	end.


-spec to_resource_owner(#user{} | undefined) -> binary().
to_resource_owner(undefined) -> <<"{}"/utf8>>;
to_resource_owner(User) ->
	ems_schema:to_json({<<"id">>, User#user.id,
						 <<"codigo">>, User#user.codigo,
						 <<"login">>, User#user.login, 
						 <<"name">>, User#user.name,
						 <<"email">>, User#user.email,
						 <<"type">>, User#user.type}).


-spec add_user(binary(), binary()) -> {ok, #user{}} | {error, atom()}.
add_user(Login, Password) ->
	User = #user{login = Login, password = Password},
	insert(User).


-spec new_from_map(map(), #config{}) -> {ok, #user{}} | {error, atom()}.
new_from_map(Map, Conf) -> new_from_map(Map, Conf, undefined).
new_from_map(Map, _Conf, Id) ->
	try
		PasswdCrypto = maps:get(<<"passwd_crypto">>, Map, <<>>),
		Password = maps:get(<<"password">>, Map, <<>>),
		{ok, #user{id = Id,
					codigo = maps:get(<<"codigo">>, Map, undefined),
					codigo_pessoa = maps:get(<<"codigo_pessoa">>, Map, undefined),
					login = ?UTF8_STRING(maps:get(<<"login">>, Map, <<>>)),
					name = ?UTF8_STRING(maps:get(<<"name">>, Map, <<>>)),
					cpf = ?UTF8_STRING(maps:get(<<"cpf">>, Map, <<>>)),
					password = case PasswdCrypto of
									<<"SHA1">> -> ?UTF8_STRING(Password);
									_ -> ems_util:criptografia_sha1(Password)
							   end,
					passwd_crypto = <<"SHA1">>,
					endereco = ?UTF8_STRING(maps:get(<<"endereco">>, Map, <<>>)),
					complemento_endereco = ?UTF8_STRING(maps:get(<<"complemento_endereco">>, Map, <<>>)),
					bairro = ?UTF8_STRING(maps:get(<<"bairro">>, Map, <<>>)),
					cidade = ?UTF8_STRING(maps:get(<<"cidade">>, Map, <<>>)),
					uf = ?UTF8_STRING(maps:get(<<"uf">>, Map, <<>>)),
					cep = ?UTF8_STRING(maps:get(<<"cep">>, Map, <<>>)),
					rg = ?UTF8_STRING(maps:get(<<"rg">>, Map, <<>>)),
					data_nascimento = ems_util:date_to_binary(maps:get(<<"data_nascimento">>, Map, <<>>)),
					sexo = maps:get(<<"sexo">>, Map, <<>>),
					telefone = ?UTF8_STRING(maps:get(<<"telefone">>, Map, <<>>)),
					celular = ?UTF8_STRING(maps:get(<<"celular">>, Map, <<>>)),
					ddd = ?UTF8_STRING(maps:get(<<"ddd">>, Map, <<>>)),
					ctrl_path = maps:get(<<"ctrl_path">>, Map, <<>>),
					ctrl_file = maps:get(<<"ctrl_file">>, Map, <<>>),
					ctrl_modified = maps:get(<<"ctrl_modified">>, Map, undefined),
					ctrl_hash = erlang:phash2(Map)
			}
		}
	catch
		_Exception:Reason -> 
			ems_logger:format_warn("ems_user parse invalid user specification: ~p\n\t~p.\n", [Reason, Map]),
			{error, Reason}
	end.


-spec get_table(fs | db) -> user_db | user_fs.
get_table(db) -> user_db;
get_table(fs) -> user_fs.

-spec find(user_fs | user_db, non_neg_integer()) -> {ok, #user{}} | {error, atom()}.
find(Table, Codigo) ->
	case ems_db:find_first(Table, [{codigo, "==", Codigo}]) of
		{error, Reason} -> {error, Reason};
		Record -> {ok, setelement(1, Record, user)}
	end.

-spec all(user_fs | user_db) -> list() | {error, atom()}.
all(Table) ->
	case ems_db:all(Table) of
		{ok, Records} -> 
			Records2 = [setelement(1, R, user) || R <- Records],
			{ok, Records2};
		Error -> Error
	end.


%% middleware functions

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

delete(Id) -> ems_db:delete(user, Id).

valida(_User, _Operation) -> ok.
