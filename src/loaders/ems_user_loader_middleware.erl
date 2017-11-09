%%********************************************************************
%% @title Module ems_user_loader_middleware
%% @version 1.0.0
%% @doc Module responsible for load users
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_loader_middleware).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([insert_or_update/5, is_empty/1, size_table/1, clear_table/1, reset_sequence/1, get_filename/0, check_remove_records/2]).


-spec is_empty(fs | db) -> boolean().
is_empty(db) ->	mnesia:table_info(user_db, size) == 0;
is_empty(fs) ->	mnesia:table_info(user_fs, size) == 0.
	

-spec size_table(fs | db) -> non_neg_integer().
size_table(db) -> mnesia:table_info(user_db, size);
size_table(fs) -> mnesia:table_info(user_fs, size).
	

-spec clear_table(fs | db) -> ok | {error, efail_clear_ets_table}.
clear_table(db) ->	
	case mnesia:clear_table(user_db) of
		{atomic, ok} -> ok;
		_ -> {error, efail_clear_ets_table}
	end;
clear_table(fs) ->	
	case mnesia:clear_table(user_fs) of
		{atomic, ok} -> ok;
		_ -> {error, efail_clear_ets_table}
	end.
	
	
-spec reset_sequence(fs | db) -> ok.
reset_sequence(db) -> 
	ems_db:init_sequence(user_db, 0),
	ok;
reset_sequence(fs) ->	
	ems_db:init_sequence(user_fs, 0),
	ok.
	
	
-spec check_remove_records(list(), fs | db) -> non_neg_integer().	
check_remove_records(_Codigos, _SourceType) -> 0.
	

-spec get_filename() -> list(tuple()).
get_filename() -> 
	Conf = ems_config:getConfig(),
	Conf#config.user_path_search.
	
	
-spec insert_or_update(map() | tuple(), tuple(), #config{}, atom(), insert | update) -> {ok, #service{}, atom(), insert | update} | {ok, skip} | {error, atom()}.
insert_or_update(Map, CtrlDate, Conf, SourceType, _Operation) ->
	try
		case ems_user:new_from_map(Map, Conf) of
			{ok, NewUser = #user{id = Id, ctrl_hash = CtrlHash}} -> 
				Table = ems_user:get_table(SourceType),
				case ems_user:find(Table, Id) of
					{error, enoent} -> 
						case SourceType == fs orelse (SourceType == db andalso not ems_user:exist(user_fs, Id)) of
							true ->
								User = NewUser#user{ctrl_insert = CtrlDate},
								{ok, User, Table, insert};
							false ->
								{ok, skip}
						end;
					{ok, CurrentUser = #user{ctrl_hash = CurrentCtrlHash}} ->
						case CtrlHash =/= CurrentCtrlHash of
							true ->
								?DEBUG("ems_user_loader_middleware update ~p from ~p.", [Map, SourceType]),
								User = CurrentUser#user{
												 codigo = NewUser#user.codigo,
												 login = NewUser#user.login,
												 name = NewUser#user.name,
												 cpf = NewUser#user.cpf,
												 password = NewUser#user.password,
												 passwd_crypto = NewUser#user.passwd_crypto,
												 endereco = NewUser#user.endereco,
												 complemento_endereco = NewUser#user.complemento_endereco,
												 bairro = NewUser#user.bairro,
												 cidade = NewUser#user.cidade,
												 uf = NewUser#user.uf,
												 cep = NewUser#user.cep,
												 rg = NewUser#user.rg,
												 data_nascimento = NewUser#user.data_nascimento,
												 sexo = NewUser#user.sexo,
												 telefone = NewUser#user.telefone,
												 celular = NewUser#user.celular,
												 ddd = NewUser#user.ddd,
												 ctrl_path = NewUser#user.ctrl_path,
												 ctrl_file = NewUser#user.ctrl_file,
												 ctrl_update = CtrlDate,
												 ctrl_modified = NewUser#user.ctrl_modified,
												 ctrl_hash = NewUser#user.ctrl_hash
											},
								{ok, User, Table, update};
							false -> 
								{ok, skip}
						end
				end;
			Error -> Error
		end

	catch
		_Exception:Reason -> {error, Reason}
	end.

