%%********************************************************************
%% @title Module ems_user_loader_middleware
%% @version 1.0.0
%% @doc Module responsible for load user from filesystem or db
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_loader_middleware).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([insert/4, update/4, is_empty/1, size_table/1, clear_table/1, reset_sequence/1, get_filename/0]).

-spec insert(map(), tuple(), #config{}, fs | db) -> {ok, #service{}, atom(), insert | update} | {ok, skip} | {error, atom()}.
insert(Map, CtrlInsert, Conf, SourceType) ->
	prepare_insert_or_update(Map, CtrlInsert, Conf, SourceType).


-spec update(tuple(), tuple(), #config{}, fs | db) -> {ok, #service{}, atom(), insert | update} | {ok, skip} | {error, atom()}.
update(Map, CtrlUpdate, Conf, SourceType) ->
	prepare_insert_or_update(Map, CtrlUpdate, Conf, SourceType).


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
	

%% internal functions

-spec get_filename() -> list(tuple()).
get_filename() -> 
	Conf = ems_config:getConfig(),
	Conf#config.user_path_search.
	
	
-spec prepare_insert_or_update(map() | tuple(), tuple(), #config{}, atom()) -> {ok, #service{}, atom(), insert | update} | {ok, skip} | {error, atom()}.
prepare_insert_or_update(Map, CtrlDate, Conf, SourceType) ->
	try
		case ems_user:new_user_from_map(Map, Conf) of
			{ok, NewUser = #user{codigo = Codigo, ctrl_modified = CtrlModified, ctrl_hash = CtrlHash}} -> 
				Table = ems_user:get_table(SourceType),
				case ems_user:find(Table, Codigo) of
					{error, enoent} -> 
						Id = ems_db:sequence(Table),
						User = NewUser#user{id = Id,
											      ctrl_insert = CtrlDate},
						{ok, User, Table, insert};
					{ok, CurrentUser = #user{ctrl_modified = CurrentCtrlModified, ctrl_hash = CurrentCtrlHash}} ->
						case CtrlHash =/= CurrentCtrlHash of
							true ->
								case CtrlModified == undefined orelse CtrlModified > CurrentCtrlModified of
									true ->
										?DEBUG("ems_user_loader_middleware update ~p from ~p.", [Map, SourceType]),
										User = CurrentUser#user{
														 codigo = Codigo,
														 codigo_pessoa = NewUser#user.codigo_pessoa,
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
														 ctrl_modified = CtrlModified,
														 ctrl_hash = CtrlHash
													},
										{ok, User, Table, update};
									false -> {ok, skip}
								end;
							false -> {ok, skip}
						end;
					X -> io:format("erro find ~p\n", [X])
				end;
			Error -> Error
		end

	catch
		_Exception:Reason -> {error, Reason}
	end.

