%%********************************************************************
%% @title Module ems_user_endereco_loader_middleware
%% @version 1.0.0
%% @doc Module responsible for load endereco
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_endereco_loader_middleware).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([insert_or_update/5, is_empty/1, size_table/1, clear_table/1, reset_sequence/1, get_filename/0, check_remove_records/2]).


-spec is_empty(fs | db) -> boolean().
is_empty(db) ->	mnesia:table_info(user_endereco_db, size) == 0;
is_empty(fs) ->	mnesia:table_info(user_endereco_fs, size) == 0.
	

-spec size_table(fs | db) -> non_neg_integer().
size_table(db) -> mnesia:table_info(user_endereco_db, size);
size_table(fs) -> mnesia:table_info(user_endereco_fs, size).
	

-spec clear_table(fs | db) -> ok | {error, efail_clear_ets_table}.
clear_table(db) ->	
	case mnesia:clear_table(user_endereco_db) of
		{atomic, ok} -> ok;
		_ -> {error, efail_clear_ets_table}
	end;
clear_table(fs) ->	
	case mnesia:clear_table(user_endereco_fs) of
		{atomic, ok} -> ok;
		_ -> {error, efail_clear_ets_table}
	end.
	
	
-spec reset_sequence(fs | db) -> ok.
reset_sequence(db) -> 
	ems_db:init_sequence(user_endereco_db, 0),
	ok;
reset_sequence(fs) ->	
	ems_db:init_sequence(user_endereco_fs, 0),
	ok.

-spec check_remove_records(list(), fs | db) -> non_neg_integer().	
check_remove_records(_Codigos, _SourceType) -> 0.
	

%% internal functions

-spec get_filename() -> list(tuple()).
get_filename() -> 
	Conf = ems_config:getConfig(),
	Conf#config.user_endereco_path_search.
	

update_endereco_tabela_users(Endereco, SourceType, CodigoPessoa) ->
	UserTable = ems_user:get_table(SourceType),
	case ems_user:find_by_codigo_pessoa(UserTable, CodigoPessoa) of
		{ok, Users} -> 
			update_endereco_tabela_users_(Users, Endereco, UserTable);
		_ -> ok
	end.


-spec update_endereco_tabela_users_(list(#user{}), #user_endereco{}, atom()) -> ok.
update_endereco_tabela_users_([], _, _) -> ok;
update_endereco_tabela_users_([User|UserT], 
							 Endereco, 
							 UserTable) -> 
	User2 = User#user{endereco = Endereco#user_endereco.endereco,
					  complemento_endereco = Endereco#user_endereco.complemento,
					  bairro = Endereco#user_endereco.bairro,
					  cidade = Endereco#user_endereco.cidade,
					  uf = Endereco#user_endereco.uf,
					  cep = Endereco#user_endereco.cep},
	mnesia:dirty_write(UserTable, User2),
	update_endereco_tabela_users_(UserT, Endereco, UserTable).
	
	
-spec insert_or_update(map() | tuple(), tuple(), #config{}, atom(), insert | update) -> {ok, #service{}, atom(), insert | update} | {ok, skip} | {error, atom()}.
insert_or_update(Map, CtrlDate, Conf, SourceType, _Operation) ->
	try
		case ems_user_endereco:new_from_map(Map, Conf) of
			{ok, NewRecord = #user_endereco{id = Id, ctrl_hash = CtrlHash, codigo = CodigoPessoa}} -> 
				Table = ems_user_endereco:get_table(SourceType),
				case ems_user_endereco:find(Table, Id) of
					{error, enoent} -> 
						Record = NewRecord#user_endereco{ctrl_insert = CtrlDate},
						update_endereco_tabela_users(Record, case SourceType of
																db -> user_db;
																fs -> user_fs
															end, CodigoPessoa),
						{ok, Record, Table, insert};
					{ok, CurrentRecord = #user_endereco{ctrl_hash = CurrentCtrlHash}} ->
						case CtrlHash =/= CurrentCtrlHash of
							true ->
								?DEBUG("ems_user_endereco_loader_middleware update ~p from ~p.", [Map, SourceType]),
								Record = CurrentRecord#user_endereco{
												 codigo = CodigoPessoa,
												 endereco = NewRecord#user_endereco.endereco,
												 complemento = NewRecord#user_endereco.complemento,
												 bairro = NewRecord#user_endereco.bairro,
												 cidade = NewRecord#user_endereco.cidade,
												 uf = NewRecord#user_endereco.uf,
												 cep = NewRecord#user_endereco.cep,
												 type = NewRecord#user_endereco.type,
												 ctrl_path = NewRecord#user_endereco.ctrl_path,
												 ctrl_file = NewRecord#user_endereco.ctrl_file,
												 ctrl_update = CtrlDate,
												 ctrl_modified = NewRecord#user_endereco.ctrl_modified,
												 ctrl_hash = NewRecord#user_endereco.ctrl_hash
											},
								update_endereco_tabela_users(Record, case SourceType of
																db -> user_db;
																fs -> user_fs
														  end, CodigoPessoa),

								{ok, Record, Table, update};
							false -> {ok, skip}
						end
				end;
			Error -> Error
		end

	catch
		_Exception:Reason -> {error, Reason}
	end.


	


