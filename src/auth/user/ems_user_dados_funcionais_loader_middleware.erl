%%********************************************************************
%% @title Module ems_user_dados_funcionais_loader_middleware
%% @version 1.0.0
%% @doc Module responsible for load dados funcionais from filesystem or db
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_dados_funcionais_loader_middleware).

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
	Conf#config.user_dados_funcionais_path_search.
	
	
-spec prepare_insert_or_update(map() | tuple(), tuple(), #config{}, atom()) -> {ok, #service{}, atom(), insert | update} | {ok, skip} | {error, atom()}.
prepare_insert_or_update(Map, CtrlDate, Conf, SourceType) ->
	try
		case ems_user_dados_funcionais:new_from_map(Map, Conf) of
			{ok, NewRecord = #user_dados_funcionais{codigo = Codigo, ctrl_modified = CtrlModified, ctrl_hash = CtrlHash}} -> 
				Table = ems_user_dados_funcionais:get_table(SourceType),
				case ems_user_dados_funcionais:find(Table, Codigo) of
					{error, enoent} -> 
						Id = ems_db:sequence(Table),
						Record = NewRecord#user_dados_funcionais{id = Id,
											ctrl_insert = CtrlDate},
						{ok, Record, Table, insert};
					{ok, CurrentRecord = #user_dados_funcionais{ctrl_modified = CurrentCtrlModified, ctrl_hash = CurrentCtrlHash}} ->
						io:format("is update... ~p\n", [Map]),
						case CtrlHash =/= CurrentCtrlHash of
							true ->
								case CtrlModified == undefined orelse CtrlModified > CurrentCtrlModified of
									true ->
										?DEBUG("ems_user_dados_funcionais_loader_middleware update ~p from ~p.", [Map, SourceType]),
										Record = CurrentRecord#user_dados_funcionais{
														 codigo = Codigo,
														 type = NewRecord#user_dados_funcionais.type,
														 subtype = NewRecord#user_dados_funcionais.subtype,
														 active = NewRecord#user_dados_funcionais.active,
														 matricula = NewRecord#user_dados_funcionais.matricula,
														 lotacao = NewRecord#user_dados_funcionais.lotacao,
														 lotacao_sigla = NewRecord#user_dados_funcionais.lotacao_sigla,
														 lotacao_centro = NewRecord#user_dados_funcionais.lotacao_centro,
														 lotacao_codigo_funcao = NewRecord#user_dados_funcionais.lotacao_codigo_funcao,
														 lotacao_funcao = NewRecord#user_dados_funcionais.lotacao_funcao,
														 lotacao_orgao = NewRecord#user_dados_funcionais.lotacao_orgao,
														 lotacao_codigo_cargo = NewRecord#user_dados_funcionais.lotacao_codigo_cargo,
														 lotacao_cargo = NewRecord#user_dados_funcionais.lotacao_cargo,
														 ctrl_path = NewRecord#user_dados_funcionais.ctrl_path,
														 ctrl_file = NewRecord#user_dados_funcionais.ctrl_file,
														 ctrl_update = CtrlDate,
														 ctrl_modified = CtrlModified,
														 ctrl_hash = CtrlHash
													},
										{ok, Record, Table, update};
									false -> {ok, skip}
								end;
							false -> {ok, skip}
						end;
					X -> io:format("erro find ~p\n", [X]),
						{ok, skip}
				end;
			Error -> Error
		end

	catch
		_Exception:Reason -> {error, Reason}
	end.

