%%********************************************************************
%% @title Module ems_db
%% @version 1.0.0
%% @doc Module that provides interface with the database.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_db).

-export([start/0]).
-export([get/2, exist/2, all/1, insert/1, insert/2, update/1, delete/2, 
		 match/2, find/2, find/3, find/5, find_by_id/2, find_by_id/3, filter/2, 
		 filter_with_limit/4, select_fields/2, 
		 find_first/2, find_first/3, find_first/4, field_position/3]).
-export([init_sequence/2, sequence/1, sequence/2, current_sequence/1]).
-export([init_counter/2, counter/2, current_counter/1, inc_counter/1, dec_counter/1]).
-export([get_connection/1, release_connection/1, get_sqlite_connection_from_csv_file/1, create_datasource_from_map/1, create_datasource_from_map/2]).
-export([get_param/1, get_param/2, set_param/2, get_re_param/2]).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").



%% *********** Database schema creation ************

start() ->
	create_database([node()]),
	ems_cache:new(ems_db_odbc_connection_cache),
	ems_cache:new(ems_db_parsed_query_cache).
	
-spec create_database(list()) -> ok.	
create_database(Nodes) ->
	% Define a pasta de armazenamento dos databases
	filelib:ensure_dir(?DATABASE_PATH ++ "/"),
	application:set_env(mnesia, dir, ?DATABASE_PATH),
	mnesia:create_schema(Nodes),
	mnesia:start(),

    mnesia:create_table(service_datasource, [{type, set},
											 {ram_copies, Nodes},
											 {attributes, record_info(fields, service_datasource)}]),

    mnesia:create_table(user_fs, [{type, set},
								 {ram_copies, Nodes},
								  {index, [#user.codigo, #user.login, #user.cpf, #user.email]},
								  {attributes, record_info(fields, user)},
								  {record_name, user}]),

    mnesia:create_table(user_db, [{type, set},
								  {disc_copies, Nodes},
								  {index, [#user.codigo, #user.login, #user.cpf, #user.email]},
								  {attributes, record_info(fields, user)},
								  {record_name, user}]),

    mnesia:create_table(user_aluno_ativo_db, [{type, set},
								  {disc_copies, Nodes},
								  {index, [#user.codigo, #user.login, #user.cpf, #user.email]},
								  {attributes, record_info(fields, user)},
								  {record_name, user}]),

    mnesia:create_table(user_aluno_inativo_db, [{type, set},
								  {disc_copies, Nodes},
								  {index, [#user.codigo, #user.login, #user.cpf, #user.email]},
								  {attributes, record_info(fields, user)},
								  {record_name, user}]),

    mnesia:create_table(user_dados_funcionais_fs, [{type, set},
								  {ram_copies, Nodes},
								  {attributes, record_info(fields, user_dados_funcionais)},
								  {record_name, user_dados_funcionais}]),

    mnesia:create_table(user_dados_funcionais_db, [{type, set},
								  {disc_copies, Nodes},
								  {attributes, record_info(fields, user_dados_funcionais)},
								  {record_name, user_dados_funcionais}]),

    mnesia:create_table(user_email_fs, [{type, set},
 								  {ram_copies, Nodes},
								  {attributes, record_info(fields, user_email)},
								  {record_name, user_email}]),

    mnesia:create_table(user_email_db, [{type, set},
								  {disc_copies, Nodes},
								  {attributes, record_info(fields, user_email)},
								  {record_name, user_email}]),

    mnesia:create_table(user_endereco_fs, [{type, set},
 								  {ram_copies, Nodes},
								  {attributes, record_info(fields, user_endereco)},
								  {record_name, user_endereco}]),

    mnesia:create_table(user_endereco_db, [{type, set},
								  {disc_copies, Nodes},
								  {attributes, record_info(fields, user_endereco)},
								  {record_name, user_endereco}]),

    mnesia:create_table(user_telefone_fs, [{type, set},
 								  {ram_copies, Nodes},
								  {attributes, record_info(fields, user_telefone)},
								  {record_name, user_telefone}]),

    mnesia:create_table(user_telefone_db, [{type, set},
								  {disc_copies, Nodes},
								  {attributes, record_info(fields, user_telefone)},
								  {record_name, user_telefone}]),

	mnesia:create_table(user_perfil_fs, [{type, set},
									    {ram_copies, Nodes},
										{index, [#user_perfil.user_id, #user_perfil.client_id]},
										{attributes, record_info(fields, user_perfil)},
										{record_name, user_perfil}]),

	mnesia:create_table(user_perfil_db, [{type, set},
										{disc_copies, Nodes},
										{index, [#user_perfil.user_id, #user_perfil.client_id]},
									    {attributes, record_info(fields, user_perfil)},
									    {record_name, user_perfil}]),

	mnesia:create_table(user_permission_fs, [{type, set},
										{ram_copies, Nodes},
										{index, [#user_permission.user_id, #user_permission.client_id]},
										{attributes, record_info(fields, user_permission)},
										{record_name, user_permission}]),

	mnesia:create_table(user_permission_db, [{type, set},
										{disc_copies, Nodes},
										{index, [#user_permission.user_id, #user_permission.client_id]},
									    {attributes, record_info(fields, user_permission)},
									    {record_name, user_permission}]),

    mnesia:create_table(client_db, [{type, set},
									{disc_copies, Nodes},
									{attributes, record_info(fields, client)},
									{record_name, client}]),

    mnesia:create_table(client_fs, [{type, set},
								    {ram_copies, Nodes},
									{attributes, record_info(fields, client)},
									{record_name, client}]),

    mnesia:create_table(sequence, [{type, set},
								   {disc_copies, Nodes},
								   {attributes, record_info(fields, sequence)}]),

    mnesia:create_table(request, [{type, set},
								  {ram_copies, Nodes},
								  {attributes, record_info(fields, request)},
								  {index, [#request.timestamp]}]),

    mnesia:create_table(ctrl_sqlite_table, [{type, set},
											{disc_copies, Nodes},
											{attributes, record_info(fields, ctrl_sqlite_table)}]),

    mnesia:create_table(catalog_schema, [{type, set},
										 {disc_copies, Nodes},
										 {attributes, record_info(fields, catalog_schema)}]),

    mnesia:create_table(produto, [{type, set},
	 							  {disc_copies, Nodes},
								  {attributes, record_info(fields, produto)}]),

    mnesia:create_table(service_owner, [{type, set},
	 							  {disc_copies, Nodes},
								  {attributes, record_info(fields, service_owner)}]),

    mnesia:create_table(ctrl_params, [{type, set},
									  {disc_copies, Nodes},
									  {attributes, record_info(fields, ctrl_params)}]),
									  

    mnesia:create_table(catalog_get_fs, [{type, set},
									  {ram_copies, Nodes},
									  {index, [#service.rowid]},
									  {attributes, record_info(fields, service)},
									  {record_name, service}]),

    mnesia:create_table(catalog_post_fs, [{type, set},
 									  {ram_copies, Nodes},
									  {index, [#service.rowid]},
									  {attributes, record_info(fields, service)},
									  {record_name, service}]),

    mnesia:create_table(catalog_put_fs, [{type, set},
									  {ram_copies, Nodes},
									  {index, [#service.rowid]},
									  {attributes, record_info(fields, service)},
									  {record_name, service}]),

    mnesia:create_table(catalog_delete_fs, [{type, set},
									  {ram_copies, Nodes},
									  {index, [#service.rowid]},
									  {attributes, record_info(fields, service)},
									  {record_name, service}]),

    mnesia:create_table(catalog_options_fs, [{type, set},
									      {ram_copies, Nodes},
										  {index, [#service.rowid]},
										  {attributes, record_info(fields, service)},
										  {record_name, service}]),

    mnesia:create_table(catalog_kernel_fs, [{type, set},
										  {ram_copies, Nodes},
										  {index, [#service.rowid]},
										  {attributes, record_info(fields, service)},
										  {record_name, service}]),

    mnesia:create_table(catalog_re_fs, [{type, set},
									      {ram_copies, Nodes},
										  {index, [#service.rowid]},
										  {attributes, record_info(fields, service)},
										  {record_name, service}]),

    mnesia:create_table(catalog_get_db, [{type, set},
										  {disc_copies, Nodes},
										  {index, [#service.rowid]},
										  {attributes, record_info(fields, service)},
										  {record_name, service}]),

    mnesia:create_table(catalog_post_db, [{type, set},
										  {disc_copies, Nodes},
										  {index, [#service.rowid]},
										  {attributes, record_info(fields, service)},
										  {record_name, service}]),

    mnesia:create_table(catalog_put_db, [{type, set},
										  {disc_copies, Nodes},
										  {index, [#service.rowid]},
										  {attributes, record_info(fields, service)},
										  {record_name, service}]),

    mnesia:create_table(catalog_delete_db, [{type, set},
										  {disc_copies, Nodes},
										  {index, [#service.rowid]},
										  {attributes, record_info(fields, service)},
										  {record_name, service}]),

    mnesia:create_table(catalog_options_db, [{type, set},
											  {disc_copies, Nodes},
											  {index, [#service.rowid]},
											  {attributes, record_info(fields, service)},
											  {record_name, service}]),

    mnesia:create_table(catalog_kernel_db, [{type, set},
											  {disc_copies, Nodes},
											  {index, [#service.rowid]},
											  {attributes, record_info(fields, service)},
											  {record_name, service}]),

    mnesia:create_table(catalog_re_db, [{type, set},
										  {disc_copies, Nodes},
										  {index, [#service.rowid]},
										  {attributes, record_info(fields, service)},
										  {record_name, service}]),

    mnesia:create_table(counter, [{type, set},
								  {ram_copies, Nodes},
								  {attributes, record_info(fields, sequence)}]),

    mnesia:create_table(stat_counter_hist, [{type, set},
								    {disc_copies, Nodes},
								    {attributes, record_info(fields, stat_counter_hist)},
								    {record_name, stat_counter_hist}]),

	
	mnesia:wait_for_tables([service_datasource, user_fs, user_dados_funcionais_fs, 
							client_fs, sequence, user_email_fs, counter, ctrl_params, 
							user_permission_fs, user_endereco_fs, catalog_options_fs,
							catalog_get_fs, catalog_post_fs, catalog_put_fs, catalog_delete_fs,
							catalog_re_fs, catalog_kernel_fs, user_db, client_db], 15000),
	
	ok.



%% *********** Functions for CRUD ************

insert(RecordType, Record) ->
	F = fun() ->
		case element(2, Record) of
			undefined ->
				Id = sequence(RecordType),
				Record1 = setelement(2, Record, Id),
				mnesia:write(Record1),
				Record1;
			Id -> 
				case mnesia:read(RecordType, Id) of
					[] -> mnesia:write(Record),
						  Record;
					_ -> {error, ealready_exist}
				end
		end
	end,		
	case mnesia:transaction(F) of
		{atomic, Result} -> {ok, Result};
		Error -> Error
	end.

insert(Record) ->
	RecordType = element(1, Record),
	insert(RecordType, Record).

update(Record) ->
	Write = fun() -> mnesia:write(Record) end,
	mnesia:transaction(Write),
	ok.

delete(RecordType, Id) when is_list(Id) ->
	Id2 = list_to_integer(Id),
	delete(RecordType, Id2);
	
delete(RecordType, Id) ->
	Delete = fun() -> mnesia:delete({RecordType, Id}) end,
	mnesia:transaction(Delete),
	ok.

	

%% ************* Funções para gerar sequences *************

init_sequence(Name, Value) ->
     {atomic, ok} =	mnesia:transaction(fun() ->
						mnesia:write(#sequence{key=Name, index=Value})
					end),
     ok.
sequence(Name) ->  mnesia:dirty_update_counter(sequence, Name, 1).
current_sequence(Name) -> mnesia:dirty_update_counter(sequence, Name, 0).
sequence(Name, Inc) -> mnesia:dirty_update_counter(sequence, Name, Inc).


%% ************* Funções para gerar counters *************

init_counter(Name, Value) ->
     {atomic, ok} =	mnesia:transaction(fun() ->
						mnesia:write(#counter{key=Name, index=Value})
					end),
     ok.
inc_counter(Name) ->  mnesia:dirty_update_counter(counter, Name, 1).
dec_counter(Name) ->  mnesia:dirty_update_counter(counter, Name, -1).
current_counter(Name) -> mnesia:dirty_update_counter(counter, Name, 0).
counter(Name, Inc) -> mnesia:dirty_update_counter(counter, Name, Inc).
     

%% ************* Funções para armazenar parâmetros em crtl_params *************

% Return a param value from crtl_params table
-spec get_param(atom()) -> any().
get_param(ParamName) -> 
	case mnesia:dirty_read(ctrl_params, ParamName) of
		[] -> undefined;
		[#ctrl_params{value = Value}] -> Value
	end.

-spec get_param(atom(), function() | any()) -> any().
get_param(ParamName, Fun) when is_function(Fun) -> 
	case mnesia:dirty_read(ctrl_params, ParamName) of
		[] -> 
			Value = Fun(),
			set_param(ParamName, Value),
			Value;
		[#ctrl_params{value = Value}] -> Value
	end;
get_param(ParamName, DefaultValue) -> 
	case mnesia:dirty_read(ctrl_params, ParamName) of
		[] -> 
			set_param(ParamName, DefaultValue),
			DefaultValue;
		[#ctrl_params{value = Value}] -> Value
	end.
	
-spec get_re_param(atom(), string()) -> {re_pattern, term(), term(), term(), term()}.	
get_re_param(ParamName, DefaultREPattern) -> 
	case mnesia:dirty_read(ctrl_params, ParamName) of
		[] -> 
			{ok, Value} = re:compile(DefaultREPattern),
			set_param(ParamName, Value),
			Value;
		[#ctrl_params{value = Value}] -> Value
	end.

	
% Save a param value to crtl_params table
-spec set_param(atom(), any()) -> ok.
set_param(ParamName, ParamValue) -> 
	P = #ctrl_params{name = ParamName, value = ParamValue},
	mnesia:dirty_write(ctrl_params, P).



% Get the connection from a datasource (sqlserver, sqlite, ou mnesia)
get_connection(Datasource = #service_datasource{type = sqlserver}) ->
	get_odbc_connection(Datasource);
get_connection(Datasource = #service_datasource{type = csvfile}) ->
	get_sqlite_connection_from_csv_file(Datasource);
get_connection(Datasource = #service_datasource{type = mnesia}) ->
	{ok, Datasource}.

% Release a conection from a datasource
release_connection(#service_datasource{type = mnesia}) -> ok;
release_connection(Datasource) -> ems_odbc_pool:release_connection(Datasource).

get_odbc_connection(Datasource) -> ems_odbc_pool:get_connection(Datasource).

create_sqlite_from_csv(#service_datasource{connection = Filename,
										   table_name = TableName,
										   csv_delimiter = Delimiter}) -> 
	FilenamePath = ?CSV_FILE_PATH ++ "/" ++ Filename,
	case filelib:last_modified(FilenamePath) of
		0 -> {error, ecsvfile_not_exist};
		LastModified ->
			SqliteFile = ?DATABASE_PATH ++ "/sqlite3_" ++ TableName,
			DatabaseExist = filelib:is_file(SqliteFile), 
			F = fun() ->
				Ctrl = ems_util:hd_or_empty(mnesia:read(ctrl_sqlite_table, Filename)),
				case Ctrl =:= [] orelse not DatabaseExist orelse Ctrl#ctrl_sqlite_table.last_modified =/= LastModified of
					true ->
						Csv2SqliteCmd = lists:flatten(io_lib:format('~s "~s" "~s" "~s" "~s"',
																	 [?CSV2SQLITE_PATH,
																	  SqliteFile, 
																	  TableName, 
																	  FilenamePath, 
																	  Delimiter])),
						ems_logger:info("ems_db execute OS command: ~p.", [Csv2SqliteCmd]),
						os:cmd(Csv2SqliteCmd),
						mnesia:write(#ctrl_sqlite_table{file_name = Filename, last_modified = LastModified});
					false -> 
						% Não foi necessário criar o arquivo csv. Não houve mudança no arquivo csv
						ok
				end
			end,
			mnesia:activity(transaction, F),
			SqliteFile
	end.


get_sqlite_connection_from_csv_file(Datasource = #service_datasource{driver = Driver}) -> 
	SqliteFile = create_sqlite_from_csv(Datasource),
	case Driver of
		odbc ->
			StringConnection = lists:flatten(io_lib:format("DRIVER=SQLite;Version=3;Database=~s;", [SqliteFile])),
			ems_odbc_pool:get_connection(Datasource#service_datasource{type = sqlite, connection = StringConnection});
		sqlite3 ->
			ems_odbc_pool:get_connection(Datasource#service_datasource{type = sqlite, connection = SqliteFile});
		_ -> erlang:error(einvalid_driver_datasource)
	end.


%create_sqlite_virtual_table_from_csv_file(Filename, TableName, _PrimaryKey) -> 
%	{ok, Conn} = ems_db:get_odbc_connection("DRIVER=SQLite;Version=3;New=True;"),
%	odbc:sql_query(Conn, "select load_extension(\"/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtable.so\")"),
%	CreateTableDDL = lists:flatten(io_lib:format("create virtual table ~s using csvtable(\"~s\")", [TableName, Filename])),
%	odbc:sql_query(Conn, CreateTableDDL),
%	odbc:commit(Conn, commit),
%	{ok, Conn}.


%% ************* Funções para pesquisa *************

%
% Find object by id. Return all fields.
% Ex.: ems_db:find_by_id(catalog_schema, 1).
% Sample result is {<<"id">>,1},{<<"name">>,<<"exemplo">>}
%
-spec get(atom() | list(atom()), non_neg_integer()) -> {ok, tuple()} | {error, enoent}.
get(Tab, Id) when is_atom(Tab) ->
	case Id > 0 of
		true ->
			case mnesia:dirty_read(Tab, Id) of
				[] -> {error, enoent};
				[Record|_] -> {ok, Record}
			end;
		false -> {error, enoent}
	end;
get([], _) -> {error, enoent};
get([Tab|TabT], Id) ->
	case mnesia:dirty_read(Tab, Id) of
		[] -> get(TabT, Id);
		[Record|_] -> {ok, Record}
	end.


%
% Check exist record by id. Return boolean.
% Ex.: ems_db:exist(catalog_schema, 1).
% Sample result is {<<"id">>,1},{<<"name">>,<<"exemplo">>}
%
-spec exist(atom() | list(atom()), non_neg_integer()) -> boolean().
exist(Tab, Id) when is_atom(Tab) ->
	case Id > 0 of
		true ->
			case mnesia:dirty_read(Tab, Id) of
				[] -> false;
				_ -> true
			end;
		false -> false
	end;
exist([], _) -> false;
exist([Tab|TabT], Id) ->
	case mnesia:dirty_read(Tab, Id) of
		[] -> exist(TabT, Id);
		_ -> true
	end.


-spec all(atom()) -> {ok, list(tuple())}.
all(Tab) ->
	F = fun() ->
		  qlc:e(
			 qlc:q([X || X <- mnesia:table(Tab)])
		  )
	   end,
	Records = mnesia:activity(async_dirty, F),
	{ok, Records}.


%
% Find object by id. Return all fields. 
% Ex.: ems_db:find_by_id(catalog_schema, 1).
% Sample result is {<<"id">>,1},{<<"name">>,<<"exemplo">>}
%
-spec find_by_id(atom() | list(atom()), non_neg_integer()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find_by_id(Tab, Id) -> get(Tab, Id).

%
% Find object by id
% Ex.: ems_db:find_by_id(catalog_schema, 1, [id, name]).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
-spec find_by_id(atom() | list(atom()), non_neg_integer(), list()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find_by_id(Tab, Id, FieldList) ->
	case get(Tab, Id) of
		{ok, Record} -> select_fields(Record, FieldList);
		Error -> Error
	end.


%
% Find objects. Return all fields.
% Ex.: ems_db:find(catalog_schema, [{id, "==", 1}]).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
-spec find(atom(), list()) -> {ok, tuple()} | {error, enoent}.
find(Tab, FilterList) -> find(Tab, [], FilterList).

%
% Find objects
% Ex.: ems_db:find(catalog_schema, [id, name], [{id, "==", 1}]).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
-spec find(atom() | list(atom()), list(), list()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find(Tab, FieldList, FilterList) when is_atom(Tab) ->
    Records = filter(Tab, FilterList),
	select_fields(Records, FieldList);
find(TabList, FieldList, FilterList) ->
    find_(TabList, FieldList, FilterList, []).

find_([], FieldList, _, Result) -> 
	select_fields(Result, FieldList);
find_([Tab|TabT], FieldList, FilterList, Result) ->
    Records = filter(Tab, FilterList),
	case Records =/= [] of
		true -> find_(TabT, FieldList, FilterList, Result ++ Records);
		false -> find_(TabT, FieldList, FilterList, Result)
	end.
	


%
% Find objects with limits
% Ex.: ems_db:find(catalog_schema, [id, name], [{id, "==", 1}], 1, 1).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
-spec find(atom() | list(atom()), list(), list(), non_neg_integer(), non_neg_integer()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find(Tab, FieldList, FilterList, Limit, Offset) when is_atom(Tab) -> 
    Records = filter_with_limit(Tab, FilterList, Limit, Offset),
	select_fields(Records, FieldList);
find(Tab, FieldList, FilterList, Limit, Offset) -> 
	find_(Tab, FieldList, FilterList, Limit, Offset, []).

find_([], FieldList, _, Limit, Offset, Result) -> 
	{ok, Result2} = select_fields(Result, FieldList),
	case Offset > length(Result2) of
		true -> {ok, []};
		false -> {ok, lists:sublist(Result2, Offset, Limit)}
	end;
find_([Tab|TabT], FieldList, FilterList, Limit, Offset, Result) -> 
    Records = filter_with_limit(Tab, FilterList, Limit, Offset),
    case Records =/= [] of 
		true -> find_(TabT, FieldList, FilterList, Limit, Offset, Result ++ Records);
		false -> find_(TabT, FieldList, FilterList, Limit, Offset, Result)
	end.

	


%
% Find first objects
% Ex.: ems_db:find_first(catalog_schema, [{id, "==", 1}]).
% Sample result is [{<<"id">>,1},{<<"name">>,<<"exemplo">>}]
%
-spec find_first(atom(), list()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find_first(Tab, FilterList) -> find_first(Tab, [], FilterList).


%
% Find first objects
% Ex.: ems_db:find_first(catalog_schema, [id, name], [{id, "==", 1}]).
% Sample result is [{<<"id">>,1},{<<"name">>,<<"exemplo">>}]
%
-spec find_first(atom() | list(atom()), list(), list()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find_first(Tab, FieldList, FilterList) when is_atom(Tab) ->
    case filter_with_limit(Tab, FilterList, 1, 1) of
		[] -> {error, enoent};
		[FirstRecord|_] -> select_fields(FirstRecord, FieldList)
	end;
find_first([], _, _) -> {error, enoent};
find_first([Tab|TabT], FieldList, FilterList) ->
    case filter_with_limit(Tab, FilterList, 1, 1) of
		[] -> find_first(TabT, FieldList, FilterList);
		[FirstRecord|_] -> select_fields(FirstRecord, FieldList)
	end.


%
% Find first objects
% Ex.: ems_db:find_first(catalog_schema, [id, name], [{id, "==", 1}], 1).
% Sample result is [{<<"id">>,1},{<<"name">>,<<"exemplo">>}]
%
-spec find_first(atom() | list(atom()), list(), list(), non_neg_integer()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find_first(Tab, FieldList, FilterList, Offset) when is_atom(Tab) ->
    case filter_with_limit(Tab, FilterList, 1, Offset) of
		[] -> {error, enoent};
		[FirstRecord|_] -> select_fields(FirstRecord, FieldList)
	end;
find_first([], _, _, _) -> {error, enoent};
find_first([Tab|TabT], FieldList, FilterList, Offset) ->
    case filter_with_limit(Tab, FilterList, 1, Offset) of
		[] -> find_first(TabT, FieldList, FilterList, Offset);
		[FirstRecord|_] -> select_fields(FirstRecord, FieldList)
	end.
	


%	
% Filter objects
% Ex.: ems_db:filter(catalog_schema, [{id, "==", 1}]). 
% Sample result is 
%[{catalog_schema,1,<<"exemplo">>,<<"schema de exemplo">>,
%                 #{<<"properties">> => #{<<"age">> => #{<<"description">> => <<"Age in years">>,
%                       <<"minimum">> => 0,
%                       <<"type">> => <<"integer">>},
%                     <<"firstName">> => #{<<"type">> => <<"string">>},
%                     <<"lastName">> => #{<<"type">> => <<"string">>}},
%                   <<"required">> => [<<"firstName">>,<<"lastName">>],
%                   <<"title">> => <<"Example Schema">>,
%                   <<"type">> => <<"object">>}}]
%
-spec filter(atom(), list(tuple())) -> list(tuple()).
filter(Tab, []) -> 
	F = fun() ->
		  qlc:e(
			 qlc:q([R || R <- mnesia:table(Tab)])
		  )
	   end,
	mnesia:activity(async_dirty, F);
filter(Tab, [{F1, "==", V1}]) ->
	Fields =  mnesia:table_info(Tab, attributes),
	FieldPosition = field_position(F1, Fields, 2),
	FieldValue = field_value(V1),
	case field_has_index(FieldPosition, Tab) of
		false ->
			Fun = fun() -> 
						qlc:e(qlc:q([R || R <- mnesia:table(Tab), element(FieldPosition, R) == FieldValue])) 
				  end,
			mnesia:activity(async_dirty, Fun);
		true ->
			mnesia:dirty_index_read(Tab, FieldValue, FieldPosition)
	end;
filter(Tab, FilterList) when is_list(FilterList) -> 
	F = fun() ->
			FieldsTable =  mnesia:table_info(Tab, attributes),
			Where = string:join([io_lib:format("element(~s, R) ~s ~p", [integer_to_list(field_position(F, FieldsTable, 2)), Op,  field_value(V)]) || {F, Op, V} <- FilterList], ","),
			ExprQuery = binary_to_list(iolist_to_binary([<<"[R || R <- mnesia:table(">>, atom_to_binary(Tab, utf8), <<"), ">>, Where, <<"].">>])),
			ParsedQuery = qlc:string_to_handle(ExprQuery),
			mnesia:activity(async_dirty, fun () -> qlc:eval(ParsedQuery) end)
		end,
	ems_cache:get(ems_db_parsed_query_cache, ?LIFE_TIME_PARSED_QUERY, {Tab, FilterList}, F);
filter(Tab, FilterTuple) when is_tuple(FilterTuple) ->
	filter(Tab, [FilterTuple]).



%	
% Filter objects with limit
% Ex.: ems_db:filter_with_limit(catalog_schema, [{id, "==", 1}], 1, 1). 
% Sample result is 
%[{catalog_schema,1,<<"exemplo">>,<<"schema de exemplo">>,
%                 #{<<"properties">> => #{<<"age">> => #{<<"description">> => <<"Age in years">>,
%                       <<"minimum">> => 0,
%                       <<"type">> => <<"integer">>},
%                     <<"firstName">> => #{<<"type">> => <<"string">>},
%                     <<"lastName">> => #{<<"type">> => <<"string">>}},
%                   <<"required">> => [<<"firstName">>,<<"lastName">>],
%                   <<"title">> => <<"Example Schema">>,
%                   <<"type">> => <<"object">>}}]
%
-spec filter_with_limit(atom(), list(), non_neg_integer(), non_neg_integer()) -> list(tuple()).
filter_with_limit(Tab, [], Limit, Offset) -> 
	TabSize = mnesia:table_info(Tab, size),
	case TabSize == 0 orelse Offset > TabSize orelse Limit < 1 orelse Offset < 1 of
		true -> [];
		false ->
			F = fun() ->
				  case Offset > 1 of
						true ->
							Q = qlc:cursor(qlc:q([R || R <- mnesia:table(Tab)])),
							qlc:next_answers(Q, Offset-1), % discart records
							Records = qlc:next_answers(Q, Limit),
							qlc:delete_cursor(Q),
							Records;
						false ->
							Q = qlc:cursor(qlc:q([R || R <- mnesia:table(Tab)])),
							Records = qlc:next_answers(Q, Limit),
							qlc:delete_cursor(Q),
							Records
				  end
			   end,
			mnesia:activity(async_dirty, F)
	end;
filter_with_limit(Tab, Filter = [{_, "==", _}], Limit, Offset) ->
	TabSize = mnesia:table_info(Tab, size),
	case TabSize == 0 orelse Offset > TabSize orelse Limit < 1 orelse Offset < 1 of
		true -> [];
		false ->
			Records = filter(Tab, Filter),
			case Offset > length(Records) of
				true -> [];
				false -> lists:sublist(Records, Offset, Limit)
			end
	end;
filter_with_limit(Tab, FilterList, Limit, Offset) when is_list(FilterList) -> 
	TabSize = mnesia:table_info(Tab, size),
	case TabSize == 0 orelse Offset > TabSize orelse Limit < 1 orelse Offset < 1 of
		true -> [];
		false ->
			F = fun() ->
					FieldsTable =  mnesia:table_info(Tab, attributes),
					Where = string:join([io_lib:format("element(~s, R) ~s ~p", [integer_to_list(field_position(F, FieldsTable, 2)), Op,  field_value(V)]) || {F, Op, V} <- FilterList], ","),
					ExprQuery = binary_to_list(iolist_to_binary([<<"[R || R <- mnesia:table(">>, atom_to_binary(Tab, utf8), <<"), ">>, Where, <<"].">>])),
					ParsedQuery = qlc:string_to_handle(ExprQuery),
					mnesia:activity(async_dirty, fun () -> 
													Records = qlc:eval(ParsedQuery),
													case Offset > length(Records) of
														true -> [];
														false -> lists:sublist(Records, Offset, Limit)
													end
												 end)
				end,
			ems_cache:get(ems_db_parsed_query_cache, ?LIFE_TIME_PARSED_QUERY, {Tab, FilterList, Limit, Offset}, F)
	end;
filter_with_limit(Tab, FilterTuple, Limit, Offset) when is_tuple(FilterTuple) -> 	
	filter_with_limit(Tab, [FilterTuple], Limit, Offset).



% match objects and faster than filter
% Ex.: ems_db:match(catalog_schema, [{id, 1}]).
% Sample result is like filter function
match(Tab, FilterList) -> 
	FieldsTable =  mnesia:table_info(Tab, attributes),
	Record = ems_schema:new_(Tab),
	Match = match(Tab, FilterList, FieldsTable, Record),
	mnesia:activity(async_dirty, fun() -> mnesia:match_object(Match) end).
		
match(_, [], _, Record) -> Record;
match(Tab, [{F, _, V}|T], FieldsTable, Record) -> 
	Fld = field_position(F, FieldsTable, 2),
	Record2 = setelement(Fld, Record, field_value(V)),
    match(Tab, T, FieldsTable, Record2);
match(Tab, [{F, V}|T], FieldsTable, Record) -> 
	Fld = field_position(F, FieldsTable, 2),
	Record2 = setelement(Fld, Record, field_value(V)),
    match(Tab, T, FieldsTable, Record2).


% select fields of object or list objects
% Ex.: ems_db:select_fields(#user{id = 1, name = "agilar", email = "evertonagilar@gmail.com"}, [name]).
% Sample result is [#{<<"name">> => "agilar"}]
-spec select_fields(list(tuple()) | tuple(), list()) -> {ok, list(map())}.
select_fields(ListRecord, []) -> {ok, ListRecord};
select_fields(Tuple, FieldList) when is_tuple(Tuple) -> 
    FieldList2 = ems_util:list_to_binlist(FieldList),
	List = ems_schema:to_list([Tuple], FieldList2),
	{ok, [Map]} = select_fields_agregate(List, []),
	{ok, Map};
select_fields(ListRecord, FieldList) -> 
    FieldList2 = ems_util:list_to_binlist(FieldList),
	List = ems_schema:to_list(ListRecord, FieldList2),
	select_fields_agregate(List, []).
	
select_fields_agregate([], Result) -> {ok, lists:reverse(Result)};
select_fields_agregate([H|T], Result) -> 
	select_fields_agregate(T, [maps:from_list(H)|Result]).
	

%	
% Return true/false if field has index on mnesia table
% Ex.: field_has_index(4, user). 
% return true
-spec field_has_index(non_neg_integer(), atom()) -> boolean().
field_has_index(FldPos, Tab) ->
	Indexes =  mnesia:table_info(Tab, index),
	lists:member(FldPos, Indexes).


% Return the field position on record
field_position(_, [], _) -> erlang:error(einvalid_field_filter);
field_position(Field, Fields, Idx) when is_list(Field) -> 
	field_position(list_to_atom(Field), Fields, Idx);
field_position(Field, Fields, Idx) when is_binary(Field) -> 
	field_position(binary_to_atom(Field, utf8), Fields, Idx);
field_position(Field, [F|Fs], Idx) ->
	case Field == F of
		true -> Idx;
		_ -> field_position(Field, Fs, Idx+1)
	end.

% Return the field as binary
field_value(V) when is_list(V) -> list_to_binary(V);
field_value(V) -> V.


-spec parse_datasource_type(binary()) -> atom().
parse_datasource_type(<<"mnesia">>) -> mnesia;
parse_datasource_type(<<"sqlserver">>) -> sqlserver;
parse_datasource_type(<<"csvfile">>) -> csvfile;
parse_datasource_type(_) -> erlang:error(einvalid_datasource_type_property).

-spec parse_data_source_driver(atom(), binary()) -> atom().
parse_data_source_driver(csvfile, <<"sqlite3">>) -> sqlite3;
parse_data_source_driver(csvfile, <<"odbc">>) -> odbc;
parse_data_source_driver(csvfile, _) -> erlang:error(einvalid_datasource_driver_property);
parse_data_source_driver(_, _) -> undefined.

-spec parse_datasource_csvdelimiter(atom(), binary()) -> string().
parse_datasource_csvdelimiter(csvfile, <<";">>) -> ";";
parse_datasource_csvdelimiter(csvfile, <<"|">>) -> "|";
parse_datasource_csvdelimiter(csvfile, <<",">>) -> ",";
parse_datasource_csvdelimiter(csvfile, <<"@">>) -> "@";
parse_datasource_csvdelimiter(csvfile, undefined) ->  ";";
parse_datasource_csvdelimiter(csvfile, <<>>) ->  ";";
parse_datasource_csvdelimiter(csvfile, _) -> erlang:error(einvalid_datasource_csvdelimiter_property);
parse_datasource_csvdelimiter(_, _) -> undefined.

-spec parse_datasource_table_name(atom(), binary() | list()) -> string() | list(atom()) | atom().
parse_datasource_table_name(_, undefined) -> undefined;
parse_datasource_table_name(_, <<>>) -> undefined;
parse_datasource_table_name(mnesia, Value) -> ems_util:binlist_to_atomlist(Value);
parse_datasource_table_name(_, Value) -> binary_to_list(Value).

-spec parse_datasource_fields(atom(), binary() | list()) -> string() | list(atom()) | atom().
parse_datasource_fields(_, undefined) -> [];
parse_datasource_fields(_, <<>>) -> [];
parse_datasource_fields(mnesia, Value) -> ems_util:binlist_to_atomlist(Value);
parse_datasource_fields(_, Value) -> binary_to_list(Value).


-spec parse_remap_fields_reverso(map()) -> map().
parse_remap_fields_reverso(undefined) -> undefined;
parse_remap_fields_reverso(RemapFields) ->
	RemapFieldsList = maps:to_list(RemapFields),
	RemapFieldsRev = [{Value, Key} || {Key,Value} <- RemapFieldsList],
	maps:from_list(RemapFieldsRev).


-spec parse_datasource_remap_fields(list(map())) -> map().
parse_datasource_remap_fields(undefined) -> undefined;
parse_datasource_remap_fields([]) -> undefined;
parse_datasource_remap_fields([MapH|MapT] = ListMap) when is_list(ListMap) -> 
	parse_datasource_remap_fields(MapT, MapH);
parse_datasource_remap_fields(_) -> 
	erlang:error(einvalid_datasource_remap_fields_property).

parse_datasource_remap_fields([], Result) -> Result;
parse_datasource_remap_fields([MapH|MapT], Result) -> 
	parse_datasource_remap_fields(MapT, maps:merge(MapH, Result)).


-spec parse_datasource_primary_key(atom(), binary()) -> string() | atom().
parse_datasource_primary_key(_, undefined) -> undefined;
parse_datasource_primary_key(_, <<>>) -> undefined;
parse_datasource_primary_key(mnesia, Value) -> binary_to_atom(Value, utf8);
parse_datasource_primary_key(_, Value) -> binary_to_list(Value).

-spec parse_datasource_foreign_key(atom(), binary()) -> string() | atom().
parse_datasource_foreign_key(_, undefined) -> undefined;
parse_datasource_foreign_key(_, <<>>) -> undefined;
parse_datasource_foreign_key(mnesia, Value) -> binary_to_atom(Value, utf8);
parse_datasource_foreign_key(_, Value) -> binary_to_list(Value).

-spec parse_datasource_foreign_table_name(atom(), binary() | list()) -> string() | list(atom()) | atom().
parse_datasource_foreign_table_name(_, undefined) -> undefined;
parse_datasource_foreign_table_name(mnesia, Value) -> ems_util:binlist_to_atomlist(Value);
parse_datasource_foreign_table_name(_, Value) -> ems_util:binlist_to_list(Value).

-spec parse_datasource_sql(atom(), binary()) -> string().
parse_datasource_sql(_, undefined) -> undefined;
parse_datasource_sql(_, <<>>) -> undefined;
parse_datasource_sql(mnesia, _) -> erlang:error(einvalid_datasource_sql_property);
parse_datasource_sql(_, Value) -> binary_to_list(Value).

-spec parse_datasource_connection(atom(), binary()) -> string().
parse_datasource_connection(mnesia, undefined) -> undefined;
parse_datasource_connection(mnesia, <<>>) -> undefined;
parse_datasource_connection(mnesia, _) -> erlang:error(einvalid_datasource_connection_property);
parse_datasource_connection(_, undefined) -> erlang:error(einvalid_datasource_connection_property);
parse_datasource_connection(_, <<>>) -> erlang:error(einvalid_datasource_connection_property);
parse_datasource_connection(_, Value) -> binary_to_list(Value).

-spec parse_datasource_sql_check_validation_connection(atom(), binary()) -> string().
parse_datasource_sql_check_validation_connection(sqlserver, undefined) -> "select 1";
parse_datasource_sql_check_validation_connection(sqlserver, Value) -> binary_to_list(Value);
parse_datasource_sql_check_validation_connection(_, undefined) -> undefined;
parse_datasource_sql_check_validation_connection(_, <<>>) -> undefined;
parse_datasource_sql_check_validation_connection(_, _) -> erlang:error(einvalid_datasource_sql_check_validation_connection_property).

-spec create_datasource_from_map(map()) -> #service_datasource{} | undefined.
create_datasource_from_map(M) -> create_datasource_from_map(M, undefined).

-spec create_datasource_from_map(map(), non_neg_integer()) -> #service_datasource{} | undefined.
create_datasource_from_map(M, Rowid) ->
	try
		Type = parse_datasource_type(maps:get(<<"type">>, M, undefined)),
		Driver = parse_data_source_driver(Type, maps:get(<<"driver">>, M, undefined)),
		Connection = parse_datasource_connection(Type, maps:get(<<"connection">>, M, undefined)),
		TableName = parse_datasource_table_name(Type, maps:get(<<"table_name">>, M, undefined)),
		Fields = parse_datasource_fields(Type, maps:get(<<"fields">>, M, undefined)),
		RemapFields = parse_datasource_remap_fields(maps:get(<<"remap_fields">>, M, undefined)),
		RemapFieldsRev = parse_remap_fields_reverso(RemapFields),
		PrimaryKey = parse_datasource_primary_key(Type, maps:get(<<"primary_key">>, M, undefined)),
		ForeignKey = parse_datasource_foreign_key(Type, maps:get(<<"foreign_key">>, M, undefined)),
		ForeignTableName = parse_datasource_foreign_table_name(Type, maps:get(<<"foreign_table_name">>, M, undefined)),
		CsvDelimiter = parse_datasource_csvdelimiter(Type, maps:get(<<"csv_delimiter">>, M, undefined)),
		ShowRemapFields = ems_util:parse_bool(maps:get(<<"show_remap_fields">>, M, true)),
		Sql = parse_datasource_sql(Type, maps:get(<<"sql">>, M, undefined)),
		Timeout0 = ems_util:parse_range(maps:get(<<"timeout">>, M, ?MAX_TIME_ODBC_QUERY), 1, ?MAX_TIME_ODBC_QUERY),
		case Timeout0 < 360000 of
			true -> Timeout = 360000;
			false -> Timeout = Timeout0
		end,
		MaxPoolSize = ems_util:parse_range(maps:get(<<"max_pool_size">>, M, ?MAX_CONNECTION_BY_POOL), 1, ?MAX_CONNECTION_BY_POOL),
		SqlCheckValidConnection = parse_datasource_sql_check_validation_connection(Type, maps:get(<<"sql_check_valid_connection">>, M, undefined)),
		CloseIdleConnectionTimeout = ems_util:parse_range(maps:get(<<"close_idle_connection_timeout">>, M, ?CLOSE_IDLE_CONNECTION_TIMEOUT), 1, ?MAX_CLOSE_IDLE_CONNECTION_TIMEOUT),
		CheckValidConnectionTimeout = ems_util:parse_range(maps:get(<<"check_valid_connection_timeout">>, M, ?CHECK_VALID_CONNECTION_TIMEOUT), 1, ?MAX_CLOSE_IDLE_CONNECTION_TIMEOUT),
		CtrlHash = erlang:phash2([Type, Driver, Connection, TableName, Fields, 
								  PrimaryKey, ForeignKey, ForeignTableName, CsvDelimiter, 
								  Sql, Timeout, MaxPoolSize, 
								  SqlCheckValidConnection, CloseIdleConnectionTimeout, 
								  CheckValidConnectionTimeout, RemapFields, ShowRemapFields]),
		case ems_db:find_first(service_datasource, [{ctrl_hash, "==", CtrlHash}]) of
			  {error, enoent} ->										
					Id = ems_db:inc_counter(service_datasource),
					IdStr = integer_to_list(Id),
					ConnectionCountMetricName = list_to_atom(lists:concat(["ems_odbc_pool_", IdStr, "_conn_count"])),
					ConnectionCreatedMetricName = list_to_atom(lists:concat(["ems_odbc_pool_", IdStr, "_created_count"])),
					ConnectionClosedMetricName = list_to_atom(lists:concat(["ems_odbc_pool_", IdStr, "_closed_count"])),
					ConnectionShutdownMetricName = list_to_atom(lists:concat(["ems_odbc_pool_", IdStr, "_shutdown_count"])),
					ConnectionReuseMetricName = list_to_atom(lists:concat(["ems_odbc_pool_", IdStr, "_reuse_count"])),
					ConnectionUnavailableMetricName = list_to_atom(lists:concat(["ems_odbc_pool_", IdStr, "_unavailable_count"])),
					ConnectionMaxPoolSizeExceededMetricName = list_to_atom(lists:concat(["ems_odbc_pool_", IdStr, "_max_pool_size_exceeded_count"])),
					NewDs = #service_datasource{id = Id,
												rowid = Rowid,
												type = Type,
												driver = Driver,
												connection = Connection,
												table_name = TableName,
												fields = Fields,
												primary_key = PrimaryKey,
												foreign_key = ForeignKey,
												foreign_table_name = ForeignTableName,
												csv_delimiter = CsvDelimiter,
												sql = Sql,
												timeout = Timeout,
												max_pool_size = MaxPoolSize,
												remap_fields = RemapFields,
												remap_fields_rev = RemapFieldsRev,
												show_remap_fields = ShowRemapFields,
												connection_count_metric_name = ConnectionCountMetricName,
												connection_created_metric_name = ConnectionCreatedMetricName,
												connection_closed_metric_name = ConnectionClosedMetricName,
												connection_shutdown_metric_name = ConnectionShutdownMetricName,
												connection_reuse_metric_name = ConnectionReuseMetricName,
												connection_unavailable_metric_name = ConnectionUnavailableMetricName,
												connection_max_pool_size_exceeded_metric_name = ConnectionMaxPoolSizeExceededMetricName,
												sql_check_valid_connection = SqlCheckValidConnection,
												check_valid_connection_timeout = CheckValidConnectionTimeout,
												close_idle_connection_timeout = CloseIdleConnectionTimeout,
												ctrl_hash = CtrlHash
											},
					ems_db:insert(NewDs),
					NewDs;
			  {ok, ExistDs} -> ExistDs
		 end										
	catch
		_:Reason-> 
			ems_logger:format_error("ems_db parse invalid datasource ~p. Reason: ~p.\n", [M, Reason]),
			undefined
	end.
	

