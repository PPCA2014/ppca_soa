	%%********************************************************************
%% @title Module ems_db
%% @version 1.0.0
%% @doc Module that provides interface with the database.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_db).

-export([start/0]).
-export([get/2, all/1, insert/1, insert/2, update/1, delete/2, 
		 match/2, find/2, find/3, find/5, find_by_id/2, find_by_id/3, filter/2, 
		 filter_with_limit/4, select_fields/2, 
		 find_first/2, find_first/3, find_first/4]).
-export([init_sequence/2, sequence/1, sequence/2, current_sequence/1]).
-export([init_counter/2, counter/2, current_counter/1, inc_counter/1, dec_counter/1]).
-export([get_connection/1, release_connection/1, get_sqlite_connection_from_csv_file/1, create_datasource_from_map/1, create_datasource_from_map/2]).
-export([get_param/1, set_param/2]).

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
    mnesia:create_table(user, [{type, set},
							   {disc_copies, Nodes},
							   {index, [#user.codigo, #user.login, #user.cpf, #user.email]},
							   {attributes, record_info(fields, user)}]),

    mnesia:create_table(user_fs, [{type, set},
								  {disc_copies, Nodes},
								  {index, [#user.codigo, #user.codigo_pessoa, #user.login, #user.cpf, #user.email]},
								  {attributes, record_info(fields, user)},
								  {record_name, user}]),

    mnesia:create_table(user_db, [{type, set},
								  {disc_copies, Nodes},
								  {index, [#user.codigo, #user.codigo_pessoa, #user.login, #user.cpf, #user.email]},
								  {attributes, record_info(fields, user)},
								  {record_name, user}]),


    mnesia:create_table(user_dados_funcionais_fs, [{type, set},
								  {disc_copies, Nodes},
								  {index, [#user_dados_funcionais.codigo]},
								  {attributes, record_info(fields, user_dados_funcionais)},
								  {record_name, user_dados_funcionais}]),

    mnesia:create_table(user_dados_funcionais_db, [{type, set},
								  {disc_copies, Nodes},
								  {index, [#user_dados_funcionais.codigo]},
								  {attributes, record_info(fields, user_dados_funcionais)},
								  {record_name, user_dados_funcionais}]),

    mnesia:create_table(user_email_fs, [{type, set},
								  {disc_copies, Nodes},
								  {index, [#user_email.codigo]},
								  {attributes, record_info(fields, user_email)},
								  {record_name, user_email}]),

    mnesia:create_table(user_email_db, [{type, set},
								  {disc_copies, Nodes},
								  {index, [#user_email.codigo]},
								  {attributes, record_info(fields, user_email)},
								  {record_name, user_email}]),

	mnesia:create_table(user_permission, [{type, set},
										   {disc_copies, Nodes},
										   {index, [#user_permission.hash, #user_permission.hash2]},
										   {attributes, record_info(fields, user_permission)}]),


	mnesia:create_table(user_perfil_fs, [{type, set},
										{disc_copies, Nodes},
										{index, [#user_perfil.codigo, #user_perfil.user_id, #user_perfil.client_id, #user_perfil.codigo_usuario, #user_perfil.codigo_cliente]},
										{attributes, record_info(fields, user_perfil)},
										{record_name, user_perfil}]),

	mnesia:create_table(user_perfil_db, [{type, set},
										{disc_copies, Nodes},
										{index, [#user_perfil.codigo, #user_perfil.user_id, #user_perfil.client_id, #user_perfil.codigo_usuario, #user_perfil.codigo_cliente]},
									    {attributes, record_info(fields, user_perfil)},
									    {record_name, user_perfil}]),

	mnesia:create_table(user_permission_fs, [{type, set},
										{disc_copies, Nodes},
										{index, [#user_permission.codigo, #user_permission.user_id, #user_permission.client_id, #user_permission.codigo_usuario, #user_permission.codigo_cliente]},
										{attributes, record_info(fields, user_permission)},
										{record_name, user_permission}]),

	mnesia:create_table(user_permission_db, [{type, set},
										{disc_copies, Nodes},
										{index, [#user_permission.codigo, #user_permission.user_id, #user_permission.client_id, #user_permission.codigo_usuario, #user_permission.codigo_cliente]},
									    {attributes, record_info(fields, user_permission)},
									    {record_name, user_permission}]),

    mnesia:create_table(client, [{type, set},
							     {disc_copies, Nodes},
							     {index, [#client.codigo]},
							     {attributes, record_info(fields, client)}]),
							     
    mnesia:create_table(client_db, [{type, set},
									{disc_copies, Nodes},
									{index, [#client.codigo]},
									{attributes, record_info(fields, client)},
									{record_name, client}]),

    mnesia:create_table(client_fs, [{type, set},
									{disc_copies, Nodes},
									{index, [#client.codigo]},
									{attributes, record_info(fields, client)},
									{record_name, client}]),

    mnesia:create_table(sequence, [{type, set},
								   {disc_copies, Nodes},
								   {attributes, record_info(fields, sequence)}]),

    mnesia:create_table(counter, [{type, set},
								  {ram_copies, Nodes},
								  {attributes, record_info(fields, sequence)}]),

    mnesia:create_table(request, [{type, set},
								  {ram_copies, Nodes},
								  {attributes, record_info(fields, request)},
								  {index, [#request.timestamp]}]),

    mnesia:create_table(service_datasource, [{type, set},
											 {ram_copies, Nodes},
											 {attributes, record_info(fields, service_datasource)}]),

    mnesia:create_table(ctrl_sqlite_table, [{type, set},
											{disc_copies, Nodes},
											{attributes, record_info(fields, ctrl_sqlite_table)}]),

    mnesia:create_table(catalog_schema, [{type, set},
										 {disc_copies, Nodes},
										 {attributes, record_info(fields, catalog_schema)}]),

    mnesia:create_table(produto, [{type, set},
	 							  {disc_copies, Nodes},
								  {attributes, record_info(fields, produto)}]),

    mnesia:create_table(service, [{type, set},
	 							  {disc_copies, Nodes},
								  {attributes, record_info(fields, service)}]),


    mnesia:create_table(service_owner, [{type, set},
	 							  {disc_copies, Nodes},
								  {attributes, record_info(fields, service_owner)}]),

    mnesia:create_table(ctrl_params, [{type, set},
									  {disc_copies, Nodes},
									  {attributes, record_info(fields, ctrl_params)}]),
									  

    mnesia:create_table(catalog_get_fs, [{type, set},
									  {disc_copies, Nodes},
									  {index, [#service.rowid]},
									  {attributes, record_info(fields, service)},
									  {record_name, service}]),

    mnesia:create_table(catalog_post_fs, [{type, set},
									  {disc_copies, Nodes},
									  {index, [#service.rowid]},
									  {attributes, record_info(fields, service)},
									  {record_name, service}]),

    mnesia:create_table(catalog_put_fs, [{type, set},
									  {disc_copies, Nodes},
									  {index, [#service.rowid]},
									  {attributes, record_info(fields, service)},
									  {record_name, service}]),

    mnesia:create_table(catalog_delete_fs, [{type, set},
									  {disc_copies, Nodes},
									  {index, [#service.rowid]},
									  {attributes, record_info(fields, service)},
									  {record_name, service}]),

    mnesia:create_table(catalog_options_fs, [{type, set},
										  {disc_copies, Nodes},
										  {index, [#service.rowid]},
										  {attributes, record_info(fields, service)},
										  {record_name, service}]),

    mnesia:create_table(catalog_kernel_fs, [{type, set},
										  {disc_copies, Nodes},
										  {index, [#service.rowid]},
										  {attributes, record_info(fields, service)},
										  {record_name, service}]),

    mnesia:create_table(catalog_re_fs, [{type, set},
										  {disc_copies, Nodes},
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

	% foi preciso aguardar um pouco a inicialização do banco
	ems_util:sleep(1000),

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
						ems_logger:info("OS command: ~p.", [Csv2SqliteCmd]),
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
		<<"odbc">> ->
			StringConnection = lists:flatten(io_lib:format("DRIVER=SQLite;Version=3;Database=~s;", [SqliteFile])),
			ems_odbc_pool:get_connection(Datasource#service_datasource{type = sqlite, connection = StringConnection});
		<<"sqlite3">> ->
			ems_odbc_pool:get_connection(Datasource#service_datasource{type = sqlite, connection = SqliteFile});
		_ -> throw({error, einvalid_driver_datasource})
	end.


%create_sqlite_virtual_table_from_csv_file(Filename, TableName, _PrimaryKey) -> 
%	{ok, Conn} = ems_db:get_odbc_connection("DRIVER=SQLite;Version=3;New=True;"),
%	odbc:sql_query(Conn, "select load_extension(\"/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtable.so\")"),
%	CreateTableDDL = lists:flatten(io_lib:format("create virtual table ~s using csvtable(\"~s\")", [TableName, Filename])),
%	odbc:sql_query(Conn, CreateTableDDL),
%	odbc:commit(Conn, commit),
%	{ok, Conn}.


%% ************* Funções para pesquisa *************

-spec get(atom(), non_neg_integer()) -> {ok, tuple()} | {error, enoent}.
get(Tab, Id) when is_number(Id) ->
	case mnesia:dirty_read(Tab, Id) of
		[] -> {error, enoent};
		[Record|_] -> {ok, Record}
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
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
-spec find_by_id(atom(), non_neg_integer()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find_by_id(Tab, Id) ->
	case get(Tab, Id) of
		{ok, Record} -> {ok, Record};
		Error -> Error
	end.

%
% Find object by id
% Ex.: ems_db:find_by_id(catalog_schema, 1, [id, name]).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
-spec find_by_id(atom(), non_neg_integer(), list()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find_by_id(Tab, Id, FieldList) ->
	case get(Tab, Id) of
		{ok, Record} -> select_fields(Record, FieldList);
		Error -> Error
	end.


%
% Find objects
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
-spec find(atom(), list(), list()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find(Tab, FieldList, FilterList) ->
    Records = filter(Tab, FilterList),
	select_fields(Records, FieldList).


%
% Find objects with limits
% Ex.: ems_db:find(catalog_schema, [id, name], [{id, "==", 1}], 1, 1).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
-spec find(atom(), list(), list(), non_neg_integer(), non_neg_integer()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find(Tab, FieldList, FilterList, Limit, Offset) -> 
    Records = filter_with_limit(Tab, FilterList, Limit, Offset),
	select_fields(Records, FieldList).


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
-spec find_first(atom(), list(), list()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find_first(Tab, FieldList, FilterList) ->
    case filter_with_limit(Tab, FilterList, 1, 1) of
		[] -> {error, enoent};
		[FirstRecord|_] -> select_fields(FirstRecord, FieldList)
	end.


%
% Find first objects
% Ex.: ems_db:find_first(catalog_schema, [id, name], [{id, "==", 1}], 1).
% Sample result is [{<<"id">>,1},{<<"name">>,<<"exemplo">>}]
%
-spec find_first(atom(), list(), list(), non_neg_integer()) -> {ok, tuple()} | {ok, map()} | {error, enoent}.
find_first(Tab, FieldList, FilterList, Offset) ->
    case filter_with_limit(Tab, FilterList, 1, Offset) of
		[] -> {error, enoent};
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
	mnesia:activity(async_dirty, F);
filter_with_limit(Tab, Filter = [{_, "==", _}], Limit, Offset) ->
	Records = filter(Tab, Filter),
	lists:sublist(Records, Offset, Limit); 
filter_with_limit(Tab, FilterList, Limit, Offset) when is_list(FilterList) -> 
	F = fun() ->
			FieldsTable =  mnesia:table_info(Tab, attributes),
			Where = string:join([io_lib:format("element(~s, R) ~s ~p", [integer_to_list(field_position(F, FieldsTable, 2)), Op,  field_value(V)]) || {F, Op, V} <- FilterList], ","),
			ExprQuery = binary_to_list(iolist_to_binary([<<"[R || R <- mnesia:table(">>, atom_to_binary(Tab, utf8), <<"), ">>, Where, <<"].">>])),
			ParsedQuery = qlc:string_to_handle(ExprQuery),
			mnesia:activity(async_dirty, fun () -> 
											Records = qlc:eval(ParsedQuery),
											lists:sublist(Records, Offset, Limit) 
										 end)
		end,
	ems_cache:get(ems_db_parsed_query_cache, ?LIFE_TIME_PARSED_QUERY, {Tab, FilterList, Limit, Offset}, F);
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
field_position(Field, [F|Fs], Idx) ->
	case Field == F of
		true -> Idx;
		_ -> field_position(Field, Fs, Idx+1)
	end.

% Return the field as binary
field_value(V) when is_list(V) -> list_to_binary(V);
field_value(V) -> V.

% Return a param value from crtl_params table
get_param(ParamName) -> 
	case mnesia:dirty_read(ctrl_params, ParamName) of
		[] -> undefined;
		[#ctrl_params{value = Value}] -> Value
	end.
	
% Save a param value to crtl_params table
set_param(ParamName, ParamValue) -> 
	P = #ctrl_params{name = ParamName, value = ParamValue},
	mnesia:dirty_write(ctrl_params, P).
	

-spec parse_data_source_driver(binary()) -> binary().
parse_data_source_driver(<<>>) -> <<>>;
parse_data_source_driver(undefined) -> <<>>;
parse_data_source_driver(<<"sqlite3">>) -> <<"sqlite3">>;
parse_data_source_driver(<<"odbc">>) -> <<"odbc">>;
parse_data_source_driver(_) -> erlang:error(einvalid_datasource_driver).


-spec parse_datasource_csvdelimiter(string()) -> ok.
parse_datasource_csvdelimiter(";") -> ";";
parse_datasource_csvdelimiter("|") -> "|";
parse_datasource_csvdelimiter(",") -> ",";
parse_datasource_csvdelimiter("@") -> "@";
parse_datasource_csvdelimiter(_) -> erlang:error(einvalid_datasource_csvdelimiter).


-spec create_datasource_from_map(map()) -> #service_datasource{} | undefined.
create_datasource_from_map(M) -> create_datasource_from_map(M, undefined).

-spec create_datasource_from_map(map(), non_neg_integer()) -> #service_datasource{} | undefined.
create_datasource_from_map(M, Rowid) ->
	try
		Type = erlang:binary_to_atom(maps:get(<<"type">>, M), utf8),
		Driver = parse_data_source_driver(maps:get(<<"driver">>, M, <<>>)),
		Connection = binary_to_list(maps:get(<<"connection">>, M, <<>>)),
		TableName = binary_to_list(maps:get(<<"table_name">>, M, <<>>)),
		TableName2 = binary_to_list(maps:get(<<"table_name2">>, M, <<>>)),
		PrimaryKey = binary_to_list(maps:get(<<"primary_key">>, M, <<>>)),
		CsvDelimiter = parse_datasource_csvdelimiter(binary_to_list(maps:get(<<"csv_delimiter">>, M, <<";">>))),
		Sql = binary_to_list(maps:get(<<"sql">>, M, <<>>)),
		Timeout = ems_util:parse_range(maps:get(<<"timeout">>, M, ?MAX_TIME_ODBC_QUERY), 1, ?MAX_TIME_ODBC_QUERY),
		MaxPoolSize = ems_util:parse_range(maps:get(<<"max_pool_size">>, M, ?MAX_CONNECTION_BY_POOL), 1, ?MAX_CONNECTION_BY_POOL),
		SqlCheckValidConnection = binary_to_list(maps:get(<<"sql_check_valid_connection">>, M, <<>>)),
		CloseIdleConnectionTimeout = ems_util:parse_range(maps:get(<<"close_idle_connection_timeout">>, M, ?CLOSE_IDLE_CONNECTION_TIMEOUT), 1, ?MAX_CLOSE_IDLE_CONNECTION_TIMEOUT),
		CheckValidConnectionTimeout = ems_util:parse_range(maps:get(<<"check_valid_connection_timeout">>, M, ?CHECK_VALID_CONNECTION_TIMEOUT), 1, ?MAX_CLOSE_IDLE_CONNECTION_TIMEOUT),
		CtrlHash = erlang:phash2([Type, Driver, Connection, TableName, TableName2, PrimaryKey, CsvDelimiter, Sql, Timeout, MaxPoolSize, SqlCheckValidConnection, CloseIdleConnectionTimeout, CheckValidConnectionTimeout]),
		case ems_db:find_first(service_datasource, [{ctrl_hash, "==", CtrlHash}]) of
			  {error, enoent} ->										
					Id = ems_db:inc_counter(service_datasource),
					IdStr = integer_to_list(Id),
					ConnectionCountMetricName = list_to_atom("ems_odbc_pool_" ++ IdStr ++ "_conn_count"),
					ConnectionCreatedMetricName = list_to_atom("ems_odbc_pool_" ++ IdStr ++ "_created_count"),
					ConnectionClosedMetricName = list_to_atom("ems_odbc_pool_" ++ IdStr ++ "_closed_count"),
					ConnectionShutdownMetricName = list_to_atom("ems_odbc_pool_" ++ IdStr ++ "_shutdown_count"),
					ConnectionReuseMetricName = list_to_atom("ems_odbc_pool_" ++ IdStr ++ "_reuse_count"),
					ConnectionUnavailableMetricName = list_to_atom("ems_odbc_pool_" ++ IdStr ++ "_unavailable_count"),
					ConnectionMaxPoolSizeExceededMetricName = list_to_atom("ems_odbc_pool_" ++ IdStr ++ "_max_pool_size_exceeded_count"),
					NewDs = #service_datasource{id = Id,
												rowid = Rowid,
												type = Type,
												driver = Driver,
												connection = Connection,
												table_name = TableName,
												table_name2 = TableName2,
												primary_key = PrimaryKey,
												csv_delimiter = CsvDelimiter,
												sql = Sql,
												timeout = Timeout,
												max_pool_size = MaxPoolSize,
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
	

