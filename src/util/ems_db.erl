	%%********************************************************************
%% @title Module ems_db
%% @version 1.0.0
%% @doc Module that provides interface with the database.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_db).

-export([start/0]).
-export([get/2, all/1, insert/1, update/1, delete/2, existe/1, match_object/1, 
		 match/2, find/2, find/3, find/5, find_by_id/3, filter/2, 
		 filter_with_limit/4, select_fields/2, 
		 find_first/2, find_first/3, find_first/4]).
-export([init_sequence/2, sequence/1, sequence/2, current_sequence/1]).
-export([init_counter/2, counter/2, current_counter/1, inc_counter/1, dec_counter/1]).
-export([get_connection/1, release_connection/1, get_sqlite_connection_from_csv_file/1, create_datasource_from_map/1, create_datasource_from_map/2]).
-export([get_param/1, set_param/2]).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").



%% *********** Database schema creation ************

start() ->
	create_database([node()]),
	ems_cache:new(ems_db_odbc_connection_cache),
	ems_cache:new(ems_db_parsed_query_cache).
	
create_database(Nodes) ->
	% Define a pasta de armazenamento dos databases
	filelib:ensure_dir(?DATABASE_PATH ++ "/"),
	application:set_env(mnesia, dir, ?DATABASE_PATH),

	mnesia:create_schema(Nodes),
	mnesia:start(),

    mnesia:create_table(user, [{type, set},
							   {disc_copies, Nodes},
							   {index, [#user.login, #user.cpf, #user.email, #user.user_id]},
							   {attributes, record_info(fields, user)}]),

	mnesia:create_table(user_permission, [{type, set},
										   {disc_copies, Nodes},
										   {index, [#user_permission.hash, #user_permission.hash2]},
										   {attributes, record_info(fields, user_permission)}]),


    mnesia:create_table(client, [{type, set},
							     {disc_copies, Nodes},
							     {index, [#client.codigo]},
							     {attributes, record_info(fields, client)}]),
							     
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
									  


	ok.



%% *********** Functions for CRUD ************

get(RecordType, Id) when is_list(Id) ->
	Id2 = list_to_integer(Id),
	get(RecordType, Id2);
get(RecordType, Id) when is_number(Id) ->
	case mnesia:dirty_read(RecordType, Id) of
		[] -> {error, enoent};
		[Record|_] -> {ok, Record}
	end;
get(_RecordType, _) -> {error, enoent}.


all(RecordType) ->
	Query = fun() ->
		  qlc:e(
			 qlc:q([X || X <- mnesia:table(RecordType)])
		  )
	   end,
	Records = mnesia:activity(async_dirty, Query),
	{ok, Records}.

insert(Record) ->
	RecordType = element(1, Record),
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
	{atomic, Result} = mnesia:transaction(F),
	{ok, Result}.

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

%% @doc Verifica se um registro existe
match_object(Pattern) ->	
	{atomic, Records} = mnesia:transaction(fun() -> 
												mnesia:match_object(Pattern) 
										   end),
	Records.


    


%% @doc Verifica se um registro existe
existe(Pattern) ->	
	case match_object(Pattern) of
		[] -> false;
		_ -> true
	end.

	

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


create_sqlite_from_csv(#service_datasource{connection = FileName,
										   table_name = TableName,
										   csv_delimiter = Delimiter}) -> 
	FileNamePath = ?CSV_FILE_PATH ++ "/" ++ FileName,
	case filelib:last_modified(FileNamePath) of
		0 -> {error, ecsvfile_not_exist};
		LastModified ->
			SqliteFile = ?DATABASE_PATH ++ "/sqlite3_" ++ TableName,
			DatabaseExist = filelib:is_file(SqliteFile), 
			F = fun() ->
				Ctrl = ems_util:hd_or_empty(mnesia:read(ctrl_sqlite_table, FileName)),
				case Ctrl =:= [] orelse not DatabaseExist orelse Ctrl#ctrl_sqlite_table.last_modified =/= LastModified of
					true ->
						Csv2SqliteCmd = lists:flatten(io_lib:format('~s "~s" "~s" "~s" "~s"',
																	 [?CSV2SQLITE_PATH,
																	  SqliteFile, 
																	  TableName, 
																	  FileNamePath, 
																	  Delimiter])),
						ems_logger:info("OS command: ~p.", [Csv2SqliteCmd]),
						os:cmd(Csv2SqliteCmd),
						mnesia:write(#ctrl_sqlite_table{file_name = FileName, last_modified = LastModified});
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


%create_sqlite_virtual_table_from_csv_file(FileName, TableName, _PrimaryKey) -> 
%	{ok, Conn} = ems_db:get_odbc_connection("DRIVER=SQLite;Version=3;New=True;"),
%	odbc:sql_query(Conn, "select load_extension(\"/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtable.so\")"),
%	CreateTableDDL = lists:flatten(io_lib:format("create virtual table ~s using csvtable(\"~s\")", [TableName, FileName])),
%	odbc:sql_query(Conn, CreateTableDDL),
%	odbc:commit(Conn, commit),
%	{ok, Conn}.


%
% Find object by id
% Ex.: ems_db:find_by_id(catalog_schema, 1, [id, name]).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
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
find(Tab, FilterList) -> find(Tab, [], FilterList).

%
% Find objects
% Ex.: ems_db:find(catalog_schema, [id, name], [{id, "==", 1}]).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
find(Tab, FieldList, FilterList) ->
    Records = filter(Tab, FilterList),
	select_fields(Records, FieldList).


%
% Find objects with limits
% Ex.: ems_db:find(catalog_schema, [id, name], [{id, "==", 1}], 1, 1).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
find(Tab, FieldList, FilterList, Limit, Offset) ->
    Records = filter_with_limit(Tab, FilterList, Limit, Offset),
	select_fields(Records, FieldList).


%
% Find first objects
% Ex.: ems_db:find_first(catalog_schema, [{id, "==", 1}]).
% Sample result is [{<<"id">>,1},{<<"name">>,<<"exemplo">>}]
%
find_first(Tab, FilterList) -> find_first(Tab, [], FilterList).


%
% Find first objects
% Ex.: ems_db:find_first(catalog_schema, [id, name], [{id, "==", 1}]).
% Sample result is [{<<"id">>,1},{<<"name">>,<<"exemplo">>}]
%
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
filter(Tab, []) -> 
	F = fun() ->
		  qlc:e(
			 qlc:q([R || R <- mnesia:table(Tab)])
		  )
	   end,
	mnesia:activity(async_dirty, F);
filter(Tab, [{F1, "==", V1}]) ->
	Fields =  mnesia:table_info(Tab, attributes),
	Fld1 = field_position(F1, Fields, 2),
	case field_has_index(Fld1, Tab) of
		false ->
			Fun = fun() -> 
						qlc:e(qlc:q([R || R <- mnesia:table(Tab), element(Fld1, R) == field_value(V1)])) 
				  end,
			mnesia:activity(async_dirty, Fun);
		true ->
			mnesia:dirty_index_read(Tab, field_value(V1), Fld1)
	end;
filter(Tab, FilterList) when is_list(FilterList) -> 
	F = fun() ->
			FieldsTable =  mnesia:table_info(Tab, attributes),
			Where = string:join(lists:map(fun({F, Op, V}) ->
												Fld = field_position(F, FieldsTable, 2),
												io_lib:format("element(~s, R) ~s ~p", [integer_to_list(Fld), Op,  field_value(V)])
										  end, FilterList), ","),
			ExprQuery = lists:flatten(io_lib:format("[R || R <- mnesia:table(~p), ~s].", [Tab, Where])),
			ParsedQuery = qlc:string_to_handle(ExprQuery),
			mnesia:activity(async_dirty, fun () -> qlc:eval(ParsedQuery) end)
		end,
	ems_cache:get(ems_db_parsed_query_cache, ?LIFE_TIME_PARSED_QUERY, {Tab, FilterList}, F);
filter(Tab, FilterTuple) when is_tuple(FilterTuple) ->
	filter(Tab, [FilterTuple]).


%	
% Return true/false if field has index on mnesia table
% Ex.: field_has_index(4, user). 
% return true
field_has_index(FldPos, Tab) ->
	Indexes =  mnesia:table_info(Tab, index),
	lists:member(FldPos, Indexes).
	


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
filter_with_limit(Tab, [], Limit, Offset) -> 
	F = fun() ->
		  Q = qlc:q([R || R <- mnesia:table(Tab), element(2, R) >= Offset, element(2, R) =< Limit + Offset - 1]),
		  qlc:info(Q, [{n_elements, Limit}]),
		  qlc:e(Q)
	   end,
	mnesia:activity(async_dirty, F);
filter_with_limit(Tab, Filter = [{_, "==", _}], Limit, Offset) ->
	Records = filter(Tab, Filter),
	lists:sublist(Records, Offset, Limit); 
filter_with_limit(Tab, FilterList, Limit, Offset) when is_list(FilterList) -> 
	F = fun() ->
			FieldsTable =  mnesia:table_info(Tab, attributes),
			Where = string:join(lists:map(fun({F, Op, V}) ->
												Fld = field_position(F, FieldsTable, 2),
												io_lib:format("element(~s, R) ~s ~p", [integer_to_list(Fld), Op,  field_value(V)])
										  end, FilterList), ","),
			ExprQuery = lists:flatten(io_lib:format("[R || R <- mnesia:table(~p), ~s].", [Tab, Where])),
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
% Sample result is [{<<"name">>,"agilar"}]
select_fields(ListRecord, []) -> ListRecord;
select_fields(ListRecord, FieldList) -> 
    FieldList2 = ems_util:list_to_binlist(FieldList),
	ems_schema:to_list(ListRecord, FieldList2).


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
	

-spec create_datasource_from_map(map()) -> #service_datasource{}.
create_datasource_from_map(M) ->
	create_datasource_from_map(M, undefined).

-spec create_datasource_from_map(map(), non_neg_integer()) -> #service_datasource{}.
create_datasource_from_map(M, Rowid) ->
	Type = list_to_atom(binary_to_list(maps:get(<<"type">>, M))),
	Driver = maps:get(<<"driver">>, M, <<>>),
	Connection = binary_to_list(maps:get(<<"connection">>, M, <<>>)),
	TableName = binary_to_list(maps:get(<<"table_name">>, M, <<>>)),
	PrimaryKey = binary_to_list(maps:get(<<"primary_key">>, M, <<>>)),
	CsvDelimiter = binary_to_list(maps:get(<<"csv_delimiter">>, M, <<";">>)),
	Sql = binary_to_list(maps:get(<<"sql">>, M, <<>>)),
	Timeout = maps:get(<<"timeout">>, M, ?MAX_TIME_ODBC_QUERY),
	MaxPoolSize = maps:get(<<"max_pool_size">>, M, ?MAX_CONNECTION_BY_POOL),
	Id = erlang:phash2([Rowid, Type, Driver, Connection, TableName, PrimaryKey, CsvDelimiter, Sql, Timeout, MaxPoolSize]),
	#service_datasource{id = Id,
						rowid = Rowid,
						type = Type,
						driver = Driver,
						connection = Connection,
						table_name = TableName,
						primary_key = PrimaryKey,
						csv_delimiter = CsvDelimiter,
						sql = Sql,
						timeout = Timeout,
						max_pool_size = MaxPoolSize}.
	

