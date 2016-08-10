%%********************************************************************
%% @title Module ems_db
%% @version 1.0.0
%% @doc Module that provides interface with the database.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_db).

-export([start/0]).
-export([get/2, all/1, insert/1, update/1, delete/2, existe/1, match_object/1, match/2, find/3, filter/2, select_fields/2]).
-export([sequence/1, init_sequence/2]).
-export([get_odbc_connection/2, release_odbc_connection/3]).
-export([create_sqlite_virtual_table_from_csv_file/3, get_odbc_connection_csv_file/5]).


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
							   {attributes, record_info(fields, user)}]),

    mnesia:create_table(sequence, [{type, set},
								   {disc_copies, Nodes},
								   {attributes, record_info(fields, sequence)}]),

    mnesia:create_table(sequence_transient, [{type, set},
											 {ram_copies, Nodes},
											 {attributes, record_info(fields, sequence)}]),

    mnesia:create_table(request, [{type, set},
								  {ram_copies, Nodes},
								  {attributes, record_info(fields, request)},
								  {index, [#request.timestamp]}]),

    mnesia:create_table(ctrl_sqlite_table, [{type, set},
											{ram_copies, Nodes},
											{attributes, record_info(fields, ctrl_sqlite_table)}]),


    mnesia:create_table(catalog_schema, [{type, set},
										 {disc_copies, Nodes},
										 {attributes, record_info(fields, catalog_schema)}]),


	ok.



%% *********** Functions for CRUD ************

get(RecordType, Id) when is_list(Id) ->
	Id2 = list_to_integer(Id),
	get(RecordType, Id2);

get(RecordType, Id) when is_number(Id) ->
	Query = fun() ->
		mnesia:read(RecordType, Id)
	end,
	case mnesia:transaction(Query) of
		{atomic, []} -> {erro, notfound};
		{atomic, [Record|_]} -> {ok, Record};
		{aborted, _Reason} -> {erro, aborted}
	end;

get(_RecordType, _) -> {erro, notfound}.


all(RecordType) ->
	Query = fun() ->
		  qlc:e(
			 qlc:q([X || X <- mnesia:table(RecordType)])
		  )
	   end,
	{atomic, Records} = mnesia:transaction(Query),
	{ok, Records}.

insert(Record) ->
	RecordType = element(1, Record),
	case element(2, Record) of
		undefined -> Id = sequence(RecordType),
					 Record1 = setelement(2, Record, Id);
		_ -> Record1 = Record
	end,
	Write = fun() -> mnesia:write(Record1) end,
	mnesia:transaction(Write),
	{ok, Record1}.

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

%% Inicializa ou reseta uma sequence
init_sequence(Name, Value) ->
     {atomic, ok} =	mnesia:transaction(fun() ->
						mnesia:write(#sequence{key=Name, index=Value})
					end),
     ok.

%% Retorna o valor corrente para uma sequence. A sequence é criada se não existir.
sequence(Name) ->
     sequence(Name, 1).

%% Incrementa a sequence com Inc
sequence(Name, Inc) ->
     mnesia:dirty_update_counter(sequence, Name, Inc).


get_odbc_connection(Module, Datasource) ->
	F = fun() ->
		case odbc:connect(Datasource, [{scrollable_cursors, off},
									   {timeout, 3500},
									   {trace_driver, off}]) of
			{ok, Conn}	-> {ok, Conn};
			{error, Reason} -> {error, Reason}
		end
	end,
	ems_cache:get(ems_db_odbc_connection_cache, infinity, {Module, Datasource}, F).

release_odbc_connection(Module, Datasource, Conn) ->
	F = fun() -> odbc:disconnect(Conn) end,
	ems_cache:flush_future(ems_db_odbc_connection_cache, ?LIFE_TIME_ODBC_CONNECTION, {Module, Datasource}, F).

get_odbc_connection_csv_file(Module, FileName, TableName, _PrimaryKey, Delimiter) -> 
	FileNamePath = ?CSV_FILE_PATH ++ "/" ++ FileName,
	case filelib:last_modified(FileNamePath) of
		0 -> {error, ecsvfile_not_exist};
		LastModified ->
			DatabaseExist = filelib:is_file(?DATABASE_SQLITE_PATH), 
			F = fun() ->
				Ctrl = ems_util:hd_or_empty(mnesia:read(ctrl_sqlite_table, FileName)),
				case Ctrl =:= [] orelse not DatabaseExist orelse Ctrl#ctrl_sqlite_table.last_modified =/= LastModified of
					true ->
						Csv2SqliteCmd = lists:flatten(io_lib:format("~s '~s\' '~s' '~s' '~s'",
																	 [?CSV2SQLITE_PATH,
																	  ?DATABASE_SQLITE_PATH, 
																	  TableName, 
																	  FileNamePath, 
																	  Delimiter])),
						os:cmd(Csv2SqliteCmd),
						mnesia:write(#ctrl_sqlite_table{file_name = FileName, last_modified = LastModified});
					false -> 
						% não foi necessário fazer a carga dos dados do arquivo para o banco sqlite
						ok
				end
			end,
			mnesia:activity(transaction, F),
			get_odbc_connection(Module, ?DATABASE_SQLITE_STRING_CONNECTION)
	end.

	

create_sqlite_virtual_table_from_csv_file(FileName, TableName, _PrimaryKey) -> 
	{ok, Conn} = ems_db:get_odbc_connection("DRIVER=SQLite;Version=3;New=True;"),
	odbc:sql_query(Conn, "select load_extension(\"/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtable.so\")"),
	CreateTableDDL = lists:flatten(io_lib:format("create virtual table ~s using csvtable(\"~s\")", [TableName, FileName])),
	odbc:sql_query(Conn, CreateTableDDL),
	odbc:commit(Conn, commit),
	{ok, Conn}.



%
% Find objects
% Ex.: ems_db:find(catalog_schema, [id, name], [{id, "==", 1}]).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
find(Tab, FieldList, FilterList) ->
    Records = filter(Tab, FilterList),
    select_fields(Records, FieldList).

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
			 qlc:q([X || X <- mnesia:table(Tab)])
		  )
	   end,
	mnesia:activity(transaction, F);
filter(Tab, [{F1, "==", V1}]) ->
	Fields =  mnesia:table_info(catalog_schema, attributes),
	Fld1 = field_index(F1, Fields, 2),
	Fun = fun() -> 
				qlc:e(qlc:q([R || R <- mnesia:table(Tab), element(Fld1, R) == field_value(V1)])) 
		  end,
	mnesia:activity(transaction, Fun);
filter(Tab, FilterList) -> 
	F = fun() ->
			FieldsTable =  mnesia:table_info(Tab, attributes),
			Where = string:join(lists:map(fun({F, Op, V}) ->
												Fld = field_index(F, FieldsTable, 2),
												io_lib:format("element(~s, R) ~s ~p", [integer_to_list(Fld), Op,  field_value(V)])
										  end, FilterList), ","),
			ExprQuery = lists:flatten(io_lib:format("[R || R <- mnesia:table(~p), ~s].", [Tab, Where])),
			ParsedQuery = qlc:string_to_handle(ExprQuery),
			mnesia:activity(transaction, fun () -> qlc:eval(ParsedQuery) end)
		end,
	ems_cache:get(ems_db_parsed_query_cache, ?LIFE_TIME_PARSED_QUERY, {Tab, FilterList}, F).


% match objects and faster than filter
% Ex.: ems_db:match(catalog_schema, [{id, 1}]).
% Sample result is like filter function
match(Tab, FilterList) -> 
	FieldsTable =  mnesia:table_info(Tab, attributes),
	Record = ems_schema:new_(Tab),
	Match = match(Tab, FilterList, FieldsTable, Record),
	mnesia:activity(transaction, fun() -> mnesia:match_object(Match) end).
		
match(_, [], _, Record) -> Record;
match(Tab, [{F, _, V}|T], FieldsTable, Record) -> 
	Fld = field_index(F, FieldsTable, 2),
	Record2 = setelement(Fld, Record, field_value(V)),
    match(Tab, T, FieldsTable, Record2);
match(Tab, [{F, V}|T], FieldsTable, Record) -> 
	Fld = field_index(F, FieldsTable, 2),
	Record2 = setelement(Fld, Record, field_value(V)),
    match(Tab, T, FieldsTable, Record2).


% select fields of object or list objects
% Ex.: ems_db:select_fields(#user{id = 1, name = "agilar", email = "evertonagilar@gmail.com"}, [name]).
% Sample result is [{<<"name">>,"agilar"}]
select_fields(ListRecord, []) -> ListRecord;
select_fields(ListRecord, FieldList) -> 
    FieldList2 = ems_util:list_to_binlist(FieldList),
	ems_schema:to_list(ListRecord, FieldList2).


field_index(_, [], _) -> erlang:error(einvalid_field_filter);
field_index(Field, Fields, Idx) when is_list(Field) -> 
	field_index(list_to_atom(Field), Fields, Idx);
field_index(Field, [F|Fs], Idx) ->
	case Field == F of
		true -> Idx;
		_ -> field_index(Field, Fs, Idx+1)
	end.


field_value(V) when is_list(V) -> list_to_binary(V);
field_value(V) -> V.


