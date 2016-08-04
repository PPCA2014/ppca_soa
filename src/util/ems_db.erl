%%********************************************************************
%% @title Module ems_db
%% @version 1.0.0
%% @doc Module that provides interface with the database.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_db).

-export([start/0]).
-export([get/2, all/1, insert/1, update/1, delete/2, existe/1, match_object/1]).
-export([sequence/1, init_sequence/2]).
-export([get_odbc_connection/1, release_odbc_connection/1]).
-export([create_sqlite_virtual_table_from_csv_file/3, get_odbc_connection_csv_file/4]).


-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").


-define(CSV2SQLITE_PATH, ?WORKING_PATH ++ "/bin/csv2sqlite.py"). 
-define(DATABASE_SQLITE_PATH, ?DATABASE_PATH ++ "/ems_dynamic_view.dat").	
-define(DATABASE_SQLITE_STRING_CONNECTION, lists:flatten(io_lib:format("DRIVER=SQLite;Version=3;Database=~s;", [?DATABASE_SQLITE_PATH]))).	



%% *********** Database schema creation ************

start() ->
	create_database([node()]).
	
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

    mnesia:create_table(request, [{type, set},
								  {ram_copies, Nodes},
								  {attributes, record_info(fields, request)},
								  {index, [#request.timestamp]}]),

    mnesia:create_table(ctrl_sqlite_table, [{type, set},
											{ram_copies, Nodes},
											{attributes, record_info(fields, ctrl_sqlite_table)}]),




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


get_odbc_connection(Datasource) ->
	try
		case odbc:connect(Datasource, [{scrollable_cursors, off},
									   {timeout, 3500},
									   {trace_driver, off}]) of
			{ok, Conn}	-> {ok, Conn};
			{error, Reason} -> {error, Reason}
		end
	catch
		_Exception:Reason2 -> {error, Reason2}
	end.

release_odbc_connection(Conn) ->
	odbc:disconnect(Conn).

get_odbc_connection_csv_file(FileName, TableName, _PrimaryKey, Delimiter) -> 
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
			ems_db:get_odbc_connection(?DATABASE_SQLITE_STRING_CONNECTION)
	end.

	

create_sqlite_virtual_table_from_csv_file(FileName, TableName, _PrimaryKey) -> 
	{ok, Conn} = ems_db:get_odbc_connection("DRIVER=SQLite;Version=3;New=True;"),
	odbc:sql_query(Conn, "select load_extension(\"/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtable.so\")"),
	CreateTableDDL = lists:flatten(io_lib:format("create virtual table ~s using csvtable(\"~s\")", [TableName, FileName])),
	odbc:sql_query(Conn, CreateTableDDL),
	odbc:commit(Conn, commit),
	{ok, Conn}.
	
	
