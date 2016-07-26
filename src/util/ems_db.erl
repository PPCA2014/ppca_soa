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
-export([create_sqlite_virtual_table_from_csv_file/3, create_sqlite_table_from_csv_file/4]).


-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% *********** Database schema creation ************

start() ->
	create_database([node()]).
	
create_database(Nodes) ->
	mnesia:create_schema(Nodes),
	mnesia:start(),

    mnesia:create_table(user, [{type, set},
							   {disc_copies, Nodes},
							   {attributes, record_info(fields, user)}]),

    mnesia:create_table(sequence, [{type, set},
									{ram_copies, Nodes},
									{attributes, record_info(fields, sequence)}]),

    mnesia:create_table(request, [{type, set},
									 {ram_copies, Nodes},
									 {attributes, record_info(fields, request)},
									 {index, [#request.timestamp]}]),

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
		io:format("datasource is ~p\n", [Datasource]),
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
	

create_sqlite_table_from_csv_file(FileName, TableName, _PrimaryKey, Delimiter) -> 
	DatabasePath = ?PRIV_PATH ++ "/db/",
	filelib:ensure_dir(DatabasePath),
	DatabaseName = DatabasePath ++ "database.sqlite",	
	io:format("database is ~p\n", [DatabaseName]),
	Cmd = lists:flatten(io_lib:format("./csv2sqlite.py '~s\' '~s' '~s' '~s'", [DatabaseName, 
										  								     TableName, 
																			 FileName, 
																			 Delimiter])),
	io:format(Cmd),
	os:cmd(Cmd),
	StringConn = lists:flatten(io_lib:format("DRIVER=SQLite;Version=3;Database=~s;", [DatabaseName])),
	io:format("str conn is ~p\n", [StringConn]),
	ems_db:get_odbc_connection(StringConn).

create_sqlite_virtual_table_from_csv_file(FileName, TableName, _PrimaryKey) -> 
	{ok, Conn} = ems_db:get_odbc_connection("DRIVER=SQLite;Version=3;New=True;"),
	odbc:sql_query(Conn, "select load_extension(\"/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtable.so\")"),
	CreateTableDDL = lists:flatten(io_lib:format("create virtual table ~s using csvtable(\"~s\")", [TableName, FileName])),
	odbc:sql_query(Conn, CreateTableDDL),
	odbc:commit(Conn, commit),
	{ok, Conn}.
	
	
