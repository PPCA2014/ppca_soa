-module(msbus_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("../include/msbus_schema.hrl").

start_server_test() ->
	msbus_logger:info("========= Teste do Módulo msbus_user ==========="),
	file:set_cwd("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus"),
	code:add_path("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus/deps/jsx/ebin"),
	application:start(msbus),
	ok.

lista_todos_os_users_call_test() ->
	msbus_user:call(all).
	
lista_todos_os_users_cast_test() ->
	msbus_user:cast({all, self()}).
	
inserir_user_test() ->
	Id = integer_to_list(msbus_sequence:sequence(user_test)),
	User1 = #user{nome="Usuario " ++ Id, email="usuario " ++ Id ++ "@gmail.com"},
	{ok, _User2} = msbus_user:call({insert, User1}).

atualizar_user_test() ->
	%% cria um user primeiro
	UserNo = integer_to_list(msbus_sequence:sequence(user_test)),
	User1 = #user{nome="Usuario " ++ UserNo, email="usuario " ++ UserNo ++ "@gmail.com"},
	{ok, User2} = msbus_user:call({insert, User1}),

	%% atualiza um usuário
	Nome2 = User2#user.nome ++ " (update)",
	User3 = User2#user{nome = Nome2},
	msbus_user:call({update, User3}),

	%% ele foi persistido?
	{ok, _Record} = msbus_user:call({get, User3#user.id}).

test_delete_test() ->
	%% insere um usuário
	UserNo = integer_to_list(msbus_sequence:sequence(user_test)),
	User1 = #user{nome="Usuario " ++ UserNo, email="usuario " ++ UserNo ++ "@gmail.com"},
	{ok, User2} = msbus_user:call({insert, User1}),

	msbus_user:call({delete, User2#user.id}),
	?assert(msbus_user:call({get, User2#user.id}) =:= {erro, notfound}).

stop_server_test() ->
	application:stop(msbus),
	ok.
