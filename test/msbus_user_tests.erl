-module(msbus_user_tests).

-export([start/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("../include/db_schema.hrl").

start() ->
	test_ins_update_call(),
	test_ins_update_cast(),
	test_delete_call().
	
test_ins_update_call() ->
	%mnesia:clear_table(user),
	
	%% lista todos
	msbus_user:call(all),

	%% insere um usuário
	Id = integer_to_list(msbus_sequence:sequence(user_test)),
	User1 = #user{nome="Usuario " ++ Id, email="usuario " ++ Id ++ "@gmail.com"},
	{ok, User2} = msbus_user:call({insert, User1}),
	
	%% atualiza um usuário
	Nome2 = User2#user.nome ++ " (update)",
	User3 = User2#user{nome = Nome2},
	msbus_user:call({update, User3}),
	
	msbus_user:call(all).


test_delete_call() ->
	%%mnesia:clear_table(user),

	%% insere um usuário
	Id = integer_to_list(msbus_sequence:sequence(user_test)),
	User1 = #user{nome="Usuario " ++ Id, email="usuario " ++ Id ++ "@gmail.com"},
	{ok, User2} = msbus_user:call({insert, User1}),

	msbus_user:call({delete, User2#user.id}).


test_ins_update_cast() ->
	%mnesia:clear_table(user),
	
	%% lista todos
	msbus_user:cast({all, self()}),

	%% insere um usuário
	Id = integer_to_list(msbus_sequence:sequence(user_test)),
	User1 = #user{nome="Usuario " ++ Id, email="usuario " ++ Id ++ "@gmail.com"},
	msbus_user:cast({insert, User1, self()}),
	receive
		{ok, User2} -> 
			io:format("~p\n", [User2])
			%% atualiza um usuário
			%Nome2 = User2#user.nome ++ " (update)",
			%User3 = User2#user{nome = Nome2},
			%msbus_user:cast({update, User3, self()})
	end,
	
	msbus_user:cast({all, self()}).


