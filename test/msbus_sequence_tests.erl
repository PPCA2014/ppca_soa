-module(msbus_sequence_tests).

-include_lib("eunit/include/eunit.hrl").

start_server_test() ->
	msbus_logger:info("========= Testes do Módulo msbus_sequence ==========="),
	file:set_cwd("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus"),
	code:add_path("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus/deps/jsx/ebin"),
	application:start(msbus),
	ok.

sequencer_test() ->
	msbus_logger:info("\n\nTEST: Criar algumas sequences. Todas devem ser criadas sem erro."),
	msbus_sequence:sequence(foo),
	msbus_sequence:sequence(foo),
	msbus_sequence:sequence(foo),
	msbus_sequence:sequence(cat),
	msbus_sequence:sequence(test),
	ok.

init_sequence_test() ->
	msbus_logger:info("\n\nTEST: Criar duas sequences foo que devem ser 1 e 2."),
	msbus_sequence:init_sequence(foo, 0),
	?assert(msbus_sequence:sequence(foo) =:= 1),
	?assert(msbus_sequence:sequence(foo) =:= 2),
	ok.


stop_server_test() ->
	application:stop(msbus),
	ok.
	

