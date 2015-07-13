-module(msbus_sequence_tests).

-include_lib("eunit/include/eunit.hrl").

start_server_test() ->
	%% Inicia o barramento antes de executar os testes
	file:set_cwd("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus"),
	code:add_path("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus/deps/jsx/ebin"),
	application:start(msbus),
	ok.

sequencer_test() ->
	msbus_sequence:sequence(foo),
	msbus_sequence:sequence(foo),
	msbus_sequence:sequence(foo),
	msbus_sequence:sequence(cat),
	msbus_sequence:sequence(test),
	ok.

init_sequence_test() ->
	msbus_sequence:init_sequence(foo, 0),
	?assert(msbus_sequence:sequence(foo) =:= 1),
	?assert(msbus_sequence:sequence(foo) =:= 2),
	ok.


stop_server_test() ->
	application:stop(msbus),
	ok.
	

