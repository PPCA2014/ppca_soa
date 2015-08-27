-module(msbus_json_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("../include/msbus_schema.hrl").

	
start_server_test() ->
	msbus_logger:info("========= Testes para conversão JSON ==========="),
	file:set_cwd("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus"),
	code:add_path("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus/deps/jsx/ebin"),
	application:start(msbus),
	ok.

	
json_encode_string_test() ->
	msbus_util:json_encode("o rato roeu a roupa do rei de roma"),
	ok.

json_encode_string_bin_test() ->
	msbus_util:json_encode(<<"o rato roeu a roupa do rei de roma">>),
	ok.

json_encode_tupla_test() ->
	msbus_util:json_encode({user, "Everton", "Agilar", 34}),
	ok.

json_encode_lista_de_tupla_test() ->
	msbus_util:json_encode([{user, "Everton", "Agilar", 34}, {user, "Maria", "", 123456789}]),
	ok.

json_encode_lista_de_tupla2_test() ->
	msbus_util:json_encode([{user, "Everton", "Agilar", {endereco, "Brasilia", "Asa Norte", 604}}, {user, "Maria", "", 123456789}]),
	ok.

json_encode_lista_vazia_test() ->
	msbus_util:json_encode([]),
	ok.

json_encode_tupla_vazia_test() ->
	msbus_util:json_encode({}),
	ok.

stop_server_test() ->
	application:stop(msbus),
	ok.
	


