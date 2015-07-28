-module(msbus_invalid_header_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("../include/msbus_schema.hrl").

	
start_server_test() ->
	msbus_logger:info("========= Testes invalid header ==========="),
	file:set_cwd("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus"),
	code:add_path("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus/deps/jsx/ebin"),
	application:start(msbus),
	application:start(inets),
	ok.

dominio() -> "http://localhost:2301".

header_inexistentes_test() ->
	msbus_logger:info("\n\nTEST: Varios parametros do header que nao existem mas serao ignorados e deve retornar OK."),
	Payload = <<"{\"nome\" : \"Usuário do teste\", \"email\":\"usuariodoteste@gmail.com\"}">>,
	HTTPHeader = [{"Content-Length", 445}, 
				  {"User-agent2", "Eunit Test"}, 
				  {"HosT", "localhost:2301"}, 
				  {"xxxx", ""}, 
				  "1", 
				  {"asdfa-12342134","1"}, 
				  {"ultra", "man"},
				  {"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",""},
				  {},
				  {"_", "_"},
				  {"_", "_"}],
	Url = dominio() ++ "/user",
	Request = {Url, HTTPHeader, "application/json", Payload},
	{ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, _Body}} = httpc:request(post, Request, [], []).

mime_type_invalido_test() ->
	msbus_logger:info("\n\nTEST: Passar mime-type invalido mas deve retornar OK."),
	Payload = <<"{\"nome\" : \"Usuário do teste\", \"email\":\"usuariodoteste@gmail.com\"}">>,
	HTTPHeader = [],
	Url = dominio() ++ "/user",
	Request = {Url, HTTPHeader, "qiwueoriqpwieuroqiueproiqwoeirupoqiueori", Payload},
	{ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, _Body}} = httpc:request(post, Request, [], []).

	
stop_server_test() ->
	msbus_logger:info("Finalizando os testes..."),
	msbus_util:sleep(1000),
	application:stop(inets),
	application:stop(msbus),
	ok.
	



