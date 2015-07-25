-module(msbus_user_service_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("../include/msbus_schema.hrl").

	
start_server_test() ->
	msbus_logger:info("========= Testes msbus_user_service ==========="),
	file:set_cwd("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus"),
	code:add_path("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus/deps/jsx/ebin"),
	application:start(msbus),
	application:start(inets),
	ok.

url() -> "http://localhost:2301/user".
type() -> "application/json".
header() -> [{"User-Agent", "Eunit Test"}].

nome_em_branco_test() ->
	Payload = <<"{\"nome\" : \"\", \"email\":\"usuariodoteste\"}">>,
	Request = {url(), header(), type(), Payload},
	{ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, _Body}} = httpc:request(post, Request, [], []).

email_em_branco_test() ->
	Payload = <<"{\"nome\" : \"Usuário do teste\", \"email\":\"\"}">>,
	Request = {url(), header(), type(), Payload},
	{ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, _Body}} = httpc:request(post, Request, [], []).

email_invalido_test() ->
	Payload = <<"{\"nome\" : \"Usuário do teste\", \"email\":\"usuariodoteste\"}">>,
	Request = {url(), header(), type(), Payload},
	{ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, _Body}} = httpc:request(post, Request, [], []).

create_user_test() ->
	Payload = <<"{\"nome\" : \"Usuário do teste\", \"email\":\"usuariodoteste@gmail.com\"}">>,
	Request = {url(), header(), type(), Payload},
	{ok, {{"HTTP/1.1", 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, Request, [], []).



stop_server_test() ->
	msbus_logger:info("Finalizando os testes de msbus_user_service..."),
	msbus_util:sleep(1000),
	application:stop(inets),
	application:stop(msbus),
	ok.
	


