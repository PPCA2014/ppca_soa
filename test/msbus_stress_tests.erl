-module(msbus_stress_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("../include/msbus_schema.hrl").

	
start_server_test() ->
	msbus_logger:info("========= Testes Stress ==========="),
	file:set_cwd("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus"),
	code:add_path("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus/deps/jsx/ebin"),
	application:start(msbus),
	application:start(inets),
	ok.

dominio() -> "http://localhost:2301".

get_stress_hello_world_test_() ->
	msbus_logger:info("\n\nTEST: Stressar o servidor com varias requisicoes hello_world. Todas devem retornar OK."),
	Request = fun(_) -> 
					{ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} = 
							httpc:request(get, {dominio() ++ "/hello_world", []}, [], [])
			   end,
	lists:foreach(Request, lists:seq(1, 10)).

get_stress_url_nao_existe_test_() ->
	msbus_logger:info("\n\nTEST: Stressar o servidor com varias requisicoes para URLs que nao existem. Todas devem retornar 404."),
	Request = fun(T) -> 
					{ok, {{_Version, 404, _ReasonPhrase}, _Headers, _Body}} = 
							httpc:request(get, {dominio() ++ "/recurso_" ++ integer_to_list(T), []}, [], [])
			   end,
	lists:foreach(Request, lists:seq(1, 10)).
	
get_stress_payload_em_branco_insert_user_test_() ->
	msbus_logger:info("\n\nTEST: Com payload em branco, todos os processos de user vao quebrar e deve retornar servico_falhou."),
	Request = fun(T) -> 
					Payload = "",
					HTTPHeader = [],
					Url = dominio() ++ "/user",
					Request = {Url, HTTPHeader, "application/json", Payload},
					{ok, {{_Version, 502, _ReasonPhrase}, _Headers, _Body}} = httpc:request(post, Request, [], [])
			   end,
	lists:foreach(Request, lists:seq(1, 10)).

stop_server_test() ->
	msbus_logger:info("Finalizando os testes de stress..."),
	application:stop(inets),
	application:stop(msbus),
	ok.
	


