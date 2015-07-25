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

%% Retorno 200

get_stress_hello_world_test() ->
	Request = fun(_) -> 
					{ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} = 
							httpc:request(get, {dominio() ++ "/hello_world", []}, [], [])
			   end,
	lists:foreach(Request, lists:seq(1, 1000)).

%% Erros 404


get_stress_test() ->
	Request = fun(T) -> 
					{ok, {{_Version, 404, _ReasonPhrase}, _Headers, _Body}} = 
							httpc:request(get, {dominio() ++ "/recurso_" ++ integer_to_list(T), []}, [], [])
			   end,
	lists:foreach(Request, lists:seq(1, 1000)).
	

stop_server_test() ->
	msbus_logger:info("Finalizando os testes de stress..."),
	msbus_util:sleep(1000),
	application:stop(inets),
	application:stop(msbus),
	ok.
	


