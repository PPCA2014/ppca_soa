-module(msbus_parse_http_headers_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("../include/msbus_schema.hrl").

	
start_server_test() ->
	msbus_logger:info("========= Testes msbus_parse_http_headers_tests ==========="),
	file:set_cwd("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus"),
	code:add_path("/home/agilar/Dropbox/workspace/erlang/erlangMS/msbus/deps/jsx/ebin"),
	application:start(msbus),
	application:start(inets),
	ok.

parse_http_headers_test() ->
	H1 =  "GET /health/top_services/10 HTTP/1.1\r\nHost: localhost:2301\r\nConnection: keep-alive\r\nCache-Control: max-age=0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.89 Safari/537.36\r\nHTTPS: 1\r\nDNT: 1\r\nAccept-Encoding: gzip, deflate, sdch\r\nAccept-Language: pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4\r\nCookie: csrftoken=IPIn48a3JySsrV5Io5nudh5ZJPF6sERb",
    msbus_http_util:get_http_header(H1).

	
stop_server_test() ->
	msbus_logger:info("Finalizando os testes..."),
	application:stop(inets),
	application:stop(msbus),
	ok.
	



