-module(msbus_logger_tests).

-include_lib("eunit/include/eunit.hrl").

logger_test() ->

	msbus_logger:start(),

	msbus_logger:info("Mensagem info sem parâmetro."),

	msbus_logger:sync().

