#!/bin/sh
#
# author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
#
erl -pa ../msbus/ebin deps/jsx/ebin deps/poolboy/ebin -sname msbus -setcookie erlangms -eval "msbus:start()" -boot start_sasl -config ./priv/conf/elog
