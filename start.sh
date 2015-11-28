#!/bin/sh
#
# start msbus and build before if necessary
# author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
#
alias rebar=./rebar
./build.sh
erl -pa ../msbus/ebin deps/jsx/ebin deps/poolboy/ebin -sname msbus -setcookie erlangms -eval "msbus:start()" -boot start_sasl -config ./priv/conf/elog
