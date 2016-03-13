#!/bin/sh
#
# author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
#
erl -pa ../emsbus/ebin deps/jsx/ebin deps/poolboy/ebin -sname emsbus -setcookie erlangms -eval "ems_bus:start()" -boot start_sasl -config ./priv/conf/elog
