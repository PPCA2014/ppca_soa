#!/bin/sh
#erl ../msbus/ebin deps/jsx/ebin -eval "application:start(msbus)" -boot start_sasl -config elog
erl -pa ../msbus/ebin deps/jsx/ebin deps/poolboy/ebin -sname msbus -setcookie erlangms -eval "application:start(msbus)"
	
