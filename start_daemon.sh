#!/bin/sh
erl -detached  -pa ../msbus/ebin deps/jsx/ebin deps/poolboy/ebin -eval "application:start(msbus)" -boot start_sasl -config ./priv/conf/elog
	
