#!/bin/sh
#erl ../msbus/ebin deps/jsx/ebin -eval "application:start(msbus)" -boot start_sasl -config elog
erl -pa ../msbus/ebin deps/jsx/ebin -sname msbus -eval "application:start(msbus)"
	
