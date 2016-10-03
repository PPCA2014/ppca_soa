#!/bin/bash
#
# author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
#
current_dir=`pwd`

if [ "$1" == "observer" ]; then
	echo "Start with observer daemon..."
	erl -pa $current_dir/ebin deps/jesse/ebin deps/json_rec/ebin deps/mochiweb/ebin deps/jiffy/ebin deps/ecsv/ebin deps/jsx/ebin deps/poolboy/ebin deps/erlydtl/ebin \
		-sname emsbus -setcookie erlangms \
		-eval "ems_bus:start()" \
		-boot start_sasl \
		-config ./priv/conf/elog \
		-run observer
else
	erl -pa $current_dir/ebin deps/jesse/ebin deps/json_rec/ebin deps/mochiweb/ebin deps/jiffy/ebin deps/ecsv/ebin deps/jsx/ebin deps/poolboy/ebin deps/erlydtl/ebin \
		-sname emsbus -setcookie erlangms \
		-eval "ems_bus:start()" \
		-boot start_sasl \
		-config ./priv/conf/elog
fi

