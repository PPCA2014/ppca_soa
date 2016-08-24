#!/bin/sh
#
# author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
#
current_dir=`pwd`

# with observer daemon
#erl -pa $current_dir/ebin deps/json_rec/ebin deps/mochiweb/ebin deps/jiffy/ebin deps/ecsv/ebin deps/jsx/ebin deps/poolboy/ebin  -sname emsbus -setcookie erlangms -eval "ems_bus:start()" -boot start_sasl -config ./priv/conf/elog -run observer

erl -pa $current_dir/ebin deps/jesse/ebin deps/json_rec/ebin deps/mochiweb/ebin deps/jiffy/ebin deps/ecsv/ebin deps/jsx/ebin deps/poolboy/ebin  \
	-sname emsbus -setcookie erlangms \
	-eval "ems_bus:start()" \
	-boot start_sasl \
	-config ./priv/conf/elog
