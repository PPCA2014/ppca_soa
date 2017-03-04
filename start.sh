#!/bin/bash
#
# author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
#

current_dir=$(dirname $0)
cd $current_dir
deps=$(ls -d deps/*/ebin)

if [ "$1" == "observer" ]; then
	echo "Start with observer daemon..."
	/usr/bin/erl -pa $current_dir/ebin $deps \
		-sname emsbus -setcookie erlangms \
		-eval "ems_bus:start()" \
		-boot start_sasl \
		-config $current_dir/priv/conf/elog \
		-run observer \
		--enable-dirty-schedulers
else
	/usr/bin/erl -pa $current_dir/ebin $deps \
		-sname emsbus -setcookie erlangms \
		-eval "ems_bus:start()" \
		-boot start_sasl \
		-config $current_dir/priv/conf/elog \
		--enable-dirty-schedulers
fi

