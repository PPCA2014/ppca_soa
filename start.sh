#!/bin/bash
#
# author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
#

# Erlang Runtime version required > 20
ERLANG_VERSION=20

# Erlang Runtime version installled
ERLANG_VERSION_OS=`erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell 2> /dev/null | sed 's/[^0-9]//g'`

# Prints a message and ends the system
# Parâmetros:
#  $1  - Mensagem que será impressa 
#  $2  - Código de retorno para o comando exit
die () {
    echo $1
    echo
    exit $2
}

# Checks if the version of erlang installed is compatible with this script
check_erlang_version(){
	printf "Checking Erlang Runtime version... "
	if [ -n "$ERLANG_VERSION_OS" ]; then
		if [ $ERLANG_VERSION_OS -ge $ERLANG_VERSION ]; then
			printf "OK\n"
		else
			printf "ERROR\n"
			die "Build canceled because the Erlang Runtime installed is incompatible with this software. Expected version: $ERLANG_VERSION"
		fi 
	else
		die "Oops, you should install Erlang Runtime $ERLANG_VERSION first !!!"
	fi
}

check_erlang_version

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

