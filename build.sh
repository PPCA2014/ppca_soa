#!/bin/bash
#
# Autor: Everton de Vargas Agilar
#
# Objetivo: Faz o build do projeto.
#
# Modo de usar: 
#
#    $ ./build.sh
#
#
#
## Histórico de modificações do software:
#
# Data       |  Quem           |  Mensagem  
# -----------------------------------------------------------------------------------------------------
# 10/11/2015  Everton Agilar     Release inicial do script de release
#
#
#
#
#
#
#
########################################################################################################

VERSION_SCRIPT="1.0.0"
SKIP_DEPS="false"
SKIP_CLEAN="false"
KEEP_DB="false"

# Imprime na tela a ajuda do comando
help() {
	echo
	echo "How to use: sudo ./build.sh"
	echo ""
	echo "Additional parameters:"
	echo "  --keep_db                 -> does not delete the priv/db folder"
	echo "  --skip_deps               -> skip rebar deps"
	echo "  --skip_clean              -> skip rebar clean"
	echo
	exit 1
}

echo " _             _ _            __     __      "
echo "|_)   o| _|   |_|_)|  /\ |\ |/__|\/|(_       "
echo "|_)|_|||(_|   |_| \|_/--\| \|\_||  |__)  ooo "
                                             

# Read command line parameters
for P in $*; do
	if [[ "$P" =~ ^--.+$ ]]; then
		if [ "$P" = "--skip_deps" ]; then
			SKIP_DEPS="true"
		elif [ "$P" = "--skip_clean" ]; then
			SKIP_CLEAN="true"
		elif [ "$P" = "--keep_db" ]; then
			KEEP_DB="true"
		elif [ "$P" = "--help" ]; then
			help
		else
			echo "Invalid parameter: $P"
			help
		fi
	fi
done

echo "-------------------------------------------------------------------------"
echo 

rm -Rf priv/log
rm -f *.dump

# limpa a pasta do banco de dados do barramento
if [ "$KEEP_DB" = "false" ]; then
	rm -Rf priv/db
else
	echo "does not delete the priv/db folder..."
fi	


# algumas libs devem ser limpas para garantir a última versão
if [ "$SKIP_DEPS" = "false" ]; then
	rm -Rf deps/cowboy
	rm -Rf deps/cowlib
	rm -Rf deps/ranch
	rm -Rf deps/oauth2
	rm -Rf deps/erlydtl
	if [ "$SKIP_CLEAN" = "false" ]; then	
		tools/rebar/rebar clean get-deps compile	
	else
		echo "skip rebar clean..."
		tools/rebar/rebar get-deps compile	
	fi
else
	echo "skip rebar deps..."
	if [ "$SKIP_CLEAN" = "false" ]; then	
		tools/rebar/rebar clean compile	
	else
		echo "skip rebar clean..."
		tools/rebar/rebar clean compile	
	fi
fi




