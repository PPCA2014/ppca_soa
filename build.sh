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

# limpa a pasta de logs e o banco de dados
rm -Rf priv/log
rm -Rf priv/db

# algumas libs devem ser limpas para garantir a última versão master
rm -Rf deps/cowboy
rm -Rf deps/cowlib
rm -Rf deps/ranch
rm -Rf deps/oauth2

tools/rebar/rebar clean get-deps compile
