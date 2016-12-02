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

rm -Rf priv/log
rm -Rf priv/db
rm -Rf log
tools/rebar/rebar clean get-deps compile
