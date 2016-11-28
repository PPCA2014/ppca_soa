#!/bin/bash
#
# Autor: Everton de Vargas Agilar
# Data: 08/06/2016
#
# Objetivo: Gerar a release do barramento
#
#
#
# Requisitos para executar o serviço:
#
#
#
#
#
# Modo de usar: 
#
#    $ ./release.sh
#
#
#
#
#
## Histórico de modificações do software:
#
# Data       |  Quem           |  Mensagem  
# -----------------------------------------------------------------------------------------------------
# 28/11/2016  Everton Agilar     Release inicial    
#
#
#
#
#
#
#
########################################################################################################

echo "Script para gerar versão do barramento"
echo "Date: $(date '+%d/%m/%Y %H:%M:%S')"

# Executar a partir do diretório do próprio script
current_dir=$(dirname $0)
cd $current_dir

# Pega a versão que será gerada do arquivo rebar.config
VERSION=$(cat ../rebar.config | sed -rn  's/^.*\{"ems_bus\", \"([0-9]\.[0-9].[0-9])\".*$/\1/p')

echo "Aguarde, gerando a versão $VERSION do barramento, isso deve demorar um pouco!"

# Recompila todo projeto 
cd ..
rebar clean
rebar compile
cd rel

# Apaga a pasta ems_bus do release anterior (se existir)
rm -Rf ems_bus

# Apaga o pacote do release anterior (se existir)
rm -f ems_bus.gz 
rm -f ems_bus.tar

# Gera o release
rebar compile generate

# Cria o link simbólico da pasta priv 
cd ems_bus
ln -sf lib/ems_bus-$VERSION/priv/ priv
# Faz algumas limpezas
rm -Rf log
rm -rf priv/db
cd ..

# Empacotando
tar -czf ems_bus-$VERSION ems_bus/

echo Feito!

