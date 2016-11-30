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

# Imprime uma mensagem e termina o script
# Parâmetros:
#  $1  - Mensagem que será impressa 
die () {
    echo $1
    exit 1
}


# Executar a partir do diretório do próprio script
WORKING_DIR=$(dirname $0)
cd $WORKING_DIR


# Pega a versão do barramneto que está no arquivo rebar.config
VERSION_RELEASE=$(cat ../src/ems_bus.app.src | sed -rn  's/^.*\{vsn.*([0-9]\.[0-9].[0-9]).*$/\1/p')
[ -z "$VERSION_RELEASE" ] && die "Não foi possível obter a versão a ser gerada no rebar.config"
echo "Aguarde, gerando a versão ems-bus-$VERSION_RELEASE do barramento, isso deve demorar um pouco!"


# Clean
echo "Limpando a pasta rel..."
rm -Rf ems-bus
rm -Rf ems-bus-$VERSION_RELEASE.tar.gz
rm -Rf deb/*.deb
for skel_deb_folder in `ls -1 deb/`; do
	rm -Rf deb/$skel_deb_folder/usr/lib/ems-bus
done


# Recompila todo projeto antes de iniciar a release
echo "Recompilando os fontes..."
cd ..
rebar clean 1> /dev/null || die "Falha ao limpar os fontes!"
rebar get-deps 1>/dev/null || die "Falha ao obter as dependências!"
rebar compile 1> /dev/null || die "Falha ao recompilar os fontes!"
cd rel


# Gera o release
echo "Gerando release com rebar..."
rebar compile generate || die "Falha ao gerar o release com rebar compile generate!"


# Renomeia a pasta gerada e o nome do script ems_bus para ems-bus
mv ems_bus ems-bus || die "Não foi possível renomear a pasta ems_bus para ems-bus!"
mv ems-bus/bin/ems_bus ems-bus/bin/ems-bus 


# Cria o link simbólico da pasta priv para a lib do projeto ems_bus-$VERSION/priv
cd ems-bus
ln -sf lib/ems_bus-$VERSION_RELEASE/priv/ priv || die "Não foi possível criar o link simbólico priv para lib/ems_bus-$VERSION_RELEASE/priv!"
# Faz algumas limpezas para não ir lixo no pacote
rm -Rf log || die "Não foi possível remover a pasta log na limpeza!"
rm -rf priv/db || die "Não foi possível remover a pasta db na limpeza!"
cd ..


# ####### Criar o pacote ems-bus-x.x.x.gz para instalação manual #######

# Cria o arquivo do pacote gz
echo "Criando pacote ems-bus-$VERSION_RELEASE.gz..."
tar -czf ems-bus-$VERSION_RELEASE.tar.gz ems-bus/ &


# ####### Criar o pacote ems-bus-x.x.x.Ubuntu-yakkety_amd64.deb ############

SKEL_DEB_PACKAGE=deb/ems-bus-Ubuntu-yakkety_amd64
rm -Rf $SKEL_DEB_PACKAGE/usr/lib/ems-bus  
cp -R ems-bus $SKEL_DEB_PACKAGE/usr/lib/ems-bus 
# Atualiza a versão no arquivo DEBIAN/control 
sed -ri "s/Version: .*/Version: 1:$VERSION_RELEASE/" $SKEL_DEB_PACKAGE/DEBIAN/control 
echo "Criando pacote $SKEL_DEB_PACKAGE..."
dpkg-deb -b $SKEL_DEB_PACKAGE deb || die "Falha ao gerar o pacote $SKEL_DEB_PACKAGE com dpkg-deb!"


#########################################################################################

# Apaga as pastas ems-bus que foram copiados para cada SKEL_DEB_PACKAGE/usr/lib
for skel_deb_folder in `ls -1 deb/`; do
	rm -Rf deb/$skel_deb_folder/usr/lib/ems-bus
done


echo "Feito!"

