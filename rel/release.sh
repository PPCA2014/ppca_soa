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


# Executar a partir do diretório do próprio script
WORKING_DIR=$(dirname $0)
cd $WORKING_DIR


# Pega a versão do barramneto que está no arquivo rebar.config
VERSION_RELEASE=$(cat ../rebar.config | sed -rn  's/^.*\{"ems-bus\", \"([0-9]\.[0-9].[0-9])\".*$/\1/p')


echo "Aguarde, gerando a versão ems-bus-$VERSION_RELEASE do barramento, isso deve demorar um pouco!"


# Clean
echo "Limpando a pasta de release..."
rm -Rf ems-bus
rm -Rf ems-bus-$VERSION_RELEASE.tar.gz
rm -rf deb/*.deb


# Recompila todo projeto antes de iniciar a release
echo "Recompilando os fontes..."
cd ..
rebar clean >> /dev/null
rebar compile >> /dev/null
cd rel


# Gera o release
echo "Gerando release com rebar..."
rebar compile generate || exit 1


# Renomeia a pasta gerada e o nome do script ems_bus para ems-bus
mv ems_bus ems-bus
mv ems-bus/bin/ems_bus ems-bus/bin/ems-bus


# Cria o link simbólico da pasta priv para a lib do projeto ems_bus-$VERSION/priv
cd ems-bus
ln -sf lib/ems_bus-$VERSION_RELEASE/priv/ priv
# Faz algumas limpezas para não ir lixo no pacote
rm -Rf log
rm -rf priv/db
cd ..


# ####### Criar o pacote ems-bus-x.x.x.gz para instalação manual #######

# Cria o arquivo do pacote gz
echo "Criando pacote ems-bus-$VERSION_RELEASE.gz..."
tar -czf ems-bus-$VERSION_RELEASE.tar.gz ems-bus/ &


# ####### Criar o pacote ems-bus-x.x.x.Ubuntu-yakkety_amd64.deb ############

echo "Criando o pacote $DEB_PACKAGE.deb..."
SKEL_DEB_PACKAGE=deb/ems-bus-Ubuntu-yakkety_amd64
rm -Rf $SKEL_DEB_PACKAGE/usr/lib/ems-bus  
cp -R ems-bus $SKEL_DEB_PACKAGE/usr/lib/ems-bus 
# Atualiza a versão no arquivo DEBIAN/control 
sed -ri "s/Version: .*/Version: 1:$VERSION_RELEASE/" $SKEL_DEB_PACKAGE/DEBIAN/control 
dpkg-deb -b $SKEL_DEB_PACKAGE deb



echo Feito!

