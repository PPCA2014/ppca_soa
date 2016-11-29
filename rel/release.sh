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
current_dir=$(dirname $0)
cd $current_dir


# Pega a versão do barramneto que está no arquivo rebar.config
VERSION_RELEASE=$(cat ../rebar.config | sed -rn  's/^.*\{"ems-bus\", \"([0-9]\.[0-9].[0-9])\".*$/\1/p')


echo "Aguarde, gerando a versão ems-bus-$VERSION_RELEASE do barramento, isso deve demorar um pouco!"


# Clean
echo "Limpando a pasta de release..."
rm -rf ems-bus
rm -rf ems-bus-$VERSION_RELEASE.gz
rm -rf deb/ems-bus_$VERSION_RELEASE_amd64.deb
rm -rf ems-bus_amd64/usr/lib/ems-bus  


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
tar -czf ems-bus-$VERSION_RELEASE.gz ems-bus/ &


# ####### Criar o pacote ems-bus_x.x.x_amd64.deb para instalação automatizada ############

echo "Criando pacote debian ems-bus-$VERSION_RELEASE.deb..."
cp -R ems-bus ems-bus_amd64/usr/lib/ems-bus 

# Atualiza a versão do pacote no arquivo DEBIAN/control 
sed -ri "s/Version: .*/Version: 1:$VERSION_RELEASE/" ems-bus_amd64/DEBIAN/control 

# Compila e gera o pacote deb
#dpkg-deb -b ems-bus_amd64 deb



echo Feito!

