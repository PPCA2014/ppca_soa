#!/bin/bash
#
# Autor: Everton de Vargas Agilar
# Data: 08/06/2016
#
# Objetivo: Gerar a release do barramento para facilitar a instalação nas 
#           principais distros Linux. Os seguintes arquivos são gerados:
#				* arquivo ems-bus-x.x.x.tar.gz
#				* arquivo deb para as principais distros Linux
#				* pasta ems-bus com instalação standalone
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
# 28/11/2016  Everton Agilar     Release inicial do script de release
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


# Pega a versão dop build do barramneto que está no arquivo src/ems_bus.app.src
VERSION_RELEASE=$(cat ../src/ems_bus.app.src | sed -rn  's/^.*\{vsn.*([0-9]{1,2}\.[0-9]{1,2}.[0-9]{1,2}).*$/\1/p')
[ -z "$VERSION_RELEASE" ] && die "Não foi possível obter a versão a ser gerada no rebar.config"
echo "Aguarde, gerando a versão ems-bus-$VERSION_RELEASE do barramento, isso pode demorar um pouco!"


# ***** Clean e preparação para build ##########*
echo "Clean..."
rm -Rf ems-bus
rm -Rf *.tar.gz
rm -Rf deb/*.deb
# Loop pelas pastas de templates dos pacotes
for SKEL_RPM_PACKAGE in `find ./rpm/* -maxdepth 0 -type d`; do
	# remove old paths
	rm -Rf $SKEL_RPM_PACKAGE/BUILD
	rm -Rf $SKEL_RPM_PACKAGE/SOURCES
	rm -Rf $SKEL_RPM_PACKAGE/BUILDROOT
	rm -Rf $SKEL_RPM_PACKAGE/RPMS

	# create new paths
	mkdir -p $SKEL_RPM_PACKAGE/BUILD
	mkdir -p $SKEL_RPM_PACKAGE/SOURCES
	mkdir -p $SKEL_RPM_PACKAGE/BUILDROOT
	mkdir -p $SKEL_RPM_PACKAGE/RPMS
done


# ########## Recompila todo projeto antes de gerar a release ########## 
echo "Recompilando os fontes..."
cd ..
rm -f erl_crash.dump
rm -rf priv/db
rm -rf priv/log
rebar clean 1> /dev/null || die "Falha ao limpar os fontes!"
rebar get-deps 1>/dev/null || die "Falha ao obter as dependências!"
rebar compile 1> /dev/null || die "Falha ao recompilar os fontes!"

# ******** Gera o release na pasta rel *********
cd rel
echo "Gerando release com rebar..."
rebar compile generate || die "Falha ao gerar o release com rebar compile generate!"


# Renomeia a pasta gerada e o nome do script ems_bus para ems-bus
mv ems_bus ems-bus || die "Não foi possível renomear a pasta ems_bus para ems-bus!"
mv ems-bus/bin/ems_bus ems-bus/bin/ems-bus
rm -rf ems-bus/priv/www/


# Cria o link simbólico da pasta priv para a lib do projeto ems_bus-$VERSION/priv
cd ems-bus
ln -sf lib/ems_bus-$VERSION_RELEASE/priv/ priv || die "Não foi possível criar o link simbólico priv para lib/ems_bus-$VERSION_RELEASE/priv!"
# Faz algumas limpezas para não ir lixo no pacote
rm -Rf log || die "Não foi possível remover a pasta log na limpeza!"
rm -rf priv/db || die "Não foi possível remover a pasta db na limpeza!"
rm -rf priv/www
cd ..


# ####### Criar o pacote ems-bus-x.x.x.tar.gz para instalação manual #######

# Cria o arquivo do pacote gz
echo "Criando pacote ems-bus-$VERSION_RELEASE.gz..."
tar -czf ems-bus-$VERSION_RELEASE.tar.gz ems-bus/ &


# ####### Criar os pacotes rpm para cada distro ############

for SKEL_RPM_PACKAGE in `find ./rpm/* -maxdepth 0 -type d`; do
	echo "Criando pacote rpm para o template $SKEL_RPM_PACKAGE..."

	SKEL_RPM_PACKAGE_SOURCES="$SKEL_RPM_PACKAGE/SOURCES"
	VERSION_RELEASE_PACK="$VERSION_RELEASE.centos"
	
	# Cria a pasta onde vão ser colocados os sources 
	mkdir -p $SKEL_RPM_PACKAGE_SOURCES || die "Não foi possível criar a pasta $SKEL_RPM_PACKAGE_SOURCES!"

	# Gera a estrutura /usr/lib/ems-bus
	rm -Rf $SKEL_RPM_PACKAGE_SOURCES/usr/lib/ems-bus || die "Não foi possível remover pasta $SKEL_RPM_PACKAGE/usr/lib/ems-bus!" 
	mkdir -p $SKEL_RPM_PACKAGE_SOURCES/usr/lib
	cp -R ems-bus $SKEL_RPM_PACKAGE_SOURCES/usr/lib/ems-bus || die "Não foi possível copiar pasta ems-bus para $SKEL_RPM_PACKAGE/usr/lib!"

	rm -Rf $SKEL_RPM_PACKAGE_SOURCES/etc || die "Não foi possível remover pasta $SKEL_RPM_PACKAGE/etc!" 

	# Gera a estrutura /etc/ems-bus
	mkdir -p $SKEL_RPM_PACKAGE_SOURCES/etc/ems-bus || die "Não foi possível criar a pasta $SKEL_RPM_PACKAGE/etc/ems-bus!" 
	ln -s /usr/lib/ems-bus/priv/catalog $SKEL_RPM_PACKAGE_SOURCES/etc/ems-bus/catalog
	ln -s /usr/lib/ems-bus/priv/conf $SKEL_RPM_PACKAGE_SOURCES/etc/ems-bus/conf
	ln -s /usr/lib/ems-bus/priv/csv $SKEL_RPM_PACKAGE_SOURCES/etc/ems-bus/csv
	ln -s /usr/lib/ems-bus/priv/ssl $SKEL_RPM_PACKAGE_SOURCES/etc/ems-bus/ssl
	ln -s /usr/lib/ems-bus/priv/schema $SKEL_RPM_PACKAGE_SOURCES/etc/ems-bus/schema
	ln -s /usr/lib/ems-bus/priv/systemd $SKEL_RPM_PACKAGE_SOURCES/etc/ems-bus/systemd
	
	# Gera a estrutura /etc/systemd/system
	mkdir -p $SKEL_RPM_PACKAGE_SOURCES/etc/systemd/system
	ln -s /usr/lib/ems-bus/priv/systemd/ems-bus.service $SKEL_RPM_PACKAGE_SOURCES/etc/systemd/system/ems-bus.service || die "Não foi possível criar o link simbólico $SKEL_RPM_PACKAGE/etc/systemd/system/ems-bus.service!" 

	# Log -> /var/log/ems-bus
	ln -s /var/log/ems-bus $SKEL_RPM_PACKAGE_SOURCES/usr/lib/ems-bus/priv/log
	
	# Copia os scripts padrão para o pacote
	#cp -f rpm/emsbus.spec $SKEL_RPM_PACKAGE/SPECS
	
	# Atualiza a versão no arquivo SPEC/emsbus.spec
	echo aqui1
	echo "sed is sed -ri sed -ri s/Version: .*$/Version: $VERSION_RELEASE_PACK/  $SKEL_RPM_PACKAGE/SPECS/emsbus.spec"
	echo aqui2
	sed -ri "s/Version: .*$/Version: $VERSION_RELEASE_PACK/"  $SKEL_RPM_PACKAGE/SPECS/emsbus.spec
	#dpkg-deb -b $SKEL_RPM_PACKAGE deb || die "Falha ao gerar o pacote $SKEL_RPM_PACKAGE com dpkg-deb!"
	
	echo aqui3
	# tar sources path
	echo "Generate $SKEL_RPM_PACKAGE_SOURCES/ems-bus-$VERSION_RELEASE_PACK.tar.gz from $SKEL_RPM_PACKAGE_SOURCES"
	#cd $SKEL_RPM_PACKAGE_SOURCES
	tar -czvf  ems-bus-$VERSION_RELEASE_PACK.tar.gz *
	
	
	echo "rpm build..."
	cd $SKEL_RPM_PACKAGE
	#ln -s  /home/agilar/erlangms/ems-bus/rel/rpm/centos ~/rpmbuild 2> /dev/null
	pwd
	rpmbuild -bb SPECS/emsbus.spec

done


echo "Feito!"

