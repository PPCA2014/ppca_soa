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


# Pega a versão do barramneto que está no arquivo rebar.config
VERSION_RELEASE=$(cat ../src/ems_bus.app.src | sed -rn  's/^.*\{vsn.*([0-9]\.[0-9].[0-9]).*$/\1/p')
[ -z "$VERSION_RELEASE" ] && die "Não foi possível obter a versão a ser gerada no rebar.config"
echo "Aguarde, gerando a versão ems-bus-$VERSION_RELEASE do barramento, isso pode demorar um pouco!"


# Clean
echo "Limpando a pasta rel..."
rm -Rf ems-bus
rm -Rf *.tar.gz
rm -Rf deb/*.deb
# Loop pelas pastas de templates dos pacotes
for SKEL_DEB_PACKAGE in `find ./deb/* -maxdepth 0 -type d`; do
	rm -Rf $SKEL_DEB_PACKAGE/usr/lib/ems-bus
	rm -Rf $SKEL_DEB_PACKAGE/etc/ems-bus
	rm -Rf $SKEL_DEB_PACKAGE/etc/systemd
done


# Recompila todo projeto antes de gerar a release
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


# ####### Criar o pacote ems-bus-x.x.x.tar.gz para instalação manual #######

# Cria o arquivo do pacote gz
echo "Criando pacote ems-bus-$VERSION_RELEASE.gz..."
tar -czf ems-bus-$VERSION_RELEASE.tar.gz ems-bus/ &


# ####### Criar os pacotes deb para cada distro ############

for SKEL_DEB_PACKAGE in `find ./deb/* -maxdepth 0 -type d`; do
	echo "Criando pacote deb para o template $SKEL_DEB_PACKAGE..."

	# Gera a estrutura /usr/lib/ems-bus
	rm -Rf $SKEL_DEB_PACKAGE/usr/lib/ems-bus || die "Não foi possível remover pasta $SKEL_DEB_PACKAGE/usr/lib/ems-bus!" 
	mkdir -p $SKEL_DEB_PACKAGE/usr/lib
	cp -R ems-bus $SKEL_DEB_PACKAGE/usr/lib/ems-bus || die "Não foi possível copiar pasta ems-bus para $SKEL_DEB_PACKAGE/usr/lib!"

	rm -Rf $SKEL_DEB_PACKAGE/etc || die "Não foi possível remover pasta $SKEL_DEB_PACKAGE/etc!" 

	# Gera a estrutura /etc/ems-bus
	mkdir -p $SKEL_DEB_PACKAGE/etc/ems-bus || die "Não foi possível criar a pasta $SKEL_DEB_PACKAGE/etc/ems-bus!" 
	ln -s /usr/lib/ems-bus/priv/catalog $SKEL_DEB_PACKAGE/etc/ems-bus/catalog
	ln -s /usr/lib/ems-bus/priv/conf $SKEL_DEB_PACKAGE/etc/ems-bus/conf
	ln -s /usr/lib/ems-bus/priv/csv $SKEL_DEB_PACKAGE/etc/ems-bus/csv
	ln -s /usr/lib/ems-bus/priv/ssl $SKEL_DEB_PACKAGE/etc/ems-bus/ssl
	ln -s /usr/lib/ems-bus/priv/schema $SKEL_DEB_PACKAGE/etc/ems-bus/schema
	ln -s /usr/lib/ems-bus/priv/systemd $SKEL_DEB_PACKAGE/etc/ems-bus/systemd
	
	# Gera a estrutura /etc/systemd/system
	mkdir -p $SKEL_DEB_PACKAGE/etc/systemd/system
	ln -s /usr/lib/ems-bus/priv/systemd/ems-bus.service $SKEL_DEB_PACKAGE/etc/systemd/system/ems-bus.service || die "Não foi possível criar o link simbólico $SKEL_DEB_PACKAGE/etc/systemd/system/ems-bus.service!" 

	# Log -> /var/log/ems-bus
	ln -s /var/log/ems-bus $SKEL_DEB_PACKAGE/usr/lib/ems-bus/priv/log
	

	# Atualiza a versão no arquivo DEBIAN/control 
	sed -ri "s/Version: .{6}(.*$)/Version: $VERSION_RELEASE-\1/" $SKEL_DEB_PACKAGE/DEBIAN/control
	dpkg-deb -b $SKEL_DEB_PACKAGE deb || die "Falha ao gerar o pacote $SKEL_DEB_PACKAGE com dpkg-deb!"
done


#########################################################################################

# Apaga as pastas ems-bus que foram copiados para cada SKEL_DEB_PACKAGE/usr/lib
# pois não são mais necessárias
for SKEL_DEB_PACKAGE in `find ./deb/* -maxdepth 0 -type d`; do
	rm -Rf $SKEL_DEB_PACKAGE/usr/lib/ems-bus
	rm -Rf $SKEL_DEB_PACKAGE/etc/ems-bus
done


echo "Feito!"

