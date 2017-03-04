#!/bin/bash
#
# Autor: Everton de Vargas Agilar
# Data: 08/06/2016
#
# Objetivo: Gerar a release do barramento para facilitar a instalação nas 
#           principais distros Linux. Os seguintes arquivos são gerados:
#				* arquivo ems-bus-x.x.x.tar.gz
#				* arquivo rpm para as principais distros Linux
#				* pasta ems-bus com instalação standalone
#
# Modo de usar: 
#
#    $ ./release_rpm.sh
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

# Pasta rel
WORKING_DIR=$(pwd)
BUILD_DEB_FLAG="true"  # is default
BUILD_RPM_FLAG="false"

# Imprime uma mensagem e termina o script
# Parâmetros:
#  $1  - Mensagem que será impressa 
die () {
    echo $1
    exit 1
}

# ***** Clean ******
clean(){
	cd $WORKING_DIR
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
	
	# Loop pelas pastas de templates dos pacotes
	for SKEL_DEB_PACKAGE in `find ./deb/* -maxdepth 0 -type d`; do
		rm -Rf $SKEL_DEB_PACKAGE/usr/lib/ems-bus
		rm -Rf $SKEL_DEB_PACKAGE/etc/ems-bus
		rm -Rf $SKEL_DEB_PACKAGE/etc/systemd
	done
}


# show help 
help(){
	echo "How to use: ./release.sh --rpm or --deb"
	echo "parameter --deb is default"
}


# enviar o pacote gerado para a pasta releases (se a pasta releases existe)
# $1 = PACKAGE_FILE
# $2 = PACKAGE_NAME
send_build_repo(){
	PACKAGE_FILE=$1
	PACKAGE_NAME=$1
	cd $WORKING_DIR
	if [ -d $WORKING_DIR/../../releases/ ]; then
		rm -rf $WORKING_DIR/../../releases/$VERSION_RELEASE/
		mkdir -p $WORKING_DIR/../../releases/$VERSION_RELEASE/
		cp $PACKAGE_FILE  $WORKING_DIR/../../releases/$VERSION_RELEASE/
		
		# pode usar -y para confirmar o envio para o repositório releases automático
		FAZER_GIT_PUSH=$(echo $1 | tr "-" "\0")  
		
		# pergunta envia para o repositório
		while [[ ! $FAZER_GIT_PUSH =~ [YyNn] ]]; do
			printf "Enviar a nova versão $VERSION_RELEASE para o repositório releases? [Yn]"
			read FAZER_GIT_PUSH
		done

		# envia se confirmou 
		if [[ $FAZER_GIT_PUSH =~ [Yy] ]]; then
			cd $WORKING_DIR/../../releases/
			echo "$VERSION_RELEASE" > setup/current_version
			git add $VERSION_RELEASE/$PACKAGE_NAME >> /dev/null
			git add setup/current_version >> /dev/null
			git commit -am "new release $VERSION_RELEASE" >> /dev/null
			echo "Enviando pacote $PACKAGE_NAME para o repositório releases..."
			git push
		fi
	else
		echo "O pacote $PACKAGE_NAME não será enviado para releases, pois este repositório não existe neste computador."  
	fi
}	
	


# Build relase
# parameter:
#  $1  --rpm or --deb
build(){
	cd $WORKING_DIR

	# Pega a versão dop build do barramneto que está no arquivo src/ems_bus.app.src
	VERSION_RELEASE=$(cat ../src/ems_bus.app.src | sed -rn  's/^.*\{vsn.*([0-9]{1,2}\.[0-9]{1,2}.[0-9]{1,2}).*$/\1/p')
	[ -z "$VERSION_RELEASE" ] && die "Não foi possível obter a versão a ser gerada no rebar.config"
	echo "Aguarde, gerando a release $VERSION_RELEASE do barramento, isso pode demorar um pouco!"


	# ########## Recompila todo projeto antes de gerar a release ########## 
	echo "Recompilando os fontes..."
	cd ..
	rm -f erl_crash.dump
	rm -rf priv/db
	rm -rf priv/log
	#rebar clean 1> /dev/null || die "Falha ao limpar os fontes!"
	#rebar get-deps 1>/dev/null || die "Falha ao obter as dependências!"
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

	# build rpm packages
	if [ "$BUILD_RPM_FLAG" == "true" ]; then

		# ####### Criar os pacotes rpm para cada distro ############

		for SKEL_RPM_PACKAGE in `find ./rpm/* -maxdepth 0 -type d`; do
			echo "Criando pacote rpm para o template $SKEL_RPM_PACKAGE..."

			SKEL_PACKAGE_SOURCES="$SKEL_RPM_PACKAGE/SOURCES"
			VERSION_PACK=$VERSION_RELEASE
			# Atualiza a versão no arquivo SPEC/emsbus.spec
			sed -ri "s/Version: .*$/Version: $VERSION_PACK/"  $SKEL_RPM_PACKAGE/SPECS/emsbus.spec
			RELEASE_PACK=$(grep 'Release:' $SKEL_RPM_PACKAGE/SPECS/emsbus.spec | cut -d":" -f2 | tr " " "\0")
			PACKAGE_NAME=ems-bus-$VERSION_RELEASE-$RELEASE_PACK.x86_64.rpm
			PACKAGE_FILE=$SKEL_RPM_PACKAGE/RPMS/x86_64/$PACKAGE_NAME
			
			# Cria a pasta onde vão ser colocados os sources 
			mkdir -p $SKEL_PACKAGE_SOURCES || die "Não foi possível criar a pasta $SKEL_PACKAGE_SOURCES!"

			# Gera a estrutura /usr/lib/ems-bus
			rm -Rf $SKEL_PACKAGE_SOURCES/usr/lib/ems-bus || die "Não foi possível remover pasta $SKEL_RPM_PACKAGE/usr/lib/ems-bus!" 
			mkdir -p $SKEL_PACKAGE_SOURCES/usr/lib
			cp -R ems-bus $SKEL_PACKAGE_SOURCES/usr/lib/ems-bus || die "Não foi possível copiar pasta ems-bus para $SKEL_RPM_PACKAGE/usr/lib!"

			rm -Rf $SKEL_PACKAGE_SOURCES/etc || die "Não foi possível remover pasta $SKEL_RPM_PACKAGE/etc!" 

			# Gera a estrutura /etc/ems-bus
			mkdir -p $SKEL_PACKAGE_SOURCES/etc/ems-bus || die "Não foi possível criar a pasta $SKEL_RPM_PACKAGE/etc/ems-bus!" 
			ln -s /usr/lib/ems-bus/priv/catalog $SKEL_PACKAGE_SOURCES/etc/ems-bus/catalog
			ln -s /usr/lib/ems-bus/priv/conf $SKEL_PACKAGE_SOURCES/etc/ems-bus/conf
			ln -s /usr/lib/ems-bus/priv/csv $SKEL_PACKAGE_SOURCES/etc/ems-bus/csv
			ln -s /usr/lib/ems-bus/priv/ssl $SKEL_PACKAGE_SOURCES/etc/ems-bus/ssl
			ln -s /usr/lib/ems-bus/priv/schema $SKEL_PACKAGE_SOURCES/etc/ems-bus/schema
			ln -s /usr/lib/ems-bus/priv/systemd $SKEL_PACKAGE_SOURCES/etc/ems-bus/systemd
			ln -s /usr/lib/ems-bus/priv/firewalld $SKEL_PACKAGE_SOURCES/etc/ems-bus/firewalld
			
			# Gera a estrutura /etc/systemd/system
			mkdir -p $SKEL_PACKAGE_SOURCES/etc/systemd/system
			mkdir -p $SKEL_PACKAGE_SOURCES/etc/systemd/system/ems-bus.service.d
			ln -s /usr/lib/ems-bus/priv/systemd/ems-bus.service $SKEL_PACKAGE_SOURCES/etc/systemd/system/ems-bus.service || die "Não foi possível criar o link simbólico $SKEL_RPM_PACKAGE/etc/systemd/system/ems-bus.service!" 
			ln -s /usr/lib/ems-bus/priv/systemd/ems-bus.service.d/limits.conf $SKEL_PACKAGE_SOURCES/etc/systemd/system/ems-bus.service.d/limits.conf || die "Não foi possível criar o link simbólico $SKEL_RPM_PACKAGE/etc/systemd/system/ems-bus.service.d/limits.conf!" 

			# Gera a estrutura /etc/firewalld
			mkdir -p $SKEL_PACKAGE_SOURCES/etc/firewalld/services
			ln -s /usr/lib/ems-bus/priv/firewalld/ems-bus.xml $SKEL_PACKAGE_SOURCES/etc/firewalld/services/ems-bus.xml || die "Não foi possível criar o link simbólico $SKEL_RPM_PACKAGE/etc/firewalld/services/ems-bus.xml!" 

			# Gera a estrutura /etc/sudoers.d
			mkdir -p $SKEL_PACKAGE_SOURCES/etc/sudoers.d
			ln -s /usr/lib/ems-bus/priv/sudoers.d/ems-bus.sudoers $SKEL_PACKAGE_SOURCES/etc/sudoers.d/ems-bus.sudoers || die "Não foi possível criar o link simbólico $SKEL_RPM_PACKAGE/etc/sudoers.d/ems-bus!" 

			# Log -> /var/log/ems-bus
			ln -s /var/log/ems-bus $SKEL_PACKAGE_SOURCES/usr/lib/ems-bus/priv/log

			#echo "Generate $SKEL_PACKAGE_SOURCES/ems-bus-$VERSION_PACK.tar.gz from $SKEL_PACKAGE_SOURCES"
			tar -czvf  ems-bus-$VERSION_PACK.tar.gz *
			
			echo "rpm build with rpmbuild..."
			cd $SKEL_RPM_PACKAGE
			rpmbuild -bb SPECS/emsbus.spec

			send_build_repo
		done
		
	# build deb packages	
	elif [ "$BUILD_DEB_FLAG" == "true" ]; then
		
		# ####### Criar os pacotes deb para cada distro ############

		for SKEL_DEB_PACKAGE in `find ./deb/* -maxdepth 0 -type d`; do
			echo "Criando pacote deb para o template $SKEL_DEB_PACKAGE..."
			
			VERSION_PACK=$VERSION_RELEASE
			DEB_CONTROL_FILE=$SKEL_DEB_PACKAGE/DEBIAN/control
			# Atualiza a versão no arquivo SPEC/emsbus.spec
			sed -ri "s/Version: .{6}(.*$)/Version: $VERSION_RELEASE\1/" $DEB_CONTROL_FILE
			RELEASE_PACK=$(grep 'Version' $DEB_CONTROL_FILE | cut -d'-' -f2-)	
			PACKAGE_NAME=ems-bus-$VERSION_RELEASE-$RELEASE_PACK.x86_64.rpm
			PACKAGE_FILE=$SKEL_RPM_PACKAGE/RPMS/x86_64/$PACKAGE_NAME
			
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
			
			# Copia os scripts padrão para o pacote
			cp -f deb/postinst $SKEL_DEB_PACKAGE/DEBIAN
			cp -f deb/postrm $SKEL_DEB_PACKAGE/DEBIAN
			cp -f deb/preinst $SKEL_DEB_PACKAGE/DEBIAN
			cp -f deb/prerm $SKEL_DEB_PACKAGE/DEBIAN
			
			dpkg-deb -b $SKEL_DEB_PACKAGE deb || die "Falha ao gerar o pacote $SKEL_DEB_PACKAGE com dpkg-deb!"

			#send_build_repo
		done
	fi
}


# *************** main ***************

# check --help parameter
if [ "$1" = "--help" ]; then
	help
	exit 0
fi

# check if build deb
if [[ "$1" =~ (-{1,2}deb|deb) ]] || [[ "$2" =~ (-{1,2}deb|deb) ]]; then
	BUILD_DEB_FLAG="true"
fi

# check if build rpm
if [[ "$1" =~ (-{1,2}rpm|rpm) ]] || [[ "$2" =~ (-{1,2}rpm|rpm) ]]; then
	BUILD_RPM_FLAG="true"
fi

# check remove link to fix Unable to generate spec: read file info
if [ -L /usr/lib/erlang/man ]; then
	echo "Preciso de permissão para remover o link /usr/lib/erlang/man (fix Unable to generate spec: read file info)"
	sudo rm  /usr/lib/erlang/man
fi	

clean
build
#clean
cd $WORKING_DIR
echo "Feito!"

