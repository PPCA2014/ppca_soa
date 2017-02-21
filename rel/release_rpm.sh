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

# Imprime uma mensagem e termina o script
# Parâmetros:
#  $1  - Mensagem que será impressa 
die () {
    echo $1
    exit 1
}

# ***** Clean e preparação para build ##########*
clean(){
	echo "Clean..."
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
}



# Pega a versão dop build do barramneto que está no arquivo src/ems_bus.app.src
VERSION_RELEASE=$(cat ../src/ems_bus.app.src | sed -rn  's/^.*\{vsn.*([0-9]{1,2}\.[0-9]{1,2}.[0-9]{1,2}).*$/\1/p')
[ -z "$VERSION_RELEASE" ] && die "Não foi possível obter a versão a ser gerada no rebar.config"
echo "Aguarde, gerando a release $VERSION_RELEASE do barramento, isso pode demorar um pouco!"

clean


# ########## Recompila todo projeto antes de gerar a release ########## 
echo "Recompilando os fontes..."
cd ..
rm -f erl_crash.dump
rm -rf priv/db
rm -rf priv/log
rebar clean 1> /dev/null || die "Falha ao limpar os fontes!"
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


# ####### Criar os pacotes rpm para cada distro ############

for SKEL_RPM_PACKAGE in `find ./rpm/* -maxdepth 0 -type d`; do
	echo "Criando pacote rpm para o template $SKEL_RPM_PACKAGE..."

	SKEL_RPM_PACKAGE_SOURCES="$SKEL_RPM_PACKAGE/SOURCES"
	RPM_VERSION_PACK=$VERSION_RELEASE
	# Atualiza a versão no arquivo SPEC/emsbus.spec
	sed -ri "s/Version: .*$/Version: $RPM_VERSION_PACK/"  $SKEL_RPM_PACKAGE/SPECS/emsbus.spec
	RPM_RELEASE_PACK=$(grep 'Release:' $SKEL_RPM_PACKAGE/SPECS/emsbus.spec | cut -d":" -f2 | tr " " "\0")
	RPM_PACKAGE=ems-bus-$VERSION_RELEASE-$RPM_RELEASE_PACK.x86_64.rpm
	RPM_PACKAGE_FILE=$SKEL_RPM_PACKAGE/RPMS/x86_64/$RPM_PACKAGE
	echo $RPM_PACKAGE_FILE
	
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
	ln -s /usr/lib/ems-bus/priv/firewalld $SKEL_RPM_PACKAGE_SOURCES/etc/ems-bus/firewalld
	
	# Gera a estrutura /etc/systemd/system
	mkdir -p $SKEL_RPM_PACKAGE_SOURCES/etc/systemd/system
	mkdir -p $SKEL_RPM_PACKAGE_SOURCES/etc/systemd/system/ems-bus.service.d
	ln -s /usr/lib/ems-bus/priv/systemd/ems-bus.service $SKEL_RPM_PACKAGE_SOURCES/etc/systemd/system/ems-bus.service || die "Não foi possível criar o link simbólico $SKEL_RPM_PACKAGE/etc/systemd/system/ems-bus.service!" 
	ln -s /usr/lib/ems-bus/priv/systemd/ems-bus.service.d/limits.conf $SKEL_RPM_PACKAGE_SOURCES/etc/systemd/system/ems-bus.service.d/limits.conf || die "Não foi possível criar o link simbólico $SKEL_RPM_PACKAGE/etc/systemd/system/ems-bus.service.d/limits.conf!" 

	# Gera a estrutura /etc/firewalld
	mkdir -p $SKEL_RPM_PACKAGE_SOURCES/etc/firewalld/services
	ln -s /usr/lib/ems-bus/priv/firewalld/ems-bus.xml $SKEL_RPM_PACKAGE_SOURCES/etc/firewalld/services/ems-bus.xml || die "Não foi possível criar o link simbólico $SKEL_RPM_PACKAGE/etc/firewalld/services/ems-bus.xml!" 

	# Gera a estrutura /etc/sudoers.d
	mkdir -p $SKEL_RPM_PACKAGE_SOURCES/etc/sudoers.d
	ln -s /usr/lib/ems-bus/priv/sudoers.d/ems-bus.sudoers $SKEL_RPM_PACKAGE_SOURCES/etc/sudoers.d/ems-bus.sudoers || die "Não foi possível criar o link simbólico $SKEL_RPM_PACKAGE/etc/sudoers.d/ems-bus!" 

	# Log -> /var/log/ems-bus
	ln -s /var/log/ems-bus $SKEL_RPM_PACKAGE_SOURCES/usr/lib/ems-bus/priv/log

	#echo "Generate $SKEL_RPM_PACKAGE_SOURCES/ems-bus-$RPM_VERSION_PACK.tar.gz from $SKEL_RPM_PACKAGE_SOURCES"
	tar -czvf  ems-bus-$RPM_VERSION_PACK.tar.gz *
	
	echo "rpm build with rpmbuild..."
	cd $SKEL_RPM_PACKAGE
	rpmbuild -bb SPECS/emsbus.spec

	# enviar o pacote gerado para a pasta releases (se a pasta releases existe)
	cd $WORKING_DIR
	if [ -d $WORKING_DIR/../../releases/ ]; then
		rm -rf $WORKING_DIR/../../releases/$VERSION_RELEASE/
		mkdir -p $WORKING_DIR/../../releases/$VERSION_RELEASE/
		cp $RPM_PACKAGE_FILE  $WORKING_DIR/../../releases/$VERSION_RELEASE/
		
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
			git add $VERSION_RELEASE/$RPM_PACKAGE >> /dev/null
			git add setup/current_version >> /dev/null
			git commit -am "new release $VERSION_RELEASE" >> /dev/null
			echo "Enviando pacote $RPM_PACKAGE para o repositório releases..."
			git push
		fi
	else
		echo "O pacote $RPM_PACKAGE não será enviado para releases, pois este repositório não existe neste computador."  
	fi

done

clean
cd $WORKING_DIR
echo "Feito!"

