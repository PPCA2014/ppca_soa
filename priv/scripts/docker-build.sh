#!/bin/bash
#
# Author: Everton de Vargas Agilar
# Date: 28/07/2017
#
# Goal: Build Docker image of project and push do docker registry
#
#
#
## Software modification history:
#
# Data       |  Quem           |  Mensagem  
# -----------------------------------------------------------------------------------------------------
# 28/06/2017  Everton Agilar     Initial release
#
#
#
#
#
########################################################################################################

clear

CURRENT_DIR=$(pwd)
VERSION_SCRIPT="1.0.0"


# Imprime na tela a ajuda do comando
help() {
	echo "Build erlang docker image frontend (Version $VERSION_SCRIPT)"
	echo "how to use: sudo ./build.sh"
	echo ""
	echo "Additional parameters:"
	echo "  --app                        -> name of docker app"
	echo "  --tag                        -> Build specific gitlab tag version of project. The default is the lastest tag"
	echo "  --base_url_git_project       -> base url of gitlab. The default is http://servicosssi.unb.br/ssi"
	echo "  --app_url_git                -> project url to build. The default is http://servicosssi.unb.br/ssi/[project_name]_frontend.git"
	echo "  --registry                   -> registry server"
	echo "  --skip_build                 -> skip build. Default is false"
	echo "  --skip_push                  -> skip push registry. Default is true"
	echo "  --skip_check                 -> skip check requirements. Default is false"
	echo "  --npm_version                -> check npm version to this"
	echo "  --node_version               -> check node version to this"
	echo "  --docker_version             -> check docker version to this"
	echo "  --git_user                   -> git user"
	echo "  --git_passwd                 -> git passwd"
	echo "  --cache_node_modules         -> cache node_modules for speed"
	echo "  --keep_stage                 -> does not delete stage area after build"
	echo "  --push                       -> push to registry. The same as --skip_push=false"
	echo "  --mode_build                 -> build npm project in mode pass in variable"
	echo "  --http_port                  -> change port http frontend. Default is 3000"
	echo "  --https_port                 -> change port https frontend. Default is 4000"
	echo
	echo "Obs.: Use only com root or sudo!"
	cd $CURRENT_DIR
	exit 1
}



# Imprime uma mensagem e termina o sistema
# Parâmetros:
#  $1  - Mensagem que será impressa 
#  $2  - Código de Return para o comando exit
die () {
    echo $1
    exit $2
}



# Não precisa ser root para pedir ajuda
if [ "$1" = "--help" ]; then
	help
fi

# Make sure only root can run our script
if [[ $EUID -ne 0 ]]; then
   echo "Only the root user can build docker images" 1>&2
   exit 1
fi


# Versões do npm e node necessárias. 
# Será verificado se as versões instaladas estão igual ou maiores do que as definidas aqui
NPM_VERSION="4.2.0"
NODE_VERSION="v7.10.0"
DOCKER_VERSION="17.03.2"


# Identify the linux distribution: ubuntu, debian, centos
LINUX_DISTRO=$(awk -F"=" '{ if ($1 == "ID"){ 
								gsub("\"", "", $2);  print $2 
							} 
						  }' /etc/os-release)

# Get linux description
LINUX_DESCRIPTION=$(awk -F"=" '{ if ($1 == "PRETTY_NAME"){ 
									gsub("\"", "", $2);  print $2 
								 } 
							   }'  /etc/os-release)


LINUX_VERSION_ID=$(awk -F"=" '{ if ($1 == "VERSION_ID"){ 
									gsub("\"", "", $2);  print $2 
								 } 
							   }'  /etc/os-release)


# As configurações podem estar armazenadas no diretório /etc/default/erlangms-docker
CONFIG_ARQ="/etc/default/erlangms-docker"


# O nome do projeto é o nome do próprio projeto docker mas sem o sufíxo .docker
APP_NAME=

# Github repository ERLANGMS release: onde está o setup do barramento
ERLANGMS_RELEASE_URL="https://github.com/erlangms/releases/raw/master"

# Onde está o template docker utilizado por este build
ERLANGMS_DOCKER_GIT_URL="https://github.com/erlangMS/docker"

# variável opcional para dizer qual modo de build da aplicação
# deixa em branço para pedir na execução do build
MODE_BUILD=""


# Registry server daemon to catalog images
REGISTRY_IP="127.0.0.1"
REGISTRY_PORT="5000"
REGISTRY_SERVER="$REGISTRY_IP:$REGISTRY_PORT"

# Flag para controle do que vai ser feito
SKIP_BUILD="false"
SKIP_PUSH="true"
SKIP_CHECK="false"

# Git credentials
GIT_USER="erlangms"
GIT_PASSWD=""



# SMTP parameter
SMTP_SERVER="mail.unb.br"
SMTP_PORT=587
SMTP_DE=""
SMTP_PARA=""
SMTP_PASSWD=""
SMTP_RE_CHECK="^[a-z0-9!#\$%&'*+/=?^_\`{|}~-]+(\.[a-z0-9!#$%&'*+/=?^_\`{|}~-]+)*@([a-z0-9]([a-z0-9-]*[a-z0-9])?\.)+[a-z0-9]([a-z0-9-]*[a-z0-9])?\$"


# Quando este flag é true, faz um cache do node_modules para acelerar o build (apenas para testes)
CACHE_NODE_MODULES="false"

# Se este flag for true, após o build a stage área não será removida. Obs.: Para finalidades de debug
KEEP_STAGE="false"

LOG_FILE="$CURRENT_DIR/docker-build.log"


# Lê uma configuração específica do arquivo de configuração. Aceita default se não estiver definido
# Parâmetros
#   $1 -> Nome da configuração. Ex. REGISTRY
#   $2 -> Valor default
le_setting () {
	KEY=$1
	DEFAULT=$2
	# Lê o valor configuração, remove espaços a esquerda e faz o unquoted das aspas duplas
	RESULT=$(egrep -i "^$KEY" $CONFIG_ARQ 2> /dev/null | cut -d"=" -f2 | sed -r 's/^ *//' | sed -r 's/^\"?(\<.*\>\$?)\"?$/\1/')
	if [ -z "$RESULT" ] ; then
		echo $DEFAULT
	else
		echo $RESULT
	fi
}	


# Lê as configurações para execução do arquivo de configuração default /etc/default/erlangms-docker
# Essas confiurações podem ser redefinidas via linha de comando
le_all_settings () {
	printf "Verify if exist conf file $CONFIG_ARQ... "
	if [ -f "$CONFIG_ARQ" ]; then
		printf "OK\n"
		echo "Reading settings from $CONFIG_ARQ..."
		REGISTRY=$(le_setting 'REGISTRY' "$REGISTRY_SERVER")
		GIT_USER=$(le_setting 'GIT_USER' '""')
		GIT_PASSWD=$(le_setting 'GIT_PASSWD' '""')
		ERLANGMS_RELEASE_URL=$(le_setting 'ERLANGMS_RELEASE_URL' "$ERLANGMS_RELEASE_URL")
		GIT_BASE_URL_PROJECTS=$(le_setting 'GIT_BASE_URL_PROJECTS' "$GIT_BASE_URL_PROJECTS")
		NPM_VERSION=$(le_setting 'NPM_VERSION' "$NPM_VERSION")
		NODE_VERSION=$(le_setting 'NODE_VERSION' "$NODE_VERSION")
		DOCKER_VERSION=$(le_setting 'DOCKER_VERSION' "$DOCKER_VERSION")

		
		# E-mail settings
		IMAP_SERVER=$(le_setting 'IMAP_SERVER' "imap.unb.br")
		SMTP_SERVER=$(le_setting 'SMTP_SERVER' "smtp.unb.br")
		SMTP_PORT=$(le_setting 'SMTP_PORT' '587')
		SMTP_LOGIN=$(le_setting 'SMTP_LOGIN')
		SMTP_PASSWD=$(le_setting 'SMTP_PASSWD')
		SMTP_DE=$(le_setting 'SMTP_DE')
		SMTP_PARA=$(echo `le_setting 'SMTP_PARA'` | tr -s ' ' |  sed -r "s/([A-Za-z0-9@\._]+) *[,$]?/'\1',/g; s/,$//")
	else
		printf "NO\n"
	fi
}


# Function to send email
# Parameters:
#   $1  - title
#   $2  - subject
send_email () {
    TITULO_MSG=$1
    SUBJECT=$2
    python <<EOF
# -*- coding: utf-8 -*-
import smtplib
from email.mime.text import MIMEText
from email.Utils import formatdate
try:
	smtp = smtplib.SMTP("$SMTP_SERVER", $SMTP_PORT)
	smtp.starttls()
	smtp.login("$SMTP_DE", "$SMTP_PASSWD")
	msg = MIMEText("""$SUBJECT""")
	msg['Subject'] = "$TITULO_MSG"
	msg['From'] = "$SMTP_DE"
	msg['To'] = "$SMTP_PARA"
	msg['Date'] = formatdate(localtime=True)
	msg['Content-Type'] = 'text/plain; charset=utf-8'
	smtp.sendmail("$SMTP_DE", ["$SMTP_PARA"], msg.as_string())
	smtp.quit()
	exit(0)
except Exception as e:
	print(e)
	exit(1)
EOF
}


# Instala os componentes necessários para o build
install_required_libs(){
	# Indicates whether it will be necessary to update the repository
	UPDATE_NECESSARY="false"

	# **** Install required packages to build images ****
	
	if [ "$INSTALL_REQUIRED_PCK" == "true" ]; then
		REQUIRED_PCK=""
		INSTALL_REQUIRED_PCK="false"
		for PCK in $REQUIRED_PCK; do 
			if ! dpkg -s $PCK > /dev/null 2>&1 ; then
				INSTALL_REQUIRED_PCK="true"
				break
			fi
		done
		echo "Installing required packages $REQUIRED_PCK..."
		apt-get -y install $REQUIRED_PCK > /dev/null 2>&1
	else
		echo "Skipping required packages to build docker images because it is already installed."
	fi
}


# setup project
prepare_project_to_build(){
	echo "Clone project $APP_URL_GIT..."

	cd build
	echo "Git clone $APP_URL_GIT $APP_NAME"
	if ! git clone $APP_URL_GIT $APP_NAME 2> /dev/null ; then
		die "Could not access project repository $APP_URL_GIT. Check your network connection or password!"
	fi
	 
	cd $APP_NAME
	
	# Faz build da última tag gerada se não foi informado o parâmetro --tag
	if [ -z $GIT_CHECKOUT_TAG ]; then
		GIT_CHECKOUT_TAG=$(git tag -l --sort=-creatordate | sed '1!d')
	fi
	
	echo "Git checkout -b $GIT_CHECKOUT_TAG"
	git checkout -b $GIT_CHECKOUT_TAG
	echo "Return git checkout -b $GIT_CHECKOUT_TAG: $?"
	
	# Get expose http and https ports from Dockerfile
	if [ -z "$HTTP_PORT" ]; then
		HTTP_PORT=$(grep ems_http_server.tcp_port emsbus.conf  | sed -r 's/[^0-9]//g')
	fi
	
	if [ -z "$HTTPS_PORT" ]; then
		HTTPS_PORT=$(grep ems_https_server.tcp_port emsbus.conf | sed -r 's/[^0-9]//g')
	fi

	[ -z "$HTTP_PORT" ] && die "HTTP port of project not informed, build canceled. Enter the parameter --http_port!"
	[ -z "$HTTPS_PORT" ] && die "HTTPS port of project not informed, build canceled. Enter the parameter --https_port!"

	# Atualiza o arquivo Dockerfile com as portas expostas
	sed -i "s/{{ HTTP_PORT }}/$HTTP_PORT/"  ../../Dockerfile
	sed -i "s/{{ HTTPS_PORT }}/$HTTPS_PORT/"  ../../Dockerfile
	
	# Atualiza o arquivo docker-compose.yml
	sed -i "s/{{ HTTP_PORT }}/$HTTP_PORT/g"  ../../docker-compose.yml
	sed -i "s/{{ HTTPS_PORT }}/$HTTPS_PORT/g"  ../../docker-compose.yml
	sed -i "s/{{ APP_NAME }}/$APP_NAME/g"  ../../docker-compose.yml
	sed -i "s/{{ APP_NAME }}/$APP_NAME/g"  ../../docker-compose.yml
	
	# Cria o arquivo emsbus.conf para a pasta conf do docker template
	mkdir -p ../../conf/
	cp emsbus.conf ../../conf/
	
}


# Performs the installation of the ems-bus
build_image(){
	
	# ***** npm install *****

	# Só executado quando há o arquivo package.json
	if [ -f package.json ]; then
		# Quando o flag CACHE_NODE_MODULES for true, vamos usar uma pasta de cache para node_modules e 
		# criar um hard link. Isso vai acelerar e muito!!! 
		if [ "$CACHE_NODE_MODULES" = "true" ]; then
			echo "node_modules cache enabled"
			NODE_MODULES_CACHE_PATH="/tmp/erlangms/build/node_modules"
			if [ -d NODE_MODULES_CACHE_PATH ]; then
				echo "Let go make drink, this will take time!!!"
				ln -s $NODE_MODULES_CACHE_PATH node_modules
			else
				echo "Let go make chimarrão, this will take time!!!"
				mkdir -p $NODE_MODULES_CACHE_PATH
				ln -s $NODE_MODULES_CACHE_PATH node_modules
			fi
		else
			echo "Let go make coffee, this will take time!!!"
		fi
		
		echo "Node project detected..."
		echo "npm install..."
		npm install
		echo "Return npm install: $?"
		if [ "$?" != "0" ]; then
			die "An error occurred in the npm install command. Build canceled."
		fi


		# ***** npm run build *****
		npm run build:$MODE_BUILD
		echo "Return npm build:$MODE_BUILD $?"
		if [ "$?" != "0" ]; then
			die "An error occurred in the npm run build command. Build canceled."
		fi

		echo "Copy sources files to ../../app/$APP_NAME..."
		mv dist/ ../../app/$APP_NAME/
		cd ../../
	else
		echo "Copy sources files to ../../app/$APP_NAME..."
		cd ..
		mv $APP_NAME/ ../app/$APP_NAME/
		cd ..
	fi


	# ***** Build docker image *****

	echo "Preparing for build docker image to app $APP_NAME, please wait..."

	# Format app version do docker
	APP_VERSION=$(echo "$GIT_CHECKOUT_TAG" | sed -r 's/[^0-9.]+//g')

	# Nome da imagem no docker sem o sufixo latest
	APP_DOCKER_FILENAME=$APP_NAME:$APP_VERSION

	# Nome da imagem no docker com sufixo latest
	APP_DOCKER_LATEST=$APP_NAME:latest


	

	echo "Build image $APP_DOCKER_LATEST"
	docker swarm leave --force

	echo "Stop image $APP_DOCKER_FILENAME..."
	docker stop $(docker images 2> /dev/null | grep "$APP_DOCKER_FILENAME" | tr -s ' ' '|' | cut -d'|' -f3)
	docker stop $(docker images 2> /dev/null | grep "$APP_DOCKER_LATEST" | tr -s ' ' '|' | cut -d'|' -f3)

	# Por segurança melhor apagar as imagens anteriores
	echo "Remove previous build images de $APP_DOCKER_FILENAME..."
	docker rmi --force $(docker images 2> /dev/null | grep "$APP_DOCKER_FILENAME" | tr -s ' ' '|' | cut -d'|' -f3)
	docker rmi --force $(docker images 2> /dev/null | grep "$APP_DOCKER_LATEST" | tr -s ' ' '|' | cut -d'|' -f3)

	# build docker image $APP_NAME:$APP_VERSION
	echo "docker build . -t $APP_DOCKER_FILENAME"
	docker build . -t $APP_DOCKER_FILENAME
	
	# Add tag $APP_DOCKER_LATEST
	echo "docker tag $APP_DOCKER_FILENAME $APP_DOCKER_LATEST"
	docker tag $APP_DOCKER_FILENAME $APP_DOCKER_LATEST
	
	
	# create stack of services
	echo "docker swarm init"
	docker swarm init


	# Create network:
	echo "docker network create -d overlay $APP_NAME"
	docker network create -d overlay $APP_NAME
	
	echo "docker stack deploy -c docker-compose.yml erlangms"
	docker stack deploy -c docker-compose.yml erlangms
	
	# remove old tar
	rm -f $APP_DOCKER_LATEST.tar

	# save image
	echo "docker save $APP_DOCKER_LATEST -o $APP_DOCKER_LATEST.tar"
	docker save $APP_DOCKER_LATEST -o $APP_DOCKER_LATEST.tar
	
	cp $APP_DOCKER_LATEST.tar $CURRENT_DIR/$APP_DOCKER_LATEST.tar
}


# check send email
check_send_email(){
	# Ask if you want to send log by email
	while [[ ! $ENVIA_LOG_EMAIL =~ [YyNn] ]]; do
		printf "You want to send the build log via email? [Yn]"
		read ENVIA_LOG_EMAIL
	done

	echo ""

	# send log by e-mail
	if [[ $ENVIA_LOG_EMAIL =~ [Yy] ]]; then
		EMAIL_OK="false"
		until [ $EMAIL_OK = "true" ]; do
			printf "Enter your e-mail: "
			read SMTP_DE
			if [[ $SMTP_DE =~ $SMTP_RE_CHECK ]]; then
				EMAIL_OK="true"
			else
				echo "E-mail $SMTP_DE is invalid"
			fi
		done
		SMTP_PARA=$SMTP_DE
		printf "Enter your password: "
		read -s SMTP_PASSWD
		echo ""
		echo "Send email, please wait..."
		TextLog=$(cat $LOG_FILE)
		send_email "Build image log on server $LINUX_DESCRIPTION << IP $LINUX_IP_SERVER >>" "$TextLog" && echo "Log sent by email to $SMTP_PARA."
	fi
}


# Verifica se a versão do npm instalado é compatível com este script de build
check_npm_version(){
	printf "Checking installed npm version... "
	npm --version > /dev/null || die "O npm não está instalado, build cancelado!"
	NPM_VERSION_OS=$(npm --version)
	NPM_VERSION2=$(echo $NPM_VERSION | sed -r 's/[^0-9]+//g')
	NPM_VERSION_OS=$(echo $NPM_VERSION_OS | sed -r 's/[^0-9]+//g')
	if [ "$NPM_VERSION_OS" -ge "$NPM_VERSION2" ]; then
		printf "OK\n"
	else
		printf "ERROR\n"
		die "Build canceled because the npm installed is incompatible with this software. Expected version: $NPM_VERSION"
	fi 
}


# Verifica se o node instalado é compatível com este script de build
check_node_version(){
	printf "Checking installed node version ... "
	node --version > /dev/null || die "O node não está instalado, build cancelado!"
	NODE_VERSION_OS=$(node --version)
	NODE_VERSION2=$(echo $NODE_VERSION | sed -r 's/[^0-9]+//g')
	NODE_VERSION_OS=$(echo $NODE_VERSION_OS | sed -r 's/[^0-9]+//g')
	if [ "$NODE_VERSION_OS" -ge "$NODE_VERSION2" ]; then
		printf "OK\n"
	else
		printf "ERROR\n"
		die "Build canceled because the installed node is incompatible with this software. Expected version: $NODE_VERSION"
	fi 
}

# Verifica se a versão do docker instalado é compatível com este script
check_docker_version(){
	printf "Checking installed docker version... "
	docker --version > /dev/null || die "Docker is not installed, start canceled!"
	DOCKER_VERSION_OS=$(docker --version)
	DOCKER_VERSION2=$(echo $DOCKER_VERSION | sed -r 's/[^0-9]+//g')
	DOCKER_VERSION_OS=$(echo $DOCKER_VERSION_OS | sed -r 's/[^0-9]+//g')
	if [ "$DOCKER_VERSION_OS" -ge "$DOCKER_VERSION2" ]; then
		printf "OK\n"
	else
		printf "ERROR\n"
		die "Build canceled because the docker installed is incompatible with this software. Expected version: $DOCKER_VERSION"
	fi 
}


# Faz push da imagem do docker para o servidor registry
# Para fazer push das imagens para um servidor Registry é preciso
# que o computador onde está sendo feito o build tenha o 
# arquivo de configuração /etc/docker/daemon.json
# para liberar conexões HTTP inseguras
push_registry(){
	if docker info > /dev/null 2>&1 ; then
		if nc -z $REGISTRY_IP $REGISTRY_PORT ; then
			
			# Cria /etc/docker/daemon.json SOMENTE se não existe!
			if [ ! -f /etc/docker/daemon.json ]; then
				echo "/etc/docker/daemon.json does not exist, creating it..."
				echo "{ \"insecure-registries\": [\"$REGISTRY_IP:$REGISTRY_PORT\"] }" > /etc/docker/daemon.json
				echo "Restart systemctl docker.service daemon after creating /etc/docker/daemon.json..."
				systemctl restart docker > /dev/null 2>&1
			fi
			
			# É necessário criar uma tag para enviar para o registry
			PUSH_TAG="$REGISTRY_SERVER/$APP_NAME"

			echo "Tag the image $APP_NAME so that it points to your registry $PUSH_TAG"
			docker tag $APP_NAME $PUSH_TAG
			
			echo "Push $PUSH_TAG to $REGISTRY_SERVER"
			docker push $REGISTRY_SERVER/$APP_NAME
		else
			echo "Registry server daemon $REGISTRY_SERVER is out, you will not be able to push docker image $APP_DOCKER_LATEST.tar now"
		fi
	else
		echo "Docker on the client must be running to push the image to the registry!"
	fi
}


# IMPORTANTE
# Stage área é onde o build é realizado, um local temporário onde arquivos são criados e modificados. 
# Depois do processo de build, esta área é por default eliminada.
# Criar a área stage: Envia todos os arquivos do build necessários para lá.
# O build não altera nenhum arquivo do projeto pois tudo é realizado na stage.
make_stage_area(){
	STAGE_AREA=/tmp/erlangms/docker/build_$$/
	mkdir -p $STAGE_AREA
	cd $STAGE_AREA
	echo "Stage area $STAGE_AREA..."
	if ! git clone "$ERLANGMS_DOCKER_GIT_URL" docker ; then
		die "Could not access erlangms docker template $ERLANGMS_DOCKER_GIT_URL. Check your network or internet connection!"
	fi
	cd docker
	mkdir -p app
	mkdir -p build
}

######################################## main ########################################


install_required_libs
le_all_settings


# Command line parameters
for P in $*; do
	# Permite informar a tag no gitlab para gerar a imagem. 
	# Se não informar, busca a última tag
	if [[ "$P" =~ ^--app_?(name)?=.+$ ]]; then
		APP_NAME="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--tag=.+$ ]]; then
		GIT_CHECKOUT_TAG="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--npm_version=.+$ ]]; then
		NPM_VERSION="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--node_version=.+$ ]]; then
		NODE_VERSION="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--docker_version=.+$ ]]; then
		DOCKER_VERSION="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--app_url_git=.+$ ]]; then
		APP_URL_GIT="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--registry=.+$ ]]; then
		REGISTRY="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--skip_build$ ]]; then
		SKIP_BUILD="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--skip_push$ ]]; then
		SKIP_PUSH="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--skip_check$ ]]; then
		SKIP_CHECK="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--git_user=.+$ ]]; then
		GIT_USER="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--git_passwd=.+$ ]]; then
		GIT_PASSWD="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--base_url_git_projects=.+$ ]]; then
		GIT_BASE_URL_PROJECTS="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--mode_build=.+$ ]]; then
		MODE_BUILD="$(echo $P | cut -d= -f2)"
	elif [ "$P" = "--cache_node_modules" ]; then
		CACHE_NODE_MODULES="true"
	elif [ "$P" = "--keep_stage" ]; then
		KEEP_STAGE="true"
	elif [[ "$P" =~ ^--http_port=.+$ ]]; then
		HTTP_PORT="$(echo $P | cut -d= -f2)"
	elif [[ "$P" =~ ^--https_port=.+$ ]]; then
		HTTPS_PORT="$(echo $P | cut -d= -f2)"
	elif [ "$P" = "--push" ]; then
		SKIP_PUSH="false"
	elif [[ "$P" =~ ^--help$ ]]; then
		help
	fi
done


[ -z "$APP_NAME" ] && die "Name of project not informed, build canceled. Enter the parameter --app!"


# APP_URL_GIT setting
if [ -z "$APP_URL_GIT" ]; then
	APP_URL_GIT=$GIT_BASE_URL_PROJECTS/$APP_NAME.git
else
	GIT_BASE_URL_PROJECTS=$(dirname "$APP_URL_GIT")
fi

[ -z "$APP_URL_GIT" ] && die "Project url not informed, build canceled. Enter the parameter --app_url_git!"



# Registry settings
if [ ! -z "$REGISTRY" ]; then
	if [[ "$REGISTRY" =~ ^[0-9a-zA-Z_.]+:[0-9]+$ ]] ; then
	   REGISTRY_PORT=$(echo $REGISTRY | awk -F: '{ print $2; }')
	   REGISTRY_SERVER=$REGISTRY
	elif [[ $REGISTRY =~ ^[0-9a-zA-Z_-.]+$ ]] ; then
		REGISTRY_SERVER=$REGISTRY:$REGISTRY_PORT
	else
		die "Parameter --registry $REGISTRY is invalid. Example: 127.0.0.1:5000"
	fi
	REGISTRY_IP="$(echo $REGISTRY_SERVER | cut -d: -f1)"
else
	die "Parameter --registry is required. Example: 127.0.0.1:5000"
fi


if [ -z "$HTTP_PORT" ]; then
	HTTP_PORT=$(le_setting "$APP_NAME.HTTP_PORT" "")
fi

if [ -z "$HTTPS_PORT" ]; then
	HTTPS_PORT=$(le_setting "$APP_NAME.HTTPS_PORT" "")
fi


make_stage_area


# Enables installation logging
exec > >(tee -a ${LOG_FILE} )
exec 2> >(tee -a ${LOG_FILE} >&2)


echo "Start build of erlangms frontend images ( Date: $(date '+%d/%m/%Y %H:%M:%S') )"


if [ "$SKIP_CHECK" = "false" ]; then
	check_npm_version
	check_node_version
	check_docker_version
else
	echo "Skip check requirements enabled..."	
fi

if [ -z "$GIT_CHECKOUT_TAG" ]; then
	echo "Frontend version: latest"
else
	echo "Frontend version: $GIT_CHECKOUT_TAG"
fi

# Lê o modo de build
while [[ ! $MODE_BUILD =~ (dev|prod) ]]; do
	printf 'Build mode (dev|prod): '
	read MODE_BUILD
done


prepare_project_to_build


echo "npm version: $(npm --version)"
echo "node version: $(node --version)"
echo "Registry server: $REGISTRY"
echo "Git base url projects: $GIT_BASE_URL_PROJECTS"
echo "Git project: $APP_URL_GIT"
echo "Git user: $GIT_USER"
echo "Docker expose http port: $HTTP_PORT"
echo "Docker expose https port: $HTTPS_PORT"
echo "Working dir: $STAGE_AREA"
echo "Log file: $LOG_FILE" 
echo "============================================================================================"



if [ "$SKIP_BUILD" = "false" ]; then
	build_image
else
	echo "Skip build image enabled..."
fi

if [ "$SKIP_PUSH" = "false" ]; then
	push_registry
else
	echo "Skip push image enabled..."
fi

#check_send_email
	
# Volta para o diretório do projeto docker
cd $CURRENT_DIR

if [ "$KEEP_STAGE" = "false" ]; then
	rm -rf $STAGE_AREA
else
	echo "Keep stage enabled"
fi

