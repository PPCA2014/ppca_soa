#!/bin/bash
#
# Author: Everton de Vargas Agilar
# Date: 03/07/2017
#
# Goal: Deploy erlangms docker project
#
#
#
## Software modification history:
#
# Data       |  Quem           |  Mensagem  
# -----------------------------------------------------------------------------------------------------
# 28/07/2017  Everton Agilar     Initial release
#
#
#
#
#
########################################################################################################


# Versão do docker necessário
# Será verificado se a versão está igual ou maior que a definida aqui
DOCKER_VERSION="17.03.2"


# Parameters
VERSION_SCRIPT="1.0.1"
WORKING_DIR=$(pwd)

# As configurações podem estar armazenadas no diretório /etc/default/erlangms-docker
CONFIG_ARQ="/etc/default/erlangms-docker"

# HTTP Server
SERVER_ADDR=$(hostname -I | cut -d" " -f1)       
SERVER_HTTP_PORT_LISTENER=
SERVER_HTTPS_PORT_LISTENER=

ENTRYPOINT="ems-bus/bin/ems-bus console"
CLIENT_CONF="/tmp/erlangms_$$_barramento_client.conf"
CLIENT_CONF_IN_MEMORY="true"
APP_VERSION="1.0"
ENVIRONMENT=`hostname`
SKIP_CHECK="false"
IMAGE_ID="latest"

# Erlangms
ERLANGMS_ADDR="127.0.0.1"
ERLANGMS_HTTP_PORT_LISTENER="2301"
ERLANGMS_HTTPS_PORT_LISTENER="2344"
ERLANGMS_BASE_URL=""
ERLANGMS_AUTH_URL=""
ERLANGMS_AUTH_PROTOCOL="oauth2"


# Imprime uma mensagem e termina o sistema
# Parâmetros:
#  $1  - Mensagem que será impressa 
#  $2  - Código de retorno para o comando exit
die () {
    echo $1
    exit $2
}


# Lê uma configuração específica do arquivo de configuração. Aceita default se não estiver definido
# Parâmetros
#   $1 -> Nome da configuração. Ex. REGISTRY
#   $2 -> Valor default
le_setting () {
	KEY=$1
	DEFAULT=$2
	# Lê o valor configuração, remove espaços a esquerda e faz o unquoted das aspas duplas
	RESULT=$(egrep "^$KEY" $CONFIG_ARQ | cut -d"=" -f2 | sed -r 's/^ *//' | sed -r 's/^\"?(\<.*\>\$?)\"?$/\1/')
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
		echo "Reading settings from $CONFIG_ARQ... OK"
		REGISTRY=$(le_setting 'REGISTRY' "$REGISTRY_SERVER")
		ENVIRONMENT=$(le_setting 'ENVIRONMENT' "$ENVIRONMENT")
		SERVER_HTTP_PORT_LISTENER=$(le_setting 'SERVER_HTTP_PORT_LISTENER' "$SERVER_HTTP_PORT_LISTENER")
		SERVER_HTTPS_PORT_LISTENER=$(le_setting 'SERVER_HTTPS_PORT_LISTENER' "$SERVER_HTTPS_PORT_LISTENER")
		ENTRYPOINT=$(le_setting 'ENTRYPOINT' "$ENTRYPOINT")
		CLIENT_CONF=$(le_setting 'CLIENT_CONF' "$CLIENT_CONF")
		DOCKER_VERSION=$(le_setting 'DOCKER_VERSION' "$DOCKER_VERSION")

		# Erlangms settings
		ERLANGMS_ADDR=$(le_setting 'ERLANGMS_ADDR' "$ERLANGMS_ADDR")
		ERLANGMS_HTTP_PORT_LISTENER=$(le_setting 'ERLANGMS_HTTP_PORT' "$ERLANGMS_HTTP_PORT")
		ERLANGMS_HTTPS_PORT_LISTENER=$(le_setting 'ERLANGMS_HTTPS_PORT' "$ERLANGMS_HTTPS_PORT")
		ERLANGMS_AUTH_PROTOCOL=$(le_setting 'ERLANGMS_AUTH_PROTOCOL' "$ERLANGMS_AUTH_PROTOCOL")
		ERLANGMS_BASE_URL=$(le_setting 'ERLANGMS_BASE_URL' "$ERLANGMS_BASE_URL")
		
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


# Verifica se a versão do docker instalado é compatível com este script
check_docker_version(){
	printf "Checking installed docker version... "
	docker --version > /dev/null || die "Docker is not installed, deploy canceled!"
	DOCKER_VERSION_OS=$(docker --version)
	DOCKER_VERSION2=$(echo $DOCKER_VERSION | sed -r 's/[^0-9]+//g')
	DOCKER_VERSION_OS=$(echo $DOCKER_VERSION_OS | sed -r 's/[^0-9]+//g')
	if [ "$DOCKER_VERSION_OS" -ge "$DOCKER_VERSION2" ]; then
		printf "OK\n"
	else
		printf "ERROR\n"
		die "Deploy canceled because the docker installed is incompatible with this software. Expected version: $DOCKER_VERSION"
	fi 
}


# Imprime na tela a ajuda do comando
help() {
	echo "Deploy erlang docker image frontend (Version $VERSION_SCRIPT)"
	echo "by tarfile: ./docker-deploy.sh --tar[file]=image.tar [--app=name]"
	echo "by registry image: ./docker-deploy.sh --image=image [--app=name]"
	echo "by gitlab project: ./docker-deploy.sh"
	echo ""
	echo "Additional parameters:"
	echo "  --app              -> name of docker app"
	echo "  --image              -> name of image app"
	echo "  --entrypoint       -> entry pont of docker image. The default is ems-bus/bin/ems-bus console"
	echo "  --registry         -> registry server"
	echo "  --http_port        -> port of http server listener"
	echo "  --https_port       -> port of https server listener"
	echo "  --environment      -> set optional environment description"
	echo "  --client_conf	     -> set client_conf filename. The default is to generate automatically"
	echo "  --skip_check	     -> skip check requirements"
	echo "  --docker_version   -> check npm version to this"
	echo "  --erlangms_addr    -> ip of erlangms"
	echo "  --erlangms_http_port   ->  port of http erlangms listener"
	echo "  --erlangms_https_port   ->  port of https erlangms listener"
	echo "  --erlangms_auth_protocol    -> authorization protocol to use. The default is oauth2"
	echo "  --erlangms_base_url    -> base url of erlangms"
	echo "  --image_id         -> id of a specific docker image. The default is latest"
	echo
	echo "Obs.: Use only com root or sudo!"
	exit 1
}


# Um volume será criado ao subir a imagem docker que aponta para este arquivo.
# Este arquivo contém configurações para o cliente que é dependente do ambiente.
# Cria este arquivo se o flag CLIENT_CONF_IN_MEMORY for true
# Se o arquivo for informado com --client_conf, então o arquivo não precisa ser gerado
make_conf_file(){
	if [ "$CLIENT_CONF_IN_MEMORY" = "true" ]; then
		echo "{\"ip\":\"$ERLANGMS_ADDR\",\"http_port\":$ERLANGMS_HTTP_PORT_LISTENER,\"https_port\":$ERLANGMS_HTTPS_PORT_LISTENER,\"base_url\":\"$ERLANGMS_BASE_URL\",\"auth_url\":\"$ERLANGMS_AUTH_URL\",\"auth_protocol\":\"$ERLANGMS_AUTH_PROTOCOL\",\"app\":\"$APP_NAME\",\"version\":\"$APP_VERSION\",\"environment\":\"$ENVIRONMENT\",\"docker_version\":\"$DOCKER_VERSION\"}" > $CLIENT_CONF
	fi
}


# Get expose ports from docker image if not defined in parameters
# Labels: HTTP_PORT and HTTPS_PORT
get_expose_ports(){
	if [ -z "$SERVER_HTTP_PORT_LISTENER" ]; then
		SERVER_HTTP_PORT_LISTENER=$( sudo docker inspect $IMAGE | sed -n '/HTTP_PORT/ p' | uniq | sed -r 's/[^0-9]+//g;' )
	fi
	if [ -z "$SERVER_HTTPS_PORT_LISTENER" ]; then
		SERVER_HTTPS_PORT_LISTENER=$( sudo docker inspect $IMAGE | sed -n '/HTTPS_PORT/ p' | uniq | sed -r 's/[^0-9]+//g;' )
	fi
	if [ -z "$SERVER_HTTP_PORT_LISTENER" ]; then
		die "Inform HTTP port of docker image!"
	fi
	if [ -z "$SERVER_HTTPS_PORT_LISTENER" ]; then
		die "Inform HTTPS port of docker image!"
	fi
}


######################################## main ########################################

# Não precisa ser root para pedir ajuda
if [ "$1" = "--help" ]; then
	help
fi

# Make sure only root can run our script
if [[ $EUID -ne 0 ]]; then
   die "Only the root user can build docker images"
fi


# Lê as configurações do arquivo de configuração default /etc/default/erlangms-docker.
le_all_settings


# Read command line parameters
for P in $*; do
	if [[ "$P" =~ ^--.+$ ]]; then
		if [[ "$P" =~ ^--tar_?(file)?=.+$ ]]; then
			TAR_FILE="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--app_?(name)?=.+$ ]]; then
			APP_NAME="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--image=.+$ ]]; then
			IMAGE="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--entrypoint=.+$ ]]; then
			ENTRYPOINT="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--http_port=.+$ ]]; then
			SERVER_HTTP_PORT_LISTENER="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--https_port=.+$ ]]; then
			SERVER_HTTPS_PORT_LISTENER="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--environment=.+$ ]]; then
			ENVIRONMENT="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--skip_check$ ]]; then
			SKIP_CHECK="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--docker_version=.+$ ]]; then
			DOCKER_VERSION="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--registry=.+$ ]]; then
			REGISTRY="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--image_id=.+$ ]]; then
			IMAGE_ID="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--erlangms_addr=.+$ ]]; then
			ERLANGMS_ADDR="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--erlangms_http_port=.+$ ]]; then
			ERLANGMS_HTTP_PORT_LISTENER_LISTENER="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--erlangms_https_port=.+$ ]]; then
			ERLANGMS_HTTPS_PORT_LISTENER_LISTENER="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--erlangms_auth_protocol=.+$ ]]; then
			ERLANGMS_AUTH_PROTOCOL="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--erlangms_base_url=.+$ ]]; then
			ERLANGMS_BASE_URL="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--client_conf=.+$ ]]; then
			CLIENT_CONF="$(echo $P | cut -d= -f2)"
			if [ ! -f "$CLIENT_CONF" ]; then
				echo "Client conf does not exist, auto generate..."
			else
				CLIENT_CONF_IN_MEMORY="false"
			fi
		elif [[ "$P" =~ ^--ERLANGMS_AUTH_URL=.+$ ]]; then
			ERLANGMS_AUTH_URL="$(echo $P | cut -d= -f2)"
		elif [ "$P" = "--help" ]; then
			help
		else
			echo "Invalid parameter: $P"
			help
		fi
	else
		# Se for informado apenas um parâmetro e não começa com -- então é o nome do tarfile
		if [ "$#" = "1" ]; then
			TAR_FILE=$P
		else
			echo "Invalid parameter: $P"
			help
		fi
	fi
done


if [ -z "$TAR_FILE" ]; then
	# Vamos precisar do registry. Validar registry settings
	if [ ! -z $REGISTRY ]; then
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
else
	# O arquivo tar foi informado, valida se existe
	if [ ! -f "$TAR_FILE" ]; then
		die "tarfile $TAR_FILE does not exist!"
	fi
fi



# Verifica se estamos dentro da pasta do projeto gitlab
CURRENT_DIR_IS_DOCKER_PROJECT_GITLAB=$(pwd | grep -c .docker$)


if [ "$SKIP_CHECK" = "false" ]; then
	check_docker_version
else
	echo "Skip check requirements enabled..."	
fi


# Erlangms settings
if [ -z "$ERLANGMS_HTTP_PORT_LISTENER" ]; then
	die "Informe HTTP port of erlangms!"
fi
if [ -z "$ERLANGMS_HTTPS_PORT_LISTENER" ]; then
	die "Informe HTTPS port of erlangms!"
fi

if [ -z "$ERLANGMS_BASE_URL" ]; then
	ERLANGMS_BASE_URL="https://$ERLANGMS_ADDR:$ERLANGMS_HTTPS_PORT_LISTENER"
fi
ERLANGMS_AUTH_URL="$ERLANGMS_BASE_URL/authorize"


# Se não foi informado o parâmetro --tarfile e a pasta atual 
# é a pasta do projeto do docker no gitlab, então tenta subir
if [ -z "$TAR_FILE" -a -z "$IMAGE" -a "$CURRENT_DIR_IS_DOCKER_PROJECT_GITLAB"="1" ]; then
	if [ -z "$APP_NAME" ]; then
		APP_NAME=$(basename $WORKING_DIR | sed -r 's/(.docker|_frontend)//g')
	fi
	APP_VERSION=$(docker inspect $APP_NAME | sed -n '/"RepoTags/ , /],/p' | sed '$d' | sed '$d' | tail -1 | sed -r 's/[^0-9\.]//g')
	IMAGE=$APP_NAME
	get_expose_ports
	make_conf_file

	echo "App name: $APP_NAME  Version: $APP_VERSION"
	echo "Erlangms base url: $ERLANGMS_BASE_URL"
	echo "Erlangms auth url: $ERLANGMS_AUTH_URL"
	echo "Erlangms auth protocol: $ERLANGMS_AUTH_PROTOCOL"
	echo "Erlangms server listener IP: $ERLANGMS_ADDR  HTTP/REST PORT: $ERLANGMS_HTTP_PORT_LISTENER   HTTPS/REST PORT: $ERLANGMS_HTTPS_PORT_LISTENER"
	echo "Web server listener IP: $SERVER_ADDR  HTTP PORT: $SERVER_HTTP_PORT_LISTENER   HTTPS PORT: $SERVER_HTTPS_PORT_LISTENER"
	echo "Client conf: $CLIENT_CONF"
	echo "Environment: $ENVIRONMENT"
	echo "Docker registry: $REGISTRY"
	echo "Docker entrypoint: $ENTRYPOINT"
	echo "Docker version: $(docker --version)"
	echo "-----------------------------------------------------------------------------"

	echo "docker stop previous $IMAGE"
	docker image stop $IMAGE > /dev/null 2>&1

	echo "docker image remove previous $IMAGE"
	docker image remove $IMAGE > /dev/null 2>&1

	echo docker run --network bridge -p $SERVER_ADDR:$SERVER_HTTP_PORT_LISTENER:$SERVER_HTTP_PORT_LISTENER \
					--network bridge -p $SERVER_ADDR:$SERVER_HTTPS_PORT_LISTENER:$SERVER_HTTPS_PORT_LISTENER \
			   -v $CLIENT_CONF:/app/$APP_NAME/barramento \
			   -dit --restart always $APP_NAME:$IMAGE_ID $ENTRYPOINT 
	docker run --network bridge -p $SERVER_ADDR:$SERVER_HTTP_PORT_LISTENER:$SERVER_HTTP_PORT_LISTENER \
			   -v $CLIENT_CONF:/app/$APP_NAME/barramento \
			   -dit --restart always $APP_NAME:$IMAGE_ID $ENTRYPOINT 

elif [ ! -z "$IMAGE" ]; then
	if [ -z "$APP_NAME" ]; then
		APP_NAME=$(echo $IMAGE | awk -F/ '{ print $2 }')
	fi
	APP_VERSION=$(docker inspect $APP_NAME | sed -n '/"RepoTags/ , /],/p' | sed '$d' | sed '$d' | tail -1 | sed -r 's/[^0-9\.]//g')
	
	ID_IMAGE=$(docker ps -f name=$APP_NAME | awk '{print $1}' | sed '1d')
	if [ ! -z "$ID_IMAGE" ]; then
		echo "docker stop $IMAGE"
		docker stop $ID_IMAGE
	fi

	LS_IMAGES=$(docker images $IMAGE)
	if [ ! -z "$LS_IMAGE" ]; then
		echo "rmi $LS_IMAGES..."
		docker rmi $LS_IMAGES
	fi

	docker rm erlangms_$APP_NAME

	echo "docker pull $IMAGE"
	docker pull $IMAGE

	get_expose_ports
	make_conf_file

	echo "App name: $APP_NAME  Version: $APP_VERSION"
	echo "Erlangms base url: $ERLANGMS_BASE_URL"
	echo "Erlangms auth url: $ERLANGMS_AUTH_URL"
	echo "Erlangms auth protocol: $ERLANGMS_AUTH_PROTOCOL"
	echo "Erlangms server listener IP: $ERLANGMS_ADDR  HTTP/REST PORT: $ERLANGMS_HTTP_PORT_LISTENER   HTTPS/REST PORT: $ERLANGMS_HTTPS_PORT_LISTENER"
	echo "Web server listener IP: $SERVER_ADDR  HTTP PORT: $SERVER_HTTP_PORT_LISTENER   HTTPS PORT: $SERVER_HTTPS_PORT_LISTENER"
	echo "Client conf: $CLIENT_CONF"
	echo "Environment: $ENVIRONMENT"
	echo "Docker registry: $REGISTRY"
	echo "Docker entrypoint: $ENTRYPOINT"
	echo "Docker version: $(docker --version)"
	echo "-----------------------------------------------------------------------------"


	echo docker run  --name erlangms_$APP_NAME \
			   --network bridge -p $SERVER_ADDR:$SERVER_HTTP_PORT_LISTENER:$SERVER_HTTP_PORT_LISTENER \
			   -v $CLIENT_CONF:/app/$APP_NAME/barramento \
			   -dit --restart always $IMAGE:$IMAGE_ID $ENTRYPOINT 
	docker run --name erlangms_$APP_NAME \
			   --network bridge -p $SERVER_ADDR:$SERVER_HTTP_PORT_LISTENER:$SERVER_HTTP_PORT_LISTENER \
			   -v $CLIENT_CONF:/app/$APP_NAME/barramento \
			   -dit --restart always $IMAGE:$IMAGE_ID $ENTRYPOINT  
else
	if [ -z "$APP_NAME" ]; then
		APP_NAME=$(echo $TAR_FILE | awk -F: '{ print $1 }')
	fi
	APP_VERSION=$(docker inspect $APP_NAME | sed -n '/"RepoTags/ , /],/p' | sed '$d' | sed '$d' | tail -1 | sed -r 's/[^0-9\.]//g')
	IMAGE=$APP_NAME
	get_expose_ports
	make_conf_file

	echo "App name: $APP_NAME  Version: $APP_VERSION"
	echo "Erlangms base url: $ERLANGMS_BASE_URL"
	echo "Erlangms auth url: $ERLANGMS_AUTH_URL"
	echo "Erlangms auth protocol: $ERLANGMS_AUTH_PROTOCOL"
	echo "Erlangms server listener IP: $ERLANGMS_ADDR  HTTP/REST PORT: $ERLANGMS_HTTP_PORT_LISTENER   HTTPS/REST PORT: $ERLANGMS_HTTPS_PORT_LISTENER"
	echo "Web server listener IP: $SERVER_ADDR  HTTP PORT: $SERVER_HTTP_PORT_LISTENER   HTTPS PORT: $SERVER_HTTPS_PORT_LISTENER"
	echo "Client conf: $CLIENT_CONF"
	echo "Environment: $ENVIRONMENT"
	echo "Docker registry: $REGISTRY"
	echo "Docker entrypoint: $ENTRYPOINT"
	echo "Docker version: $(docker --version)"
	echo "-----------------------------------------------------------------------------"

	echo "docker stop previous $IMAGE"
	docker image stop $IMAGE > /dev/null 2>&1

	echo "docker image remove previous $IMAGE"
	docker image remove $IMAGE > /dev/null 2>&1

	echo docker load -i $TAR_FILE
	docker load -i $TAR_FILE

	echo docker run --network bridge -p $SERVER_ADDR:$SERVER_HTTP_PORT_LISTENER:$SERVER_HTTP_PORT_LISTENER \
			   -v $CLIENT_CONF:/app/$APP_NAME/barramento \
			   -dit --restart always $APP_NAME:$IMAGE_ID $ENTRYPOINT 
	docker run --network bridge -p $SERVER_ADDR:$SERVER_HTTP_PORT_LISTENER:$SERVER_HTTP_PORT_LISTENER \
			   -v $CLIENT_CONF:/app/$APP_NAME/barramento \
			   -dit --restart always $APP_NAME:$IMAGE_ID $ENTRYPOINT 

fi
