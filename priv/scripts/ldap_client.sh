#!/bin/bash 
#
# Title: ERLANGMS ldap client tool (bash version)
# Author: Everton de Vargas Agilar
# Data: 16/03/2016
#
# Requisitos: Necessita do pacote ldap-utils
#

# locale
export LANG=en_US.UTF-8
export LC_CTYPE=pt_BR.UTF-8
export LC_NUMERIC=pt_BR.UTF-8
export LC_TIME=pt_BR.UTF-8
export LC_COLLATE="en_US.UTF-8"
export LC_MONETARY=pt_BR.UTF-8
export LC_MESSAGES="en_US.UTF-8"
export LC_PAPER=pt_BR.UTF-8
export LC_NAME=pt_BR.UTF-8
export LC_ADDRESS=pt_BR.UTF-8
export LC_TELEPHONE=pt_BR.UTF-8
export LC_MEASUREMENT=pt_BR.UTF-8
export LC_IDENTIFICATION=pt_BR.UTF-8

# Get linux description
LINUX_DESCRIPTION=$(awk -F"=" '{ if ($1 == "PRETTY_NAME"){ 
									gsub("\"", "", $2);  print $2 
								 } 
							   }'  /etc/os-release)

# Primary IP of the server
LINUX_IP_SERVER=$(hostname -I | cut -d" " -f1)


VERSION=1.0.0
CURRENT_DIR=$(pwd)
TMP_DIR="/tmp/erlangms/ldap/ldap_client_$(date '+%d%m%Y_%H%M%S')_$$"
EMS_NODE="ems-bus"
ENVIRONMENT="$LINUX_DESCRIPTION IP $LINUX_IP_SERVER "
CURRENT_DATE=$(date '+%d/%m/%Y %H:%M:%S')
REPORT_FILE="$TMP_DIR/report_ldap_client.txt"
SEND_EMAIL="false"
PRINT_HEADER="true"
FALHA_LDAP="false"

# tmpfiles go to /$TMP_DIR
mkdir -p $TMP_DIR && cd $TMP_DIR

# SMTP parameter
SMTP_SERVER="mail.unb.br"
SMTP_PORT=587
SMTP_DE="erlangms@unb.br"
SMTP_PARA="evertonagilar@unb.br,evertonagilar@unb.br"
SMTP_PASSWD=erl1523


send_email(){	
	REPORT_CONTENT=$(cat $REPORT_FILE)
	TITULO_MSG="ERLANGMS LDAP Client Test  -  Date: $CURRENT_DATE   IP: $LINUX_IP_SERVER" 
	SUBJECT="<html>
			<head>
				<style>
					pre {
						font-family: \"Courier New\", Courier, \"Lucida Sans Typewriter\", \"Lucida Typewriter\", monospace;
						font-size: 14px;
						font-style: normal;
						font-variant: normal;
						font-weight: 460;
						line-height: 18.5714px;
					}
					
				</style>
			<head>
			<body>
				<pre>$REPORT_CONTENT<pre>
			</body>
			</html>"
    python <<EOF
# -*- coding: utf-8 -*-
import smtplib
from email.mime.text import MIMEText
from email.Utils import formatdate
from email.mime.multipart import MIMEMultipart
from email import encoders
try:
	smtp = smtplib.SMTP("$SMTP_SERVER", $SMTP_PORT)
	smtp.starttls()
	smtp.login("$SMTP_DE", "$SMTP_PASSWD")
	msg = MIMEMultipart()
	msg['Subject'] = "$TITULO_MSG"
	msg['From'] = "$SMTP_DE"
	msg['To'] = "$SMTP_PARA"
	msg['Date'] = formatdate(localtime=True)
	part1 = MIMEText("Relatório em anexo.", 'plain')
	part2 = MIMEText("""$SUBJECT""", 'html', 'utf-8')
	msg.attach(part2)
	smtp.sendmail("$SMTP_DE", ['evertonagilar@unb.br'], msg.as_string())
	smtp.quit()
	exit(0)
except Exception as e:
	print("Send email error: "+ str(e))
	exit(1)
EOF
	
}	


generate_test(){
	if [ "$PRINT_HEADER" = "true" ]; then
		# Output to report file if send email
		if [ "$SEND_EMAIL" = "true" ]; then
			exec > >(tee -a ${REPORT_FILE} )
			exec 2> >(tee -a ${REPORT_FILE} >&2)
		fi

		echo "Starting ERLANGMS ldap_client.sh tool   ( Version: $VERSION )"
		echo "Date: $CURRENT_DATE"
		echo "Server: $LDAP_SERVER"
		echo "Environment: $ENVIRONMENT"

		PRINT_HEADER="false"
	fi

	echo ""
	if [ "$COUNTER" = "1" ]; then
		echo "Realizando requisição LDAP para $LDAP_SERVER com user $USER"
		echo ldapsearch -xLLL -h "$LDAP_SERVER" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid="$USER" -w "xxxxxxx"
		echo ""
		ldapsearch -xLLL -h "$LDAP_SERVER" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid="$USER" -w "$ADMIN_PASSWD"
	else
		echo "Realizando $COUNTER requisições LDAP para $LDAP_SERVER com user $USER"
		echo ldapsearch -xLLL -h "$LDAP_SERVER" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid="$USER" -w "xxxxxxx"
		until [  $COUNTER -lt 1 ]; do
			echo ""
			let COUNTER-=1
			ldapsearch -xLLL -h "$LDAP_SERVER" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid="$USER" -w "$ADMIN_PASSWD"
		done
	fi
}


# Verifica se não há parâmetros
if [ "$#" = "0" ] || [ "$1" = "--help" ]; then
	echo "Modo de usar 1: ./ldap_client.sh qtd_requests host_ldap login"
	echo "Modo de usar 2: ./ldap_client.sh host_ldap user"
	echo "Modo de usar 3: ./ldap_client.sh qtd_requests host_ldap login --sendemail"
	echo "         qtd_requests    => número de requisições simultâneas (default é 100)"
	echo "         host_ldap       => host do ldap (default é localhost:2389)"	
	echo "         login           => login do user"	
	echo "         admin_passwd    => password do admin do ldap"	
	echo "         --sendemail  => Envia e-mail ao admin em caso de erro"
	exit
fi

# Parâmetro qtd_requests
if [ $# -gt 3 ]; then
	COUNTER=$1
    RE='^[0-9]+$'
    if ! [[ $COUNTER =~ $RE ]] ; then
       echo "Parâmetro qtd_requests ( $COUNTER ) deve ser um número!" >&2; exit 1
    fi
else
    COUNTER=1
fi

# Parâmetro host_ldap
if [ $# -ge 4 ]; then
    LDAP_SERVER="$2"
    RE='^[0-9a-zA-Z_-.]+:[0-9]+$'
    if ! [[ $LDAP_SERVER =~ $RE ]] ; then
       echo "Parâmetro host_ldap ( $LDAP_SERVER ) deve possuir o seguinte formato: hostname:port. Ex.: localhost:2389" >&2; exit 1
    fi
else
       LDAP_SERVER="localhost:2389"
fi

# Parâmetro user
if [ "$#" = "1" ] || [ "$#" = "2" ]; then
    USER=$1
elif [ "$#" = "3" ]; then
    USER=$2
else
    USER=$3
fi

# Parâmetro admin_passwd
if [ "$#" = "4" -o "$#" = "5" ]; then
	ADMIN_PASSWD="$4"
elif [ "$#" = "3" ]; then
    ADMIN_PASSWD="$3"
elif [ "$#" = "2" ]; then
	ADMIN_PASSWD="$2"
else
	echo "Informe a senha do administrador do LDAP para autenticação:"
	read -s ADMIN_PASSWD
fi

if [ "$#" = "5" -a "$5" = "--sendemail" ]; then
	SEND_EMAIL="true"
fi

# Quando envia email o teste é mais rigoroso
# tenta várias vezes antes de notificar falha
if [ "$SEND_EMAIL" = "true" ]; then
	for T in 1 2 3 4 5 6; do
		generate_test  # ocorre erro 49 quando invalid credentials
		if [ "$?" = "0" ]; then
			FALHA_LDAP="false"
			break;
		else
			FALHA_LDAP="true"
		fi
		echo ""
		AGUARDA=$(($T*2))
		echo "Falhou, aguardando $AGUARDA segundos para realizar nova requisição..."
		sleep $AGUARDA  # aguarda de forma crescente
	done
else
	generate_test  # ocorre erro 49 quando invalid credentials
fi

if [ "$SEND_EMAIL" = "true"  -a "$FALHA_LDAP" = "true" ]; then
	echo "Parece que o servidor LDAP está fora de serviço!"
	send_email && echo "This report was send to administrators."
fi
rm -rf $TMP_DIR/