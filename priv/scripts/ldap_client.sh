#!/bin/bash 
#
# Title: ERLANGMS ldap client tool (bash version)
# Author: Everton de Vargas Agilar
# Data: 16/03/2016
#
# Requires ldap-utils package
#
# How to use: sudo ./ldap_client.sh 1 localhost geral 123456 --sendemail --auto_restart
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
TMP_DIR="/tmp/erlangms_$$/ldap/ldap_client_$(date '+%d%m%Y_%H%M%S')"
EMS_NODE="ems-bus"
CURRENT_DATE=$(date '+%d/%m/%Y %H:%M:%S')
REPORT_FILE="$TMP_DIR/report_ldap_client.txt"
SEND_EMAIL="false"
PRINT_HEADER="true"
FALHA_LDAP="false"
AUTO_RESTART="false"
RESTARTED="false"
LDAP_PORT="2389"
LDAP_SERVER="127.0.0.1:$LDAP_PORT"
ENVIRONMENT="$LINUX_DESCRIPTION  IP $LINUX_IP_SERVER"
NET_INTERFACES=$(netstat -tnl | awk -v PORT=$LDAP_PORT '$4 ~ PORT { print $4; }' | tr '\n' ' ')
MEM=$(free -h | awk '$1 == "Mem:" {  print "Total: " $2 "   Free: " $4 "   Avaiable: " $7; }')
LOAD_AVERAGE=$(cat /proc/loadavg | awk '{ print "Min: "$1"     5 Min: "$2"    15 Min: "$3 ; }')
TIME_WAIT_START_SERVER=10  # seconds
TIME_WAIT_TEST=4 # seconds
EMAIL_ONLY_ERROR="false"

# tmpfiles go to /$TMP_DIR
mkdir -p $TMP_DIR && cd $TMP_DIR

# SMTP parameter
SMTP_SERVER="mail.unb.br"
SMTP_PORT=587
SMTP_DE="erlangms@unb.br"
SMTP_PARA="evertonagilar@unb.br,evertonagilar@unb.br"
SMTP_PASSWD=erl1523


# imprime o header do comando.
# O header só impresso uma vz
print_header(){
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
		echo "Memory $MEM"
		echo "Listen interfaces: $NET_INTERFACES"
		echo "Load average: $LOAD_AVERAGE"

		PRINT_HEADER="false"
		echo ""
	fi
}	
	
	
send_email(){	
	REPORT_CONTENT=$(cat $REPORT_FILE | sed 's/passwd:.*/passwd: removido por segurança/')
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


# Performs the ldap query using the ldapsearch
ldap_search(){
	if [ "$COUNTER" = "1" ]; then
		echo "Performing LDAP request on $LDAP_SERVER with user $USER using ldapsearch"
		echo ldapsearch -xLLL -h "$LDAP_SERVER" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid="$USER" -w "xxxxxxx"
		echo ""
		ldapsearch -xLLL -h "$LDAP_SERVER" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid="$USER" -w "$ADMIN_PASSWD"
	else
		echo "Performing $COUNTER LDAP request on $LDAP_SERVER with user $USER using ldapsearch"
		echo ldapsearch -xLLL -h "$LDAP_SERVER" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid="$USER" -w "xxxxxxx"
		until [  $COUNTER -lt 1 ]; do
			echo ""
			let COUNTER-=1
			ldapsearch -xLLL -h "$LDAP_SERVER" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid="$USER" -w "$ADMIN_PASSWD"
		done
	fi
}

# Performs the LDAP request test
generate_test(){
	print_header
	for T in 1; do
		# ocorre erro 49 quando invalid credentials
		# ocorre erro 255 quando consegue contactar o servidor na porta
		ldap_search
		if [ "$?" = "0" ]; then
			FALHA_LDAP="false"
			break;
		else
			FALHA_LDAP="true"
		fi
		echo "Failed, waiting $TIME_WAIT_TEST seconds to retry..."
		WAIT=$(($T*$TIME_WAIT_TEST))
		sleep $WAIT  # Awaits increasing
		echo ""
	done
}


# Check parameters
if [ "$#" = "0" ] || [ "$1" = "--help" ]; then
	echo "Starting ERLANGMS ldap_client.sh tool   ( Version: $VERSION )"
	echo "How to use 1: ./ldap_client.sh login"
	echo "How to use 2: ./ldap_client.sh login admin_passwd"
	echo "How to use 3: ./ldap_client.sh qtd_requests ldap_server login admin_passwd  [ --sendemail  --auto_restart --email_only_error ]"
	echo "         login           		=> login do user."	
	echo "         admin_passwd    		=> password do admin do ldap."	
	echo "         ldap_server       	=> host do ldap (default é localhost:2389)"	
	echo "         qtd_requests    		=> Number of simultaneous requests (default is 1)"
	echo "         --sendemail     		=> Send an email to admin."
	echo "         --auto_restart  		=> Restarts the bus on failure (local server only)"
	echo "         --email_only_error  	=> Send email only if there is an error."
	exit
fi

# Parameter qtd_requests
if [ $# -gt 3 ]; then
	COUNTER=$1
    RE='^[0-9]+$'
    if ! [[ $COUNTER =~ $RE ]] ; then
       echo "Parameter qtd_requests ($COUNTER) must be a number!" >&2; exit 1
    fi
else
    COUNTER=1
fi

# Parameter LDAP_SERVER
if [ $# -ge 4 ]; then
    LDAP_SERVER="$2"
    if [[ $LDAP_SERVER =~ ^[0-9a-zA-Z_-.]+:[0-9]+$ ]] ; then
       LDAP_PORT=$(echo $LDAP_SERVER | awk -F: '{ print $2; }')
    elif [[ $LDAP_SERVER =~ ^[0-9a-zA-Z_-.]+$ ]] ; then
		LDAP_SERVER=$LDAP_SERVER:$LDAP_PORT
    else
		echo "Parameter LDAP_SERVER ( $LDAP_SERVER ) is invalid. Example: localhost:2389" >&2; exit 1
    fi
fi

# Parameter user
if [ "$#" = "1" ] || [ "$#" = "2" ]; then
    USER=$1
elif [ "$#" = "3" ]; then
    USER=$2
else
    USER=$3
fi

# Parameter admin_passwd
if [ $# -ge  4 ]; then
	ADMIN_PASSWD="$4"
elif [ "$#" = "3" ]; then
    ADMIN_PASSWD="$3"
elif [ "$#" = "2" ]; then
	ADMIN_PASSWD="$2"
else
	echo "Enter the LDAP administrator password for authentication:"
	read -s ADMIN_PASSWD
fi



if [ "$5" = "--sendemail" -o "$6" = "--sendemail" -o "$7" = "--sendemail" ]; then
	SEND_EMAIL="true"
fi

if [ "$5" = "--auto_restart" -o "$6" = "--auto_restart" -o "$7" = "--auto_restart" ]; then
	AUTO_RESTART="true"
fi

if [ "$5" = "--email_only_error" -o "$6" = "--email_only_error" -o "$7" = "--email_only_error" ]; then
	EMAIL_ONLY_ERROR="true"
fi


generate_test

# Auto restart option resets the bus in case of failure
if [ "$AUTO_RESTART" = "true" -a "$FALHA_LDAP" = "true" ]; then
	if systemctl --version > /dev/null 2>&1 ; then
		echo "Restarting server in case of failure, please wait..."
		if sudo systemctl restart ems-bus 2> /dev/null ; then
			RESTARTED="true"
			echo "Ok, waiting $TIME_WAIT_START_SERVER seconds to verify that the ldap service is ok...."
			sleep $TIME_WAIT_START_SERVER
			generate_test
		fi
	else
		echo "Systemctl is required to restart the LDAP server."
	fi
fi

if [ "$FALHA_LDAP" = "true" ]; then
	if [ "$AUTO_RESTART" = "true" ]; then
		echo "WARNING: $LDAP_SERVER server is out of service and could not be restarted!"
	else
		echo "WARNING: $LDAP_SERVER server is out of service!"
	fi
else
	if [ "$RESTARTED" = "true" ]; then
		echo "The $LDAP_SERVER server has been successfully restarted and is operating normally."
	fi
fi


# Envia e-mail?
if [ "$SEND_EMAIL" = "true" ]; then
	if [ $FALHA_LDAP = "true" -a EMAIL_ONLY_ERROR = "true" ]; then
		send_email && echo "This report was send to administrators."
	elif [ $EMAIL_ONLY_ERROR = "false" ]; then
		send_email && echo "This report was send to administrators."
	fi
fi
rm -rf $TMP_DIR/
