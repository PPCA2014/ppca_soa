#!/bin/bash 
#
# Title: ERLANGMS audit ldap log tool
# Author: Everton de Vargas Agilar
# Data: 14/02/2017
#
#
# Requisitos para executar o serviço:
#       * Python (já incluído como padrão no Debian)
#
# Modo de usar: 
#
#    $ sudo ./audit_ldap_log.sh
#
#
# Observações: 
#
#
#
#
## Histórico de modificações:
#
# Data       |  Quem           |  Mensagem  
# -----------------------------------------------------------------------------------------------------
# 14/02/2017  Everton Agilar     Release inicial    
#
#
#
#
#
########################################################################################################

VERSION=1.0.0
LDAP_SERVER="$(hostname):2389"
CURRENT_DIR=$(pwd)
TMP_DIR="/tmp/erlangms/audit_ldap_log_$$/"

# Enables installation logging
AUDIT_LOG_FILE="audit_ldap_log_$(date '+%d%m%Y_%H%M%S').log"
exec > >(tee -a ${LOG_FILE} )
exec 2> >(tee -a ${LOG_FILE} >&2)

# tmpfiles go to /$TMP_DIR
mkdir -p $TMP_DIR && cd $TMP_DIR


# Log destination
LOG_DEST=/home/agilar/desenvolvimento/erlangms/ems-bus/priv/log/emsbus@CPD-DES-374405/2017/Fevereiro
LOG_FILE="$LOG_DEST/emsbus_14022017_1607.log"


# Configurações SMTP para envio de alertas aos admins
SMTP_SERVER="mail.unb.br"
SMTP_PORT=587
SMTP_DE="evertonagilar@unb.br"
SMTP_PARA="evertonagilar@unb.br,evertonagilar@unb.br"
SMTP_PASSWD=unb9601


# Percentual de uso da pasta de backup
USO_LOG_DEST=$(df -h  $LOG_DEST | sed '1d' | awk '{print $5}' | sed 's/%//')


# Quando o espaço de armazenamento $LOG_DEST chegar a este limite, um e-mail será 
# enviado aos admins para avisar que o espaço em disco está acabando ( valor é percentual mas não inclua o '%' )
USO_LOG_DEST_THRESHOLD=98




# Função para enviar e-mail
# Parâmetros:
#   $1  - Título do e-mail
#   $2  - Corpo do e-mail
envia_email () {
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
	smtp.sendmail("$SMTP_DE", ['evertonagilar@unb.br'], msg.as_string())
	smtp.quit()
except Exception:
	print(Exception)
EOF
}


# Função para realizar auditoria de logs
audit(){
	echo "Starting audit_ldap_log.sh Version: $VERSION - ERLANGMS"
	echo "Date: $(date '+%d/%m/%Y %H:%M:%S')"
	echo "Log dest: $LOG_DEST"
	echo "Space available for log files: $(( 100 - $USO_LOG_DEST ))%"
	echo "Server: $(uname -a)"
	echo "Auditing ldap Logs on $LDAP_SERVER, wait..."
	echo 
	

	# create a tmp file with request search lines
	egrep '^INFO.*Ldap request search' $LOG_FILE | sort > request_search.tmp
	REQ_SEARCH_TOTAL=$(wc -l request_search.tmp | cut -d" " -f1)


	# create a tmp file with request search success lines
	grep 'Ldap' request_search.tmp | egrep '^INFO.*Ldap request search' | cut -d" " -f8- | sed 's/ success.//g' > request_search_success.tmp
	uniq request_search_success.tmp > request_search_success_uniq.tmp
	
	
	REQ_SEARCH_SUCCESS_TOTAL=$(wc -l request_search_success.tmp | cut -d" " -f1)
	REQ_SEARCH_SUCCESS_UNIQ_TOTAL=$(wc -l request_search_success_uniq.tmp | cut -d" " -f1)
	

	echo
	echo "                ****************** SUCCESS REPORT ******************"
	echo
	
	echo "1) Total number of ldap requests: $REQ_SEARCH_TOTAL"
	echo
	echo
	
	echo "2) Distinguished users were success authenticated: $REQ_SEARCH_SUCCESS_UNIQ_TOTAL users"
	echo
	echo '+---------------------------+--------------------------------------------------------------------------+'
	printf "| %-25s |  %-70s\n"  "LOGIN"   "NAME                                                                    |"
	echo '+---------------------------+--------------------------------------------------------------------------+'
	cat request_search_success_uniq.tmp | xargs printf "| %-25s |  %-70s  |\n"  
	echo '+---------------------------+--------------------------------------------------------------------------+'

	echo
	echo
	echo
	
	echo "3) Number of success requests search per user:"
	echo
	echo
	
	echo '+---------------------------+--------------------------------------------------------------------------+'
	printf "| %-25s |  %-70s\n"  "LOGIN"   "REQUESTS                                                                |"
	echo '+---------------------------+--------------------------------------------------------------------------+'
	awk '{print $1}' request_search_success_uniq.tmp > request_search_success_uniq_login.tmp 
	for USER in `cat request_search_success_uniq_login.tmp`; do
		REQ_SEARCH_SUCCESS_TOTAL_By_USER=$(grep $USER request_search_success.tmp | wc -l | cut -d" " -f1)
		printf "| %-25s |  %5d                                                                   |\n"   $USER   $REQ_SEARCH_SUCCESS_TOTAL_By_USER
	done
	echo '+---------------------------+--------------------------------------------------------------------------+'
	
	echo
	echo


	
	
}


# Função para alertar um backup bem sucedido
alerta_sucesso_backup(){
	TextLog=$(cat $LOG_FILE)
	envia_email "Alerta de backup do GitLab bem sucedido." \
				"Log da operação do backup:\n\n$TextLog\n\nAtt.\n\t git-backup.sh"  
}


# Função para alertar um backup mal sucedido
alerta_falha_backup(){
	TextLog=$(cat $LOG_FILE)
	envia_email "Alerta de backup do GitLab não realizado!!!" \
				"Atenção: Ocorreu um erro ao realizar o backup do GitLab..\n\nLog da tentativa de backup:\n\n$TextLog\n\nAtt.\n\t git-backup.sh"  
}


# Função para verificar e alertar se a pasta de backup está ficando sem espaço
verifica_espaco_disponivel(){
	if [ "$USO_LOG_DEST" -ge "$USO_LOG_DEST_THRESHOLD" ]; then
		TextLog=$(cat $LOG_FILE)
		envia_email "Alerta de falta de espaço para backup do GitLab" \
		            "Atenção: O espaço da unidade de backup do GitLab está em $USO_LOG_DEST% da capacidade!!!\n\nLog da última operação de backup:\n\n$TextLog\n\nAtt.\n\t git-backup.sh"  
	fi
}



############## main ####################

audit


cd $CURRENT_DIR
echo
echo "Ok!!!"




