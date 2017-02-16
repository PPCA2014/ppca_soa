#!/bin/bash 
#
# Title: ERLANGMS audit ldap log tool
# Author: Everton de Vargas Agilar
# Data: 14/02/2017
#
#
# Requirements to run the service:
#       * Python
#
# Use mode: 
#
#    $ sudo ./audit_ldap_log.sh minutes [ --sendemail ]
#
#   ./audit_ldap_log.sh 10 --sendemail  
#   ./audit_ldap_log.sh  
#
# Obs.: parameter --sendmail is optional and send email to admin
#
#
## History of changes:
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


# Parameters

VERSION=1.0.0
LDAP_SERVER="$(hostname):2389"
CURRENT_DIR=$(pwd)
TMP_DIR="/tmp/erlangms/ldap/audit_ldap_log_$(date '+%d%m%Y_%H%M%S')_$$"
ENVIRONMENT="desenvolvimento"
MMIN="1440"
CURRENT_DATE=$(date '+%d/%m/%Y %H:%M:%S')
REPORT_FILE="$TMP_DIR/audit_ldap_log_$(date '+%d%m%Y_%H%M%S').log"
SUB_TITLE_REPORT="LAST DAY OF OPERATION"

# Log destination parameter
LOG_DEST=/var/log/ems-bus
LOG_FILE_TMP="$TMP_DIR/full_log_file.log"
LOG_FILE="$TMP_DIR/full_log_file_filtered.log"
USO_LOG_DEST=$(df -h  $LOG_DEST | sed '1d' | awk '{print $5}' | sed 's/%//')

# SMTP parameter
SMTP_SERVER="mail.unb.br"
SMTP_PORT=587
SMTP_DE="evertonagilar@unb.br"
#SMTP_PARA="evertonagilar@unb.br,evertonagilar@unb.br,felipesantos@unb.br"
SMTP_PARA="evertonagilar@unb.br,evertonagilar@unb.br"
SMTP_PASSWD=unb9601


# tmpfiles go to /$TMP_DIR
mkdir -p $TMP_DIR && cd $TMP_DIR


if [ "$#" = "1" ]; then
	if [ "$1" = "--help" ]; then
		echo "How to use: ./audit_ldap_log.sh minutes  [ --sendemail ]"
		echo "where minutes is logfile's data was last modified minutes ago (default is 1 day -- 43200 minutes)"
		echo "parameter --sendmail is optional and send email to admin"
		exit 1
	else
		MMIN="$1"
		RE='^[0-9]{1,4}$'
		if ! [[ $MMIN =~ $RE ]] ; then
			echo "Parameter minutes with value \"$MMIN\" is inválid. Values allowed from 1 to 99."
			echo "How to use: ./audit_ldap_log.sh minutes  [ --sendemail ]"
			echo "where minutes is logfile's data was last modified minutes ago (default is 1 day -- 43200 minutes)"
			echo "parameter --sendmail is optional and send email to admin"
			exit 1
		fi
	fi
fi

# Compute initial date
DATE_INIT=$(date --date="$MMIN min ago" '+%d/%m/%Y %H:%M:%S')   # Calcula a data retroativa


# generate a report
generate_report(){
	# Output to report file
	exec > >(tee -a ${REPORT_FILE} )
	exec 2> >(tee -a ${REPORT_FILE} >&2)
	
	echo "Starting ERLANGMS audit_ldap_log tool   ( Version: $VERSION )"
	echo "Date: $CURRENT_DATE"
	echo "Log dest: $LOG_DEST"
	echo "Server: $LDAP_SERVER"
	echo "Environment: $ENVIRONMENT"
	echo
	
	if [ "$MMIN" == "1440" ]; then
		echo "Analyzing logfiles between $DATE_INIT (1 day ago) and $CURRENT_DATE"
	else
		echo "Analyzing logfiles between $DATE_INIT ($MMIN minutes ago) and $CURRENT_DATE"
	fi
	LOG_FILE_LIST=$(find "$LOG_DEST" -type f -mmin "-$MMIN" -follow)
	if [ -z "$LOG_FILE_LIST" ]; then
		echo "No logfiles to analyze."
		exit 1
	else
		echo "Reading the following logfiles:"
		find "$LOG_DEST" -type f -mmin "-$MMIN" -follow
	fi 


	# concat all logfiles (remove characters control of the line colors)
	find "$LOG_DEST" -type f -mmin "-$MMIN" -follow | xargs sed -r '/ERROR/ s/.{7}//' | sed -r '/ERROR/ s/.{4}$//' > $LOG_FILE_TMP
	
	

	# filter only the log lines of the defined interval	$DATE_INIT
	awk -v DT_INIT="$DATE_INIT" '{
			DT_LOG=$2" "$3;
			if (DT_LOG >= DT_INIT){
				print; 
			}
	}' $LOG_FILE_TMP > $LOG_FILE
	
	echo
	echo
	echo


	# Print title
	echo
	echo
	echo "                                LDAP REPORT FOR THE"
	if [ "$MMIN" == "1440" ]; then
		SUB_TITLE_REPORT="LAST DAY OF OPERATION"
		echo "                               $SUB_TITLE_REPORT"
	elif [ "$MMIN" == "1" ]; then
		SUB_TITLE_REPORT="LAST $MMIN MINUTE OF OPERATION"
		echo "                             $SUB_TITLE_REPORT"
	else 
		SUB_TITLE_REPORT="LAST $MMIN MINUTES OF OPERATION"
		echo "                          $SUB_TITLE_REPORT"
	fi
	echo
	echo
	
	

	############################################### SUCCESS REPORT ####################################################################


	# create a tmp file with request search lines
	egrep '^INFO.*Ldap request search' $LOG_FILE | tr -d "<>" > info_request_search.tmp
	REQ_SEARCH_SUCCESS_TOTAL=$(wc -l info_request_search.tmp | cut -d" " -f1)


	# create a tmp file with request search success lines
	grep 'Ldap' info_request_search.tmp | egrep '^INFO.*Ldap request search' | cut -d" " -f8- | sed 's/ success.//g' | sort > request_search_success.tmp
	uniq request_search_success.tmp > request_search_success_uniq.tmp
	
	# totals
	REQ_SEARCH_SUCCESS_TOTAL=$(wc -l request_search_success.tmp | cut -d" " -f1)
	REQ_SEARCH_SUCCESS_UNIQ_TOTAL=$(wc -l request_search_success_uniq.tmp | cut -d" " -f1)


	echo
	echo "                ****************** SUCCESS REPORT ******************"
	echo
	echo

	echo "1) Number of success ldap requests: $REQ_SEARCH_SUCCESS_TOTAL requests"
	echo
	echo
	
	echo "2) Which users were found in the ldap request search operation: $REQ_SEARCH_SUCCESS_UNIQ_TOTAL users"
	echo '+-----------------------------------------------+------------------------------------------------------+'
	printf "| %-45s |  %-51s |\n"  "LOGIN"   "NAME"
	echo '+-----------------------------------------------+------------------------------------------------------+'
	cat request_search_success_uniq.tmp | xargs printf "| %-45s |  %-50s  |\n"  
	echo '+-----------------------------------------------+------------------------------------------------------+'

	echo
	echo
	echo
	
	echo "3) Number of success ldap requests search operation per user:"
	echo '+-----------------------------------------------+------------------------------------------------------+'
	printf "| %-45s | %-52s |\n"  "LOGIN"   "REQUESTS"
	echo '+-----------------------------------------------+------------------------------------------------------+'
	awk '{print $1}' request_search_success_uniq.tmp > request_search_success_uniq_login.tmp 
	for USER in `cat request_search_success_uniq_login.tmp`; do
		REQ_SEARCH_SUCCESS_TOTAL_By_USER=$(grep -w $USER request_search_success.tmp | wc -l | cut -d" " -f1)
		printf "| %-45s | %19d  %32s|\n"   $USER   $REQ_SEARCH_SUCCESS_TOTAL_By_USER " "
	done
	echo '+-----------------------------------------------+------------------------------------------------------+'
	printf "| %-45s | TOTAL: %12d %33s|\n"   " " $REQ_SEARCH_SUCCESS_TOTAL " "
	echo '+-----------------------------------------------+------------------------------------------------------+'
	
	echo
	echo
	


	############################################  ERRRO REPORT  #######################################################################


	# create a tmp file with request search fail lines (remove caracteres de controle da cor antes de ERROR)
	egrep '^ERROR.*Ldap' $LOG_FILE > error_request_search.tmp
	REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL=$(wc -l error_request_search.tmp | cut -d" " -f1)
	
	# create a tmp file with request search login does not exist lines
	egrep 'Ldap request search' error_request_search.tmp | cut -d" " -f8- | sed 's/ does not exist.//g' | sort > request_search_login_does_not_exist.tmp
	uniq request_search_login_does_not_exist.tmp > request_search_login_does_not_exist_uniq.tmp
	REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL=$(wc -l request_search_login_does_not_exist.tmp | cut -d" " -f1)
	
	
	REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_UNIQ_TOTAL=$(wc -l request_search_login_does_not_exist_uniq.tmp | cut -d" " -f1)


	echo
	echo
	echo
	echo "                ****************** ERROR REPORT ******************"
	echo
	echo
	
	echo "4) Number of failed ldap request attempts because login does not exist: $REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL requests"
	echo
	echo
	
	echo "5) Which users were not found in the ldap request search operation: $REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_UNIQ_TOTAL users"
	echo '+-----------------------------------------------+------------------------------------------------------+'
	printf "| %-45s | %-52s |\n"  "LOGIN"   "REQUESTS"                                                                  
	echo '+-----------------------------------------------+------------------------------------------------------+'
	for USER in `cat request_search_login_does_not_exist_uniq.tmp`; do
		REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL_BY_USER=$(grep -w $USER request_search_login_does_not_exist.tmp | wc -l | cut -d" " -f1)
		printf "| %-45s | %19d         %25s|\n"   $USER   $REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL_BY_USER " "
	done
	echo '+-----------------------------------------------+------------------------------------------------------+'
	printf "| %-45s | TOTAL: %12d %33s|\n"   " " $REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL " "
	echo '+-----------------------------------------------+------------------------------------------------------+'
	echo

}


send_email(){	
	REPORT_CONTENT=$(cat $REPORT_FILE)
	TITULO_MSG="ERLANGMS LDAP Proxy Log Analysis Report -- $SUB_TITLE_REPORT" 
	SUBJECT="<font size=\"3\" face=\"Courier New, Courier, monospace\"><pre>$REPORT_CONTENT</pre></font>"
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
except Exception as e:
	print(e)
EOF
	
}	


# /////////////////// main /////////////////

generate_report
[ "$2" == "--sendemail" ] && send_email && echo "Report sent by email to administrators."
cd $CURRENT_DIR



