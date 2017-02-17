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
		echo "parameter --sendemail is optional and send email to admin"
		exit 1
	else
		MMIN="$1"
		RE='^[0-9]{1,5}$'
		if ! [[ $MMIN =~ $RE ]] ; then
			echo "Parameter minutes with value \"$MMIN\" is inválid. Values allowed from 1 to 99999."
			echo "How to use: ./audit_ldap_log.sh minutes  [ --sendemail ]"
			echo "where minutes is logfile's data was last modified minutes ago (default is 1 day -- 43200 minutes)"
			echo "parameter --sendemail is optional and send email to admin"
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


	# concat all logfiles of ems_ldap_handler
	# remove characters control of the line colors
	find "$LOG_DEST" -type f -mmin "-$MMIN" -follow | xargs grep ems_ldap_handler | sed -r '/ERROR/ s/.{7}//' | sed -r '/ERROR/ s/.{4}$//' > $LOG_FILE_TMP
	
	

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


	# filter data
	egrep '^INFO.*ems_ldap_handler search' $LOG_FILE | tr -d "<>" > info_request_search.tmp
	grep 'success' info_request_search.tmp | cut -d" " -f7- | sed 's/ success.//g' | sort > request_search_success.tmp
	uniq request_search_success.tmp > request_search_success_uniq.tmp
	

	# totals
	REQ_SEARCH_TOTAL=$(wc -l info_request_search.tmp | cut -d" " -f1)
	REQ_SEARCH_SUCCESS_TOTAL=$(wc -l request_search_success.tmp | cut -d" " -f1)
	REQ_SEARCH_SUCCESS_UNIQ_TOTAL=$(wc -l request_search_success_uniq.tmp | cut -d" " -f1)


	echo
	echo "                ****************** SUCCESS REPORT ******************"
	echo
	echo


	echo "1) Number of ldap requests: $REQ_SEARCH_TOTAL requests"
	echo
	echo "2) Number of success ldap requests: $REQ_SEARCH_SUCCESS_TOTAL requests"
	echo
	echo "3) Users found in the ldap request search operation: $REQ_SEARCH_SUCCESS_UNIQ_TOTAL users"
	echo '+-----------------------------------------------+------------------------------------------------------+'
	printf "| %-45s |  %-51s |\n"  "LOGIN"   "NAME"
	echo '+-----------------------------------------------+------------------------------------------------------+'
	cat request_search_success_uniq.tmp | xargs printf "| %-45s |  %-50s  |\n"  
	echo '+-----------------------------------------------+------------------------------------------------------+'

	echo
	echo
	echo
	
	echo "4) Number of success ldap requests search operation per user:"
	echo '+-----------------------------------+------------------------------------------------------------------+'
	printf "| %-33s | %-47s | %-14s |\n"  "LOGIN" "NAME" "REQUESTS"
	echo '+-----------------------------------+------------------------------------------------------------------+'
	
	while read USER; do
		REQ_SEARCH_SUCCESS_TOTAL_BY_USER=$(grep -w "$USER" request_search_success.tmp | wc -l | cut -d" " -f1)
		USER_FORMAT=$(echo $USER | xargs printf "%-33s | %-47s")
		printf "| %-45s | %14d |\n"   "$USER_FORMAT"   $REQ_SEARCH_SUCCESS_TOTAL_BY_USER
	done < request_search_success_uniq.tmp
	echo '+-----------------------------------+------------------------------------------------------------------+'
	printf "| %-33s | %40s TOTAL: %16d |\n"   " " " " $REQ_SEARCH_SUCCESS_TOTAL
	echo '+-----------------------------------+------------------------------------------------------------------+'
	
	echo
	echo
	


	############################################  ERRRO REPORT  #######################################################################


	# filter data
	egrep '^ERROR.*ems_ldap_handler' $LOG_FILE > error_request_search.tmp
	grep ' does not exist.' error_request_search.tmp | cut -d" " -f7- | sed 's/ does not exist.//g' | sort > request_search_login_does_not_exist.tmp
	uniq request_search_login_does_not_exist.tmp > request_search_login_does_not_exist_uniq.tmp

	# totals
	REQ_SEARCH_FAIL=$(wc -l error_request_search.tmp | cut -d" " -f1)
	REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL=$(wc -l request_search_login_does_not_exist.tmp | cut -d" " -f1)
	REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_UNIQ_TOTAL=$(wc -l request_search_login_does_not_exist_uniq.tmp | cut -d" " -f1)


	echo
	echo
	echo
	echo "                ****************** ERROR REPORT ******************"
	echo
	echo
	
	echo "5) Number of failed ldap requests: $REQ_SEARCH_FAIL requests"
	echo
	echo "6) Number of failed ldap request attempts because login does not exist: $REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL requests"
	echo
	echo "7) Users does not found in the ldap request search operation: $REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_UNIQ_TOTAL users"
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



