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
# 03/03/2017  Everton Agilar     Add --showlogs option and improve ldap report
#
#
#
#
########################################################################################################

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

# Script version
VERSION=1.0.0
LDAP_SERVER="$LINUX_IP_SERVER:2389"
CURRENT_DIR=$(pwd)
TMP_DIR="/tmp/erlangms/ldap/audit_ldap_log_$(date '+%d%m%Y_%H%M%S')_$$"
EMS_NODE="ems-bus"
ENVIRONMENT="$LINUX_DESCRIPTION IP $LINUX_IP_SERVER "
MMIN="1440"
CURRENT_DATE=$(date '+%d/%m/%Y %H:%M:%S')
REPORT_FILE="$TMP_DIR/report_audit_ldap_log_$(date '+%d%m%Y_%H%M%S').txt"
SUB_TITLE_REPORT="LAST DAY OF OPERATION"
SHOW_LOGS="false"
SEND_EMAIL="false"

# Log destination parameter
LOG_YEAR=$(date '+%Y')
LOG_DEST_BASE="/var/log/$EMS_NODE"
LOG_DEST="$LOG_DEST_BASE/$(ls $LOG_DEST_BASE/ | sed '/ems_bus@.*/ !d; 1q')/$LOG_YEAR"
LOG_FILE_TMP="$TMP_DIR/full_log_file.tmp"
LOG_FILE="$TMP_DIR/full_log_file_filtered.tmp"

# SMTP parameter
SMTP_SERVER="mail.unb.br"
SMTP_PORT=587
SMTP_DE="erlangms@unb.br"
#SMTP_PARA="evertonagilar@unb.br,evertonagilar@unb.br,felipesantos@unb.br"
SMTP_PARA="evertonagilar@unb.br,evertonagilar@unb.br"
SMTP_PASSWD=erl1523


# tmpfiles go to /$TMP_DIR
mkdir -p $TMP_DIR && cd $TMP_DIR


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
	echo "ERLANGMS Node: $EMS_NODE"
	echo
	
	if [ "$MMIN" == "1440" ]; then
		echo "Analyzing logfiles between $DATE_INIT (1 day ago) and $CURRENT_DATE"
	else
		echo "Analyzing logfiles between $DATE_INIT ($MMIN minutes ago) and $CURRENT_DATE"
	fi

	# copies the log files to $TMP_DIR tmp and returns a list of them
	LOG_FILE_LIST=$(find "$LOG_DEST/" -type f -mmin "-$MMIN" -follow -name "*.log" -size +0 -exec cp {} $TMP_DIR \; -exec basename  {} \;) 

	if [ -z "$LOG_FILE_LIST" ]; then
		echo "No logfiles to analyze."
		exit 1
	else
		echo "Reading and parsing the following logfiles:"
		echo $LOG_FILE_LIST  | tr " " "\n" | awk '{
			printf "   * "$1 ;
			printf "  -  ";
			system("du -h "$1 "| cut -f1");
		}'
	fi 
 

	# concat all logfiles of ems_ldap_handler and create $LOG_FILE_TMP
	# remove characters control of the line colors too
	cat $TMP_DIR/*.log | grep "ems_ldap_handler" | sed -r '/ERROR/ { s/.{7}// ;  s/.{4}$// } ; y/áÁàÀãÃâÂéÉêÊíÍóÓõÕôÔúÚçÇ/aAaAaAaAeEeEiIoOoOoOuUcC/ ; s/<//g; s/>//g ;' > $LOG_FILE_TMP
	
	

	# filter only the log lines of the defined interval	$DATE_INIT
	awk -v DT_INIT="$DATE_INIT" '{
			DT_LOG=$2" "$3;
			if (DT_LOG >= DT_INIT){
				print; 
			}
	}' $LOG_FILE_TMP > $LOG_FILE
	

	# Check if show logfiles content
	if [ "$SHOW_LOGS" == "true" ]; then
		echo
		echo
		echo "Show content os log files:"
		cat $LOG_FILE
		echo
	fi
	
	echo


	# Print title
	echo
	echo
	echo "                                 ERLANGMS LDAP REPORT FOR THE"
	if [ "$MMIN" == "1440" ]; then
		SUB_TITLE_REPORT="LAST DAY OF OPERATION"
	elif [ "$MMIN" == "1" ]; then
		SUB_TITLE_REPORT="LAST $MMIN MINUTE OF OPERATION"
	else 
		SUB_TITLE_REPORT="LAST $MMIN MINUTES OF OPERATION"
	fi
	SUB_TITLE_SIZE=$[$(echo $SUB_TITLE_REPORT | wc -c) / 2]
	printf "%*s\n" $[47 + $SUB_TITLE_SIZE] "$SUB_TITLE_REPORT"
	printf "%*s\n" $[49 + $SUB_TITLE_SIZE] "$ENVIRONMENT"
	echo
	


	# filter data
	egrep '^INFO.*ems_ldap_handler search' $LOG_FILE > info_request_search.tmp
	grep 'success' info_request_search.tmp | cut -d" " -f7- | sed 's/ success.//g' | sort > request_search_success.tmp
	uniq request_search_success.tmp > request_search_success_uniq.tmp
	egrep '^ERROR.*ems_ldap_handler' $LOG_FILE > error_request_search.tmp
	grep ' does not exist.' error_request_search.tmp | cut -d" " -f7- | sed 's/ does not exist.//g' | sort > request_search_login_does_not_exist.tmp
	uniq request_search_login_does_not_exist.tmp > request_search_login_does_not_exist_uniq.tmp


	# totals
	REQ_SEARCH_TOTAL_INFO=$(wc -l info_request_search.tmp | cut -d" " -f1)
	REQ_SEARCH_SUCCESS_TOTAL=$(wc -l request_search_success.tmp | cut -d" " -f1)
	REQ_SEARCH_SUCCESS_UNIQ_TOTAL=$(wc -l request_search_success_uniq.tmp | cut -d" " -f1)
	REQ_SEARCH_FAIL=$(wc -l error_request_search.tmp | cut -d" " -f1)
	REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL=$(wc -l request_search_login_does_not_exist.tmp | cut -d" " -f1)
	REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_UNIQ_TOTAL=$(wc -l request_search_login_does_not_exist_uniq.tmp | cut -d" " -f1)
	REQ_LDAP_TOTAL=$[REQ_SEARCH_TOTAL_INFO + REQ_SEARCH_FAIL]

	############################################  PRINT REPORT  #######################################################################
	
	echo "1) Number of ldap requests (success + failed): $REQ_LDAP_TOTAL requests"
	echo
	echo "2) Number of success ldap requests: $REQ_SEARCH_SUCCESS_TOTAL requests"
	echo

	echo "3) Number of success search operation per user:"
	echo '+-----------------------------------+------------------------------------------------------------------+'
	printf "| %-33s | %-47s | %14s |\n"  "LOGIN" "NAME" "REQUESTS" " "
	echo '+-----------------------------------+------------------------------------------------------------------+'
	
	TMP_NUMBER_SUCCESS_SEARCH_PER_USER=$TMP_DIR/tmp_number_success_search_per_user.tmp
	while read USER; do
		REQ_SEARCH_SUCCESS_TOTAL_BY_USER=$(grep -w "$USER" request_search_success.tmp | wc -l | cut -d" " -f1)
		USER_FORMAT=$(echo $USER | xargs printf "%-33s | %-47s")
		printf "| %-45s | %14d |\n"   "$USER_FORMAT"   $REQ_SEARCH_SUCCESS_TOTAL_BY_USER >> $TMP_NUMBER_SUCCESS_SEARCH_PER_USER
	done < request_search_success_uniq.tmp
	if [ -f $TMP_NUMBER_SUCCESS_SEARCH_PER_USER ]; then
		cat $TMP_NUMBER_SUCCESS_SEARCH_PER_USER | sort -t'|' -n -r  -nk4
	else
		printf "| %-33s | %64s |\n"   "no records found"   " "
	fi
	echo '+-----------------------------------+------------------------------------------------------------------+'
	printf "| %-33s | %40s TOTAL: %16d |\n"   " " " " $REQ_SEARCH_SUCCESS_TOTAL
	echo '+-----------------------------------+------------------------------------------------------------------+'
	
	echo
	echo "4) Number of failed ldap requests: $REQ_SEARCH_FAIL requests"
	echo
	echo "5) Number of failed ldap request attempts because login does not exist: $REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL requests"
	echo
	echo "6) Users not found in the search operation: $REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_UNIQ_TOTAL users"
	echo '+-----------------------------------------------+------------------------------------------------------+'
	printf "| %-45s | %19s %33s|\n"  "LOGIN"   "REQUESTS"   " "                                                               
	echo '+-----------------------------------------------+------------------------------------------------------+'
	TMP_USERS_NOT_FOUND_SEARCH_OP=$TMP_DIR/tmp_users_not_found_search_op.tmp
	while read USER; do
		USER=$(echo $USER | tr '"' '\0')
		REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL_BY_USER=$(grep -w "$USER" request_search_login_does_not_exist.tmp | wc -l | cut -d" " -f1)
		printf "| %-45s | %19d         %25s|\n"   "$USER"   $REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL_BY_USER " " >> $TMP_USERS_NOT_FOUND_SEARCH_OP
	done <  request_search_login_does_not_exist_uniq.tmp
	if [ -f $TMP_USERS_NOT_FOUND_SEARCH_OP ]; then
		cat $TMP_USERS_NOT_FOUND_SEARCH_OP | sort -t'|' -n -r  -nk3 
	else
		printf "| %-45s | %19s         %25s|\n"   "no records found"   "" " "
	fi
	echo '+-----------------------------------------------+------------------------------------------------------+'
	printf "| %-45s | TOTAL: %12d %33s|\n"   " " $REQ_SEARCH_FAIL_LOGIN_DOES_NOT_EXIST_TOTAL " "
	echo '+-----------------------------------------------+------------------------------------------------------+'
	echo
	

}


send_email(){	
	REPORT_CONTENT=$(cat $REPORT_FILE)
	TITULO_MSG="ERLANGMS LDAP Log Analysis Report - $LINUX_IP_SERVER" 
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
				<!--[if mso]>
				 <center>
				 <table><tr><td width=\"980\">
				<![endif]-->
				 <div style=\"max-width:980px; margin:0 auto;\"> 
					<pre>$REPORT_CONTENT<pre>
				</div>
				<!--[if mso]>
				 </td></tr></table>
				 </center>
				<![endif]-->
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
except Exception as e:
	print(e)
EOF
	
}	


# /////////////////// main /////////////////

# check parameter --showlogs
if [ "$2" == "--showlogs" ] || [ "$3" == "--showlogs" ]; then
  SHOW_LOGS="true"		
fi

# check parameter --sendemail
if [ "$2" == "--sendemail" ] || [ "$3" == "--sendemail" ]; then
  SEND_EMAIL="true"
fi

# generate report
generate_report

# send e-mail to admins
[ "$SEND_EMAIL" == "true" ] && send_email && echo "This report was send to administrators."

# remove tmp dir and back to CURRENT_DIR
cd $CURRENT_DIR
rm -rf $TMP_DIR/



