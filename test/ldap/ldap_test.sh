#!/bin/bash 
#
# ldap test search
# Author: Everton de Vargas Agilar
# Data: 16/03/2016
#

if [ $1 == "" ]; then
	echo "Uso: ldap_test.sh count_reqs host"
else
	 COUNTER=$1
	 echo "$COUNTER asynchronous ldap requests to the ErlangMS..."
	 until [  $COUNTER -lt 0 ]; do
		 let COUNTER-=1
		 ldapsearch -xLLL -h $2:2389 -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid=geral -w 123456 
	 done
	 echo "done!"
fi


			         
