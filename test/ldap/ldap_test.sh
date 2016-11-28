#!/bin/bash 
#
# ldap test search
# Author: Everton de Vargas Agilar
# Data: 16/03/2016
#
# Modo de uso:
#   ./ldap_test.sh qtd_requests host_ldap
#       qtd_requests => número de requisições simultâneas (default é 100)
#       host_ldap    => host do ldap (default é localhost)
#
#

echo "Script de teste para o módulo LDAP server do barramento"
echo "Date: $(date '+%d/%m/%Y %H:%M:%S')"
	
# Verifica se não há parâmetros
if [ $# = 0 ] || [ "$1" = "--help" ]; then
	echo "Modo de usar: ./ldap_test.sh qtd_requests host_ldap"
	echo "         qtd_requests => número de requisições simultâneas (default é 100)"
	echo "         host_ldap    => host do ldap (default é localhost:2389)"	
	exit
fi

# Parâmetro qtd_requests
if [ "$1" == "" ]; then
	COUNTER=100
else
	COUNTER=$1
fi

# Parâmetro host_ldap
if [ "$2" == "" ]; then
       HOST_LDAP="localhost:2389"
else
       HOST_LDAP="$2"
fi

echo ""
echo "Realizando $COUNTER requisições ldap para o barramento..."
echo ""
until [  $COUNTER -lt 1 ]; do
	let COUNTER-=1
	ldapsearch -xLLL -h "$HOST_LDAP" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid=geral -w 123456 >> ldap_test.log &
done



			         
