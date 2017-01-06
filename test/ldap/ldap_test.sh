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

# Verifica se não há parâmetros
if [ $# != 3 ] || [ "$1" = "--help" ]; then
	echo "Modo de usar: ./ldap_test.sh qtd_requests host_ldap user"
	echo "         qtd_requests => número de requisições simultâneas (default é 100)"
	echo "         host_ldap    => host do ldap (default é localhost:2389)"	
	echo "         user         => user da pesquisa"	
	exit
fi

# Parâmetro qtd_requests
if [ "$1" == "" ]; then
	COUNTER=1
else
	COUNTER=$1
fi

# Parâmetro host_ldap
if [ "$2" == "" ]; then
       HOST_LDAP="localhost:2389"
else
       HOST_LDAP="$2"
fi

# Parâmetro user
if [ "$3" == "" ]; then
       USER="geral"
else
       USER="$3"
fi

echo "Informe a senha do usuário para autenticação:"
read -s USER_PASSWD

if [ $COUNTER == 1 ]; then
	echo ""
	echo "Realizando requisição ldap para o barramento com user $USER..."
	echo ldapsearch -xLLL -h "$HOST_LDAP" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid=$USER
	echo ""
	ldapsearch -xLLL -h "$HOST_LDAP" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid=$USER -w $USER_PASSWD
else
	echo ""
	echo "Realizando $COUNTER requisições ldap para o barramento com user $USER..."
	echo ldapsearch -xLLL -h "$HOST_LDAP" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid=$USER
	echo ""
	until [  $COUNTER -lt 1 ]; do
		let COUNTER-=1
		ldapsearch -xLLL -h "$HOST_LDAP" -b 'dc=unb,dc=br' -D 'cn=admin,dc=unb,dc=br' uid=$USER -w 123456 >> ldap_test.log &
	done
fi
			         
