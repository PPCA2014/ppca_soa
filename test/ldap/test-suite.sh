#!/bin/bash

ldap_search() {
    FILTER=${1:-'"objectClass=*"'}
    ARGS=$2' '$3' '$4$' '$5
    local CMD='ldapsearch -b "dc=synrc,dc=com" -h localhost -p 2389 -D "uid=evertonagilar,dc=synrc,dc=com" -w secret '$FILTER' '$ARGS
    eval $CMD
}

run_test() {
    echo -n " - "$1"... "
    RES=$($2)
    MOD=$(< $3)
    ( [ "$RES" = "$MOD" ] && echo "ok." ) || echo "Failed!"
}


run_test "Search for 'objectClass=*'" "ldap_search" "000-search.txt"

#run_test "Search for '(&(uid=*)(cn=Ma*))'" "ldap_search \"(&(uid=*)(cn=Ma*))\"" "001-search.txt"

#run_test "Search with attribute selection" "ldap_search \"sn=*\" cn sn" "002-search.txt"

#run_test "Search with size limit" "ldap_search \"sn=*\" cn sn -z1" "003-search.txt"

