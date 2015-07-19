#!/bin/sh

# ---
#  PPCA_SOA
#  Inicia o barramento PPCA_SOA na porta 2301
#  Mestrado em Computação Aplicada - Universidade de Brasília
#  Turma de Construção de Software / PPCA 2014
#  Autor: Everton de Vargas Agilar
# ---

#erl  -pa ./ebin -eval "ppca_soa:start(2301)" -boot start_sasl -config elog
erl -detached -sname ppca_soa_server -pa ebin deps/jsx/ebin -eval "ppca_soa:start()" 
	
