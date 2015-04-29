#!/bin/sh

# ---
#  PPCA_SOA
#  Inicia o barramento PPCA_SOA na porta 2301
#  Mestrado em Computação Aplicada - Universidade de Brasília
#  Turma de Construção de Software / PPCA 2014
#  Professor: Rodrigo Bonifacio de Almeida
# ---

#erl  -pa ./ebin -eval "ppca_soa:start(2301)" -boot start_sasl -config elog
erl -pa ebin deps/*/ebin -eval "ppca_soa:start(2301)"
	
