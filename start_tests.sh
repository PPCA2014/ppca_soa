#!/bin/sh

# ---
#  PPCA_SOA
#  Inicia o barramento PPCA_SOA na porta 2301
#  Mestrado em Computação Aplicada - Universidade de Brasília
#  Turma de Construção de Software / PPCA 2014
#  Autor: Everton de Vargas Agilar
# ---

erl  -pa ebin deps/jsx/ebin test -eval "ppca_soa_tests:start()"

