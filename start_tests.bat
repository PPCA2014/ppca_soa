@echo off
rem # ---
rem  #  PPCA_SOA
rem  #  Inicia o barramento PPCA_SOA na porta 2301
rem  #  Mestrado em Computação Aplicada - Universidade de Brasília
rem  #  Turma de Construção de Software / PPCA 2014
rem  #  Autor: Everton de Vargas Agilar (evertonagilar@gmail.com)
rem  # ---

erl  -pa ebin deps\jsx\ebin test -eval "ppca_soa_tests:start()"
pause	
