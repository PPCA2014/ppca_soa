
@echo off
rem # ---
rem  #  PPCA_SOA
rem  #  Inicia o barramento PPCA_SOA na porta 2301
rem  #  Mestrado em Computação Aplicada - Universidade de Brasília
rem  #  Turma de Construção de Software / PPCA 2014
rem  #  Professor: Rodrigo Bonifacio de Almeida
rem  # ---

set dir_ppca_soa = .\ 
rem set  dir_ppca_soa=  \desv\ppca_soa-clone\ppca_soa
cd %dir_ppca_soa%
erl  -pa ebin -eval "ppca_soa:start(2301)"
pause	
