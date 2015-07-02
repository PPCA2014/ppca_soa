@echo off
rem # ---
rem  #  PPCA_SOA
rem  #  Inicia o barramento PPCA_SOA na porta 2301
rem  #  Mestrado em Computação Aplicada - Universidade de Brasília
rem  #  Turma de Construção de Software / PPCA 2014
rem  #  Autor: Everton de Vargas Agilar (evertonagilar@gmail.com)
rem  #         Drausio Gomes dos Santos (drausiogs@gmail.com)
rem  # ---

rem set dir_ppca_soa = .\ 
set  dir_ppca_soa=  \desv\git\ppca_soa
cd %dir_ppca_soa% 
rebar clean compile

