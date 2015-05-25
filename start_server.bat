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
rem set  dir_ppca_soa=  \desv\ppca_soa-clone\ppca_soa
rem cd %dir_ppca_soa%
erl  -pa ebin deps\jsx\ebin -eval "ppca_soa:start(2301)"
pause	
