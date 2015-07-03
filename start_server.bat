@echo off

rem # ---
rem  #  @doc Inicia o barramento PPCA_SOA na porta 2301
rem  #  @author Everton de Vargas Agilar (evertonagilar@gmail.com)
rem  #  @author Drausio Gomes dos Santos (drausiogs@gmail.com)
rem  # ---

rem set dir_ppca_soa = .\ 
rem set  dir_ppca_soa=  \desv\ppca_soa-clone\ppca_soa
rem cd %dir_ppca_soa%
erl  -pa ebin deps\jsx\ebin -eval "msbus_soa:start(2301)"
pause	
