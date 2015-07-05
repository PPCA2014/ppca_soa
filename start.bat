@echo off

rem # ---
rem  #  @doc Inicia o barramento PPCA_SOA na porta 2301
rem  #  @author Everton de Vargas Agilar (evertonagilar@gmail.com)
rem  #  @author Drausio Gomes dos Santos (drausiogs@gmail.com)
rem  # ---

erl  -pa ebin deps\jsx\ebin -eval "application:start(msbus)"
