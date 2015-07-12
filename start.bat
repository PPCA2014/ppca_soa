@echo off

rem # ---
rem  #  @doc Inicia o barramento na porta default 2301
rem  #  @author Everton de Vargas Agilar (evertonagilar@gmail.com)
rem  #  @author Drausio Gomes dos Santos (drausiogs@gmail.com)
rem  # ---

erl  -pa ebin deps\jsx\ebin -eval "application:start(msbus)"
