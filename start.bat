@echo off
REM
REM author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
REm
erl -pa ..\emsbus\ebin deps\jsx\ebin deps\poolboy\ebin -sname msbus -setcookie erlangms -eval "ems_bus:start()" -boot start_sasl -config .\priv\conf\elog
