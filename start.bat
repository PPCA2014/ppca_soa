REM
REM start msbus and build before if necessary
REM author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
REm
@echo off
./build.bat
erl -pa ..\msbus\ebin deps\jsx\ebin deps\poolboy\ebin -sname msbus -setcookie erlangms -eval "msbus:start()" -boot start_sasl -config .\priv\conf\elog
