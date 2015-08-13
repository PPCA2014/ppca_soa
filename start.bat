@echo off
erl -pa ..\msbus\ebin deps\jsx\ebin deps\poolboy\ebin -eval "application:start(msbus)"
