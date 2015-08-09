@echo off
erl -pa ..\msbus\ebin deps\jsx\ebin -eval "application:start(msbus)"
