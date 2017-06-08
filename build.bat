@echo off
cls
echo Build ems-bus with rebar...
rmdir  /S /Q deps\cowboy
rmdir  /S /Q deps\cowlib
rmdir  /S /Q deps\ranch
rmdir  /S /Q deps\oauth2
if not exist priv\log goto DeleteDBFolder
rmdir /S /Q priv\log
:DeleteDBFolder
if not exist priv\db goto BuildWithRebar
rmdir /S /Q priv\db
:BuildWithRebar
tools/rebar/rebar clean get-deps compile --config rebar_win.config
