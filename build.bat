@echo off
cls
echo Build ems-bus with rebar...
rmdir  /S /Q deps\cowboy
rmdir  /S /Q deps\cowlib
rmdir  /S /Q deps\erlydtl
rmdir  /S /Q deps\jesse
rmdir  /S /Q deps\json_rec
rmdir  /S /Q deps\jsx
rmdir  /S /Q deps\mochiweb
rmdir  /S /Q deps\ranch
rmdir  /S /Q deps\oauth2
rmdir  /S /Q deps\parse_trans
rmdir  /S /Q deps\poolboy
rmdir  /S /Q deps\rfc3339
if not exist priv\log goto DeleteDBFolder
rmdir /S /Q priv\log
:DeleteDBFolder
if not exist priv\db goto BuildWithRebar
rmdir /S /Q priv\db
:BuildWithRebar
tools/rebar/rebar clean get-deps compile --config rebar_win.config
