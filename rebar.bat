echo off
set arg1=%1
set arg2=%2
rmdir /S /Q priv\log
rmdir /S /Q priv\db
tools/rebar/rebar --config rebar_win.config %arg1% %arg2%
