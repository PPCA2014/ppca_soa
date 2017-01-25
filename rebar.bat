echo off
set arg1=%1
set arg2=%2
set arg3=%3
set arg4=%4
set arg5=%5
rmdir /S /Q priv\log
rmdir /S /Q priv\db
tools/rebar/rebar --config rebar_win.config %arg1% %arg2% %arg3% %arg4% %arg5%
 