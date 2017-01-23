echo off
rmdir /S /Q priv\log
rmdir /S /Q priv\db
tools/rebar/rebar clean get-deps compile --config rebar_win.config
