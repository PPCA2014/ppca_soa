#!/bin/sh
#
# build msbus with rebar
# author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
#
rm -Rf Mnesia*
tools/rebar/rebar get-deps compile
