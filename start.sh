#!/bin/sh

#********************************************************************
# @doc Inicia o barramento na porta default 2301
# @author Everton de Vargas Agilar <evertonagilar@gmail.com>
# @copyright erlangMS Team
#********************************************************************

#erl  -pa ./ebin -eval "application:start(msbus)" -boot start_sasl -config elog
erl -pa ebin deps/jsx/ebin test -eval "application:start(msbus)"
	
