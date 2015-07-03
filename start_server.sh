#!/bin/sh

#********************************************************************
# @doc Módulo responsável pelo gerenciamendo dos nós do barramento.
# @author Everton de Vargas Agilar <evertonagilar@gmail.com>
# @copyright erlangMS Team
#********************************************************************

#erl  -pa ./ebin -eval "ppca_soa:start(2301)" -boot start_sasl -config elog
erl -pa ebin deps/jsx/ebin -eval "msbus_soa:start()"
	
