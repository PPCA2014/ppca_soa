%%********************************************************************
%% @title Módulo msbus_ldap_util
%% @version 1.0.0
%% @doc Module with useful functions for the LDAP server.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_ldap_util).

-compile(export_all).

-include("../../include/msbus_config.hrl").
-include("../../include/msbus_schema.hrl").
-include("../../include/msbus_http_messages.hrl").
-include("../../include/LDAP.hrl").

encode_request(Socket, RequestBin, WorkerSend) ->
	case decode_ldap_request(RequestBin) of
		{MessageID, ProtocolOp} -> 
			RID = os:system_time(),
			Timestamp = calendar:local_time(),
			T1 = msbus_util:get_milliseconds(),
			Rowid = <<"GET#/ldap">>,
			{ok, #request{
				rid = RID,
				rowid = Rowid,
				t1 = T1, 
				payload = ProtocolOp, 
				timestamp = Timestamp,
				worker_send = WorkerSend
			}};
		Error -> Error
	end.



decode_ldap_request(RequestBin) ->
	case asn1rt:decode('LDAP', 'LDAPMessage', RequestBin) of
        {ok, {'LDAPMessage', MessageID, ProtocolOp, P}} ->
			io:format("MessageID: ~p~n  ProtocolOp: ~p  P: ~p~n", [MessageID, ProtocolOp, P]),
			{MessageID, ProtocolOp};
		Error -> 
			io:format("erro...~p\n", [Error]),
			Error
    end.
