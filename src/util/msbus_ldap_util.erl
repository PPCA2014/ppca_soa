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
-include("../../include/LDAP.hrl").

encode_request(Socket, RequestBin, WorkerSend) ->
	case decode_ldap_message(RequestBin) of
		{error, Reason} -> 
			{error, Reason};
		LdapMsg -> 
			RID = os:system_time(),
			Timestamp = calendar:local_time(),
			T1 = msbus_util:get_milliseconds(),
			Rowid = <<"GET#/ldap">>,
			{ok, #request{
				rid = RID,
				rowid = Rowid,
				versao_http = "LDAPv3",
				type = "GET",
				uri = "/ldap",
				url = "/ldap",
				socket = Socket, 
				t1 = T1, 
				payload = LdapMsg, 
				timestamp = Timestamp,
				authorization = "",
				worker_send = WorkerSend,
				protocolo = ldap
			}}
	end.

encode_response(MessageID, Msg) ->
	Response = #'LDAPMessage'{messageID = MessageID,
							  protocolOp = Msg,
							  controls = asn1_NOVALUE},
    case asn1rt:encode('LDAP', 'LDAPMessage', Response) of
        {ok, Result} -> Result;
        {error, Reason} -> {error, Reason}
    end.


decode_ldap_message(RequestBin) ->
	case asn1rt:decode('LDAP', 'LDAPMessage', RequestBin) of
        {ok, {'LDAPMessage', _MessageID, _ProtocolOp, _} = LdapMsg} ->
			LdapMsg;
		{error, Reason} -> 
			{error, Reason}
    end.


