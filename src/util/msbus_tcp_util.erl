%%********************************************************************
%% @title Module msbus_http_util
%% @version 1.0.0
%% @doc Module with useful functions for the TCP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_tcp_util).

-export([send_request/2]).

-include("../../include/msbus_config.hrl").
-include("../../include/msbus_schema.hrl").

%% @doc Sends the data to the client. Method timeout treatment
send_request(Socket, Response) ->
	case gen_tcp:send(Socket, [Response]) of
		{error, timeout} ->
			gen_tcp:close(Socket),
			msbus_logger:error("Timeout response to send to the client."),
			timeout;
        {error, closed} ->
			gen_tcp:close(Socket),
			msbus_logger:error("Failed to send response to closed socket."),
			ok;
        {error, OtherSendError} ->
			gen_tcp:close(Socket),
			msbus_logger:error("Error ~p to send response.", [OtherSendError]),
			OtherSendError;
		ok -> ok
	end.

