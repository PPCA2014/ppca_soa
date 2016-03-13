%%********************************************************************
%% @title Module ems_http_util
%% @version 1.0.0
%% @doc Module with useful functions for the TCP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(ems_tcp_util).

-export([send_data/2]).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%% @doc Sends the data to the client. Method timeout treatment
send_data(Socket, Data) ->
	case gen_tcp:send(Socket, Data) of
		{error, timeout} ->
			gen_tcp:close(Socket, Data),
			ems_logger:error("Timeout response to send to the client."),
			timeout;
        {error, closed} ->
			gen_tcp:close(Socket),
			ems_logger:error("Failed to send response to closed socket."),
			ok;
        {error, OtherSendError} ->
			gen_tcp:close(Socket),
			ems_logger:error("Error ~p to send response.", [OtherSendError]),
			OtherSendError;
		ok -> ok
	end.

