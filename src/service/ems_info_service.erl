%%********************************************************************
%% @title Module info
%% @version 1.0.0
%% @doc It provides information about the bus in runtime.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_info_service).

%% Server API
-export([info/1]).

  
info(_Request) -> <<"{\"message\": \"It works!!!\"}">>.

