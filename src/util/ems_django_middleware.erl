-module(ems_django_middleware).

-export([onrequest/1]).

-include("include/ems_schema.hrl").

onrequest(Request) ->
	io:format("ola!!!!\n"),
	{ok, Request}.

	
