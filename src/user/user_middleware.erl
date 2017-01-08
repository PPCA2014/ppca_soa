-module(user_middleware).

-export([onvalidate/2]).

-include("../../include/ems_schema.hrl").

onvalidate(Operation, User) ->
	case unique(Operation, User) of
		true -> ok;
		false -> {error, ealready_exist}
	end.

unique(update, #user{id = Id, name = Name}) ->
	case ems_db:find_first(user, [{id, "=/=", Id}, {name, "==", Name}]) of
		{error, enoent} -> true;
		_ -> false
	end;
unique(insert, #user{name = Name}) ->
	case ems_db:find_first(user, [{name, "==", Name}]) of
		{error, enoent} -> true;
		_ -> false
	end.
	
