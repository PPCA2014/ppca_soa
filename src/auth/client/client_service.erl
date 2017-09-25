-module(client_service).

-export([onvalidate/2, loader_sync/1, loader_last_update/1, loader_pause/1, loader_resume/1, loader_size_table/1]).

-include("include/ems_schema.hrl").

onvalidate(Operation, Client) ->
	case unique(Operation, Client) of
		true -> ok;
		false -> {error, ealready_exist}
	end.

unique(update, #client{id = Id, name = Name}) ->
	case ems_db:find_first(client, [{id, "=/=", Id}, {name, "==", Name}]) of
		{error, enoent} -> true;
		_ -> false
	end;
unique(insert, #client{name = Name}) ->
	case ems_db:find_first(client, [{name, "==", Name}]) of
		{error, enoent} -> true;
		_ -> false
	end.
	
loader_sync(Request) ->	
	ems_client_loader:sync(),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

loader_last_update(Request) ->	
	{ok, Request#request{code = 200, 
						 response_data = ems_schema:to_json({ok, ems_util:timestamp_str(ems_client_loader:last_update())})}
	}.

loader_pause(Request) ->	
	ems_client_loader:pause(),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

loader_resume(Request) ->	
	ems_client_loader:resume(),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

loader_size_table(Request) ->	
	{ok, Request#request{code = 200, 
						 response_data = ems_schema:to_json({ok, ems_client_loader:size_table()})}
	}.
