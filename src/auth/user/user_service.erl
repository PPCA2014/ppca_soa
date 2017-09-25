-module(user_service).

-export([onvalidate/2, perfil_by_user/1, loader_sync/1, loader_last_update/1, loader_pause/1, loader_resume/1, loader_size_table/1]).

-include("include/ems_schema.hrl").

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
	
perfil_by_user(Request) -> 
	UserId = ems_request:get_param_url(<<"id">>, null, Request),
	Result = ems_user_perfil:find_by_user(UserId),
	case Result of
		{ok, JsonData} ->
			{ok, Request#request{code = 200,
								 reason = ok,
								 response_data = ems_schema:to_json(JsonData)}};
		{error, Reason} = Error ->
			{error, Request#request{code = 400,
									reason = Reason,
									response_data = ems_schema:to_json(Error)}}
	end.
	
loader_sync(Request) ->	
	ems_user_loader:sync(),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

loader_last_update(Request) ->	
	{ok, Request#request{code = 200, 
						 response_data = ems_schema:to_json({ok, ems_util:timestamp_str(ems_user_loader:last_update())})}
	}.

loader_pause(Request) ->	
	ems_user_loader:pause(),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

loader_resume(Request) ->	
	ems_user_loader:resume(),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

loader_size_table(Request) ->	
	{ok, Request#request{code = 200, 
						 response_data = ems_schema:to_json({ok, ems_user_loader:size_table()})}
	}.
