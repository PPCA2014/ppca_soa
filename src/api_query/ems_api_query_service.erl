%%********************************************************************
%% @title Module ems_api_query_service
%% @version 1.0.0
%% @doc Api query service for databases.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_service).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").


%% Client API
-export([find/1, find_by_id/1, find_by_owner/1, insert/1, update/1, delete/1]).

 
%%====================================================================
%% Client API
%%====================================================================
 
find(Request) -> execute_command(find, Request).

find_by_id(Request) ->	execute_command(find_by_id, Request).

find_by_owner(Request) ->	execute_command(find_by_owner, Request).

insert(Request) -> execute_command(insert, Request).

update(Request) -> execute_command(update, Request).

delete(Request) -> execute_command(delete, Request).
  
    
%%====================================================================
%% Internal functions
%%====================================================================


execute_command(Command, Request = #request{service = #service{datasource = Datasource}}) ->
	try
		case ems_db:get_connection(Datasource) of
			{ok, Datasource2} -> 
				Result = case Command of
					find -> do_find(Request, Datasource2);
					find_by_id -> do_find_by_id(Request, Datasource2);
					find_by_owner -> do_find_by_owner(Request, Datasource2);
					insert -> do_insert(Request, Datasource2);
					update -> do_update(Request, Datasource2);
					delete -> do_delete(Request, Datasource2)
				end,
				ems_db:release_connection(Datasource2),
				case Result of
					{ok, JsonData} ->
						{ok, Request#request{code = 200,
											 reason = ok,
											 response_data = JsonData}};
					{error, Reason} = Error ->
						{error, Request#request{code = 400,
												reason = Reason,
												response_data = ems_schema:to_json(Error)}}
				end;
			{error, Reason2} = Error2 ->	
				{error, Request#request{code = 400,
										reason = Reason2,
										response_data = ems_schema:to_json(Error2)}}

		end
	catch
		_Exception:Reason3 -> 
			% Pode ocorrer einvalid_driver_datasource
			Error3 = {error, Reason3},
			{error, Request#request{code = 400,
									reason = Reason3,
									response_data = ems_schema:to_json(Error3)}}
	end.


do_find(#request{querystring_map = QuerystringMap}, Datasource) ->
	FilterJson = maps:get(<<"filter">>, QuerystringMap, <<>>),
	Fields = binary_to_list(maps:get(<<"fields">>, QuerystringMap, <<>>)),
	Limit = binary_to_integer(maps:get(<<"limit">>, QuerystringMap, <<"100">>)),
	Offset = binary_to_integer(maps:get(<<"offset">>, QuerystringMap, <<"1">>)),
	Sort = binary_to_list(maps:get(<<"sort">>, QuerystringMap, <<>>)),
	ems_api_query:find(FilterJson, Fields, Limit, Offset, Sort, Datasource).
	

do_find_by_id(Request = #request{querystring_map = QuerystringMap}, Datasource) ->
	Id = ems_util:get_param_url(<<"id">>, 0, Request),
	case Id > 0 of
		true ->
			Fields = binary_to_list(maps:get(<<"fields">>, QuerystringMap, <<>>)),
			ems_api_query:find_by_id(Id, Fields, Datasource);
		false -> {error, enoent}
	end.

do_find_by_owner(Request = #request{querystring_map = QuerystringMap}, Datasource) ->
	FilterJson = maps:get(<<"filter">>, QuerystringMap, <<>>),
	Fields = binary_to_list(maps:get(<<"fields">>, QuerystringMap, <<>>)),
	Limit = binary_to_integer(maps:get(<<"limit">>, QuerystringMap, <<"100">>)),
	Offset = binary_to_integer(maps:get(<<"offset">>, QuerystringMap, <<"1">>)),
	Sort = binary_to_list(maps:get(<<"sort">>, QuerystringMap, <<>>)),
	IdOwner = ems_util:get_param_url(<<"id">>, 0, Request),
	ems_api_query:find_by_owner(FilterJson, Fields, Limit, Offset, Sort, IdOwner, Datasource).



do_insert(#request{payload_map = Payload, service = Service}, Datasource) ->
	ems_api_query:insert(Payload, Service, Datasource).


do_update(Request = #request{payload_map = Payload, service = Service}, Datasource) ->
	Id = ems_util:get_param_url(<<"id">>, 0, Request),
	ems_api_query:update(Id, Payload, Service, Datasource).


do_delete(Request = #request{service = Service}, Datasource) ->
	Id = ems_util:get_param_url(<<"id">>, 0, Request),
	ems_api_query:delete(Id, Service, Datasource).

