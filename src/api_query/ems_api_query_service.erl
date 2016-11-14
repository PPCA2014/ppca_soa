%%********************************************************************
%% @title Module ems_api_query_service
%% @version 1.0.0
%% @doc It provides dynamic_view service for relational databases.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_service).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").


%% Client API
-export([find/1, find_by_id/1, insert/1, update/1]).

 
%%====================================================================
%% Client API
%%====================================================================
 
find(Request) -> 
	Result = execute_command(find, Request),
	{ok, Request#request{code = 200,
						 response_data = ems_schema:to_json(Result)}}.


find_by_id(Request) ->
	Result = execute_command(find_by_id, Request),
	{ok, Request#request{code = 200,
						 response_data = ems_schema:to_json(Result)}}.

insert(Request) ->
	Result = execute_command(insert, Request),
	{ok, Request#request{code = 200,
						 response_data = ems_schema:to_json(Result)}}.

update(Request) ->
	Result = execute_command(update, Request),
	{ok, Request#request{code = 200,
						 response_data = ems_schema:to_json(Result)}}.

  
    
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
					insert -> do_insert(Request, Datasource2);
					update -> do_update(Request, Datasource2)
				end,
				ems_db:release_connection(Datasource2),
				Result;
			{error, Reason} ->	{error, Reason}
		end
	catch
		_Exception:Reason2 -> {error, Reason2}
	end.


do_find(#request{querystring_map = QuerystringMap, 
				 service = #service{debug = Debug}},
				 Datasource) ->
	FilterJson = maps:get(<<"filter">>, QuerystringMap, <<>>),
	Fields = binary_to_list(maps:get(<<"fields">>, QuerystringMap, <<>>)),
	Limit = binary_to_integer(maps:get(<<"limit">>, QuerystringMap, <<"100">>)),
	Offset = binary_to_integer(maps:get(<<"offset">>, QuerystringMap, <<"1">>)),
	Sort = binary_to_list(maps:get(<<"sort">>, QuerystringMap, <<>>)),
	ems_api_query:find(FilterJson, Fields, Limit, Offset, Sort, Datasource, Debug).


do_find_by_id(Request = #request{querystring_map = QuerystringMap, 
								 service = #service{debug = Debug}}, 
			 Datasource) ->
	Id = ems_request:get_param_url(<<"id">>, 0, Request),
	Fields = binary_to_list(maps:get(<<"fields">>, QuerystringMap, <<>>)),
	ems_api_query:find_by_id(Id, Fields, Datasource, Debug).


do_insert(#request{payload_map = Payload, service = Service}, Datasource) ->
	ems_api_query:insert(Payload, Service, Datasource).


do_update(Request = #request{payload_map = Payload, service = Service}, Datasource) ->
	Id = ems_request:get_param_url(<<"id">>, 0, Request),
	ems_api_query:update(Id, Payload, Service, Datasource).


