%%********************************************************************
%% @title Module ems_logger_service
%% @version 1.0.0
%% @doc ems_logger_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_logger_service).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([log_file_head/1, 
		 log_file_tail/1, 
		 log_file_name/1, 
		 check_debug_mode/1, 
		 set_debug_mode/1, 
		 unset_debug_mode/1, 
		 sync/1,
		 set_level_info/1,
		 set_level_error/1,
		 checkpoint/1,
		 set_show_response/1,
		 unset_show_response/1]).

log_file_tail(Request) ->	
	case ems_logger:log_file_tail() of
		{ok, FileList} ->
			{ok, Request#request{code = 200, 
								 content_type = <<"text/file">>,
								 response_data = FileList}
			};
		Error -> 
			{error, Request#request{code = 200, 
									response_data = Error}
			}
	end.

log_file_head(Request) ->	
	case ems_logger:log_file_head() of
		{ok, FileList} ->
			{ok, Request#request{code = 200, 
								 content_type = <<"text/file">>,
								 response_data = FileList}
			};
		Error -> 
			{error, Request#request{code = 200, 
									response_data = Error}
			}
	end.


log_file_name(Request) ->	
	FileName = ems_logger:log_file_name(),
	{ok, Request#request{code = 200, 
						 response_data = ems_schema:to_json({ok, FileName})}
	}.
   
check_debug_mode(Request) ->	
	{ok, Request#request{code = 200, 
						 response_data = ems_schema:to_json({ok, ems_logger:in_debug()})}
	}.
    
set_debug_mode(Request) ->	
	{ok, Request#request{code = 200, 
						 response_data = ems_schema:to_json({ok, ems_logger:mode_debug(true)})}
	}.

unset_debug_mode(Request) ->	
	{ok, Request#request{code = 200, 
						 response_data = ems_schema:to_json({ok, ems_logger:mode_debug(false)})}
	}.

sync(Request) ->	
	ems_logger:sync(),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

set_level_info(Request) ->	
	ems_logger:set_level(info),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

set_level_error(Request) ->	
	ems_logger:set_level(error),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

set_show_response(Request) ->	
	ems_logger:show_response(true),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

unset_show_response(Request) ->	
	ems_logger:show_response(false),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.

checkpoint(Request) ->	
	ems_logger:checkpoint(),
	{ok, Request#request{code = 200, 
						 response_data = <<"{\"ok\": \"true\"}">>}
	}.
