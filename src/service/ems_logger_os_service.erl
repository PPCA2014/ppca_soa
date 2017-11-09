%%********************************************************************
%% @title Module ems_logger_os_service
%% @version 1.0.0
%% @doc ems_logger_os_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_logger_os_service).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([log_file_head/1, 
		 log_file_tail/1,
		 log_file_name/1]).

log_file_tail(Request = #request{service = #service{filename = Filename}}) ->	
	case ems_util:tail_file(Filename, 160) of
		{ok, FileList} -> 
			{ok, Request#request{code = 200, 
								 content_type = <<"text/file">>,
								 response_data = FileList}
			};
		Error -> 
			{error, Request#request{code = 400, 
									response_data = Error}
			}
	end.

log_file_head(Request = #request{service = #service{filename = Filename}}) ->	
	case ems_util:head_file(Filename, 160) of
		{ok, FileList} -> 
			{ok, Request#request{code = 200, 
								 content_type = <<"text/file">>,
								 response_data = FileList}
			};
		Error -> 
			{error, Request#request{code = 400, 
									response_data = Error}
			}
	end.

log_file_name(Request = #request{service = #service{filename = Filename}}) ->	
	{ok, Request#request{code = 200, 
						 response_data = ems_schema:to_json({ok, Filename})}
	}.
   
