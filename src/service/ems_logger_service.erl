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

-export([log_file_head/1, log_file_tail/1, log_file_name/1]).

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
   
    
