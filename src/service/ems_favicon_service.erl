%%********************************************************************
%% @title Módulo favicon
%% @version 1.0.0
%% @doc Módulo responsável pelo favicon do barramento.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_favicon_service).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

-export([execute/1]).

 
execute(Request = #request{timestamp = Timestamp})	->
	case file_info(?FAVICON_PATH) of
		{error, Reason} = Err -> {error, Request#request{code = case Reason of enoent -> 404; _ -> 400 end, 
														 reason = Reason,	
														 response_data = Err, 
														 response_header = error_http_header()}
								  };
		{FSize, MTime} -> 
			ETag = generate_etag(FSize, MTime),
			LastModified = cowboy_clock:rfc1123(MTime),
			Expires = cowboy_clock:rfc1123(ems_util:date_add_day(Timestamp, 7)),
			case file:read_file(?FAVICON_PATH) of
				{ok, FileData} -> 		
					{ok, Request#request{code = 200,
										 reason = ok,
										 content_type = <<"image/x-icon">>,
										 response_data = FileData,
										 response_header = generate_header(ETag, LastModified, Expires)}};
				{error, Reason} = Err -> 
					{error, Request#request{code = case Reason of enoent -> 404; _ -> 400 end, 
										    reason = Reason,
										    content_type = ?CONTENT_TYPE_JSON,
											response_data = ems_schema:to_json(Err), 
											response_header = error_http_header()}
					 }
			end
	end.

file_info(Filename) ->
	case file:read_file_info(Filename, [{time, universal}]) of
		{ok,{file_info, FSize, _Type, _Access, _ATime, MTime, _CTime, _Mode,_,_,_,_,_,_}} = 
			Result -> Result,
			{FSize, MTime};
		Error -> Error
	end.

generate_etag(FSize, MTime) -> integer_to_binary(erlang:phash2({FSize, MTime}, 16#ffffffff)).

generate_header(ETag, LastModified, Expires) ->
	#{
		<<"cache-control">> => <<"max-age=604800, public">>,
		<<"etag">> => ETag,
		<<"last-modified">> => LastModified,
		<<"expires">> => Expires
	}.


error_http_header() ->
	#{
		<<"content-type">> => <<"application/json; charset=utf-8">>,
		<<"cache-control">> => <<"no-cache">>
	}.
