%%********************************************************************
%% @title Module ems_static_file_service
%% @version 1.0.0
%% @doc static files service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_static_file_service).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

-export([execute/1]).

execute(Request = #request{url = Url,
						   if_modified_since = IfModifiedSinceReq, 
						   if_none_match = IfNoneMatchReq,
						   timestamp = Timestamp,
						   service = #service{cache_control = CacheControl,
											  expires = ExpiresMinute,
											  path = Path}}) ->
	FileName = Path ++ string:substr(Url, string:len(hd(string:tokens(Url, "/")))+2),
	case file_info(FileName) of
		{error, Reason} = Error -> 
			ems_logger:warn("ems_static_file_service file ~p does not exist.", [FileName]),
			{error, Request#request{code = case Reason of enoent -> 404; _ -> 400 end, 
									reason = Reason,	
									response_data = ems_schema:to_json(Error)}
			 };
		{FSize, MTime} -> 
			?DEBUG("ems_static_file_service loading file ~p.", [FileName]),
			MimeType = ems_http_util:mime_type(filename:extension(FileName)),
			ETag = generate_etag(FSize, MTime),
			LastModified = cowboy_clock:rfc1123(MTime),
			ExpireDate = ems_util:date_add_minute(Timestamp, ExpiresMinute + 120), % add +120min (2h) para ser horário GMT
			Expires = cowboy_clock:rfc1123(ExpireDate),
			HttpHeader = generate_header(MimeType, ETag, LastModified, Expires, CacheControl),
			case ETag == IfNoneMatchReq orelse LastModified == IfModifiedSinceReq of
				true -> {ok, Request#request{code = 304, 
											 reason = enot_modified,
											 etag = ETag,
											 response_data = <<>>, 
											 response_header = HttpHeader}
						 };
				false ->
					case file:read_file(FileName) of
						{ok, FileData} -> 
							{ok, Request#request{code = 200, 
											     reason = ok,
											     etag = ETag,
											     response_data = FileData, 
											     response_header = HttpHeader}
							};
						{error, Reason} = Error -> 
							{error, Request#request{code = case Reason of enoent -> 404; _ -> 400 end, 
												    reason = Reason,
												    response_data = ems_schema:to_json(Error)}
							}
					end
			end
		
	end.

    
    
%%====================================================================
%% Funções internas
%%====================================================================

file_info(FileName) ->
	case file:read_file_info(FileName, [{time, universal}]) of
		{ok,{file_info, FSize, _Type, _Access, _ATime, MTime, _CTime, _Mode,_,_,_,_,_,_}} = 
			Result -> Result,
			{FSize, MTime};
		Error -> Error
	end.
	

generate_header(MimeType, ETag, LastModified, Expires, CacheControl) ->
	#{
		<<"content-type">> => MimeType,
		<<"cache-control">> => CacheControl,
		<<"etag">> => ETag,
		<<"last-modified">> => LastModified,
		<<"expires">> => Expires
	}.


generate_etag(FSize, MTime) -> integer_to_binary(erlang:phash2({FSize, MTime}, 16#ffffffff)).

