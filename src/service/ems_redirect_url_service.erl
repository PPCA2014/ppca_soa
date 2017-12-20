%%********************************************************************
%% @title Module ems_redirect_url_service
%% @version 1.0.0
%% @doc It redirect urls :)
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_redirect_url_service).

-include("include/ems_schema.hrl").
-include("include/ems_config.hrl").

-export([execute/1]).
  
execute(Request = #request{service = #service{redirect_url = RedirectUrl},
						   querystring = Querystring,
						   payload = Payload,
						   accept = Accept,
						   content_type = ContentType,
						   cache_control = CacheControl,
						   if_modified_since = IfModifiedSinceReq, 
						   if_none_match = IfNoneMatchReq,
						   authorization = Authorization}) -> 
	RedirectUrl2 = iolist_to_binary([RedirectUrl, <<"?">>, Querystring]),
	?DEBUG("Redirect to ~p.", [RedirectUrl2]),
	{ok, Request#request{code = 302, 
							 response_data = Payload,
							 response_header = #{
													<<"location">> => RedirectUrl2,
													<<"content-type">> => format_header(ContentType),
													<<"cache-control">> => format_header(CacheControl),
													<<"If-None-Match">> => format_header(IfNoneMatchReq),
													<<"If-Modified-Since">> => format_header(IfModifiedSinceReq),
													<<"Authorization">> => format_header(Authorization),
													<<"accept">> => format_header(Accept)
												}
							}
	}.

format_header(undefined) -> <<>>;
format_header(V) -> V.
