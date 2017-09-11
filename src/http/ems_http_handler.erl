%%********************************************************************
%% @title Module ems_http_server
%% @version 1.0.0
%% @doc Main module HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_handler).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([init/2, terminate/3]).


init(CowboyReq, Opts) ->
	?DEBUG("ems_http_handler new request: ~p.", [CowboyReq]),
	case ems_http_util:encode_request_cowboy(CowboyReq, self()) of
		{ok, Request = #request{t1 = T1}} -> 
			?DEBUG("ems_http_handler delivers the request to the dispatcher."),
			case ems_dispatcher:dispatch_request(Request) of
				{ok, request, Request2 = #request{code = Code,
												  response_header = ResponseHeader,
												  response_data = ResponseData,
												  service = #service{content_type = ContentTypeService,
																	 cache_control = CacheControl}}} ->
					Response = cowboy_req:reply(Code, 
												ResponseHeader#{
													<<"server">> => ?SERVER_NAME,
													<<"content-type">> => maps:get(<<"content-type">>, ResponseHeader, ContentTypeService),
													<<"cache-control">> => maps:get(<<"cache-control">>, ResponseHeader, CacheControl),
													<<"access-control-allow-origin">> => ?ACCESS_CONTROL_ALLOW_ORIGIN,
													<<"access-control-max-age">> => ?ACCESS_CONTROL_MAX_AGE,
													<<"access-control-allow-headers">> => ?ACCESS_CONTROL_ALLOW_HEADERS,
													<<"access-control-allow-methods">> => ?ACCESS_CONTROL_ALLOW_METHODS,
													<<"access-control-expose-headers">> => ?ACCESS_CONTROL_EXPOSE_HEADERS}, 
												ResponseData, 
												CowboyReq),
					ems_logger:log_request(Request2);
				{ok, request, Request2 = #request{code = Code,
												  response_header = ResponseHeader,
												  response_data = ResponseData}} ->
					Response = cowboy_req:reply(Code, 
												ResponseHeader#{<<"server">> => ?SERVER_NAME,
																<<"content-type">> => ?CONTENT_TYPE_JSON,
																<<"cache-control">> => ?CACHE_CONTROL_NO_CACHE,
																<<"access-control-allow-origin">> => ?ACCESS_CONTROL_ALLOW_ORIGIN,
																<<"access-control-max-age">> => ?ACCESS_CONTROL_MAX_AGE,
																<<"access-control-allow-headers">> => ?ACCESS_CONTROL_ALLOW_HEADERS,
																<<"access-control-allow-methods">> => ?ACCESS_CONTROL_ALLOW_METHODS,
																<<"access-control-expose-headers">> => ?ACCESS_CONTROL_EXPOSE_HEADERS},
												ResponseData, 
												CowboyReq),
					ems_logger:log_request(Request2);
				{error, request, Request2 = #request{code = Code,
													 response_header = ResponseHeader,
													 response_data = ResponseData}} ->
					Response = cowboy_req:reply(Code, 
												ResponseHeader#{<<"server">> => ?SERVER_NAME,
																<<"content-type">> => ?CONTENT_TYPE_JSON,
																<<"cache-control">> => ?CACHE_CONTROL_NO_CACHE,
																<<"access-control-allow-origin">> => ?ACCESS_CONTROL_ALLOW_ORIGIN,
																<<"access-control-max-age">> => ?ACCESS_CONTROL_MAX_AGE,
																<<"access-control-allow-headers">> => ?ACCESS_CONTROL_ALLOW_HEADERS,
																<<"access-control-allow-methods">> => ?ACCESS_CONTROL_ALLOW_METHODS,
																<<"access-control-expose-headers">> => ?ACCESS_CONTROL_EXPOSE_HEADERS},
												ResponseData, 
												CowboyReq),
					ems_logger:log_request(Request2);
				{error, Reason} = Error ->
					Request2 = Request#request{code = 400, 
											   reason = Reason, 
											   response_data = ems_schema:to_json(Error), 
											   response_header = default_http_header(),
											   latency = ems_util:get_milliseconds() - T1},
					Response = cowboy_req:reply(Request2#request.code, 
												Request2#request.response_header, 
												Request2#request.response_data, CowboyReq),
					ems_logger:log_request(Request2)
			end;
		{error, Reason} = Error -> 
			ems_logger:error("ems_http_handler request error: ~p.\n", [Reason]),
			Response = cowboy_req:reply(400, default_http_header(), ems_schema:to_json(Error), CowboyReq)
	end,
	{ok, Response, Opts}.


default_http_header() ->
	#{
		<<"server">> => ?SERVER_NAME,
		<<"content-type">> => ?CONTENT_TYPE_JSON,
		<<"cache-control">> => ?CACHE_CONTROL_NO_CACHE,
		<<"access-control-allow-origin">> => ?ACCESS_CONTROL_ALLOW_ORIGIN,
		<<"access-control-max-age">> => ?ACCESS_CONTROL_MAX_AGE,
		<<"access-control-allow-headers">> => ?ACCESS_CONTROL_ALLOW_HEADERS,
		<<"access-control-allow-methods">> => ?ACCESS_CONTROL_ALLOW_METHODS,
		<<"access-control-expose-headers">> => ?ACCESS_CONTROL_EXPOSE_HEADERS
	}.

	
terminate(_Reason, _Req, _State) ->  ok.    
