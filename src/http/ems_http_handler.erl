%%********************************************************************
%% @title Module ems_http_server
%% @version 1.0.0
%% @doc Main module HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_handler).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

-export([init/2]).


init(CowboyReq, Opts) ->
	case cowboy_req:method(CowboyReq) of
		<<"OPTIONS">> ->
			case ems_dispatcher_cache:lookup_options() of
				false ->
					Response = cowboy_req:reply(204, http_header_options(), <<>>, CowboyReq),
					ems_dispatcher_cache:add_options(Response);
				{true, ResponseData} -> 
					Response = ResponseData
			end;
		_ ->
			case ems_http_util:encode_request_cowboy(CowboyReq, self()) of
				{ok, Request = #request{type = Method,
										url_hash = UrlHash,
										t1 = T1}} -> 
					case ems_dispatcher:dispatch_request(Request) of
						{ok, Request2 = #request{result_cache = true,
												 code = HttpCode,
												 response_data = ResponseData,
												 response_header = HttpHeader}} ->
							Response = cowboy_req:reply(HttpCode, HttpHeader, ResponseData, CowboyReq),
							ems_logger:log_request(Request2);
						{ok, Request2} ->
							Request3 = encode_result(Request2),
							case Method == "GET"of
								true -> ems_dispatcher_cache:add(UrlHash, T1, Request3);
								false -> ok
							end,
							Response = cowboy_req:reply(Request3#request.code, 
														Request3#request.response_header, 
														Request3#request.response_data, 
														CowboyReq),
							ems_logger:log_request(Request3);
						Error ->
							Request2 = Request#request{code = 400, 
													   reason = Error, 
													   response_data = ems_schema:to_json(Error), 
													   response_header = default_http_header(),
													   latency = ems_util:get_milliseconds() - T1},
							Response = cowboy_req:reply(Request2#request.code, 
													    Request2#request.response_header, 
													    Request2#request.response_data, CowboyReq),
							ems_logger:log_request(Request2)
					end;
				{error, _Reason} = Error -> 
					Response = cowboy_req:reply(400, default_http_header(), ems_schema:to_json(Error), CowboyReq)
			end
	end,
	{ok, Response, Opts}.



encode_result(Request = #request{type = Method, 
								 t1 = T1,
								 response_data = Result,	
								 service = #service{page_module = PageModule,
								  				    page_mime_type = PageMimeType}}) ->
	case PageModule of
		undefined ->
			case Result of
				{ok, <<_Content/binary>> = ResponseData} -> 
					Request#request{code = get_http_code_verb(Method, true), 
									reason = ok, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ResponseData,
									response_header = #{
											<<"server">> => ?SERVER_NAME,
											<<"content-type">> => <<"application/json; charset=utf-8">>,
											<<"cache-control">> => <<"no-cache">>
										}};
				{ok, <<_Content/binary>> = ResponseData, <<MimeType/binary>> = MimeType} ->
					Request#request{code = get_http_code_verb(Method, true), 
									reason = ok, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ResponseData,
									response_header = #{
											<<"server">> => ?SERVER_NAME,
											<<"content-type">> => MimeType,
											<<"cache-control">> => header_cache_control(MimeType)
										}};
				{HttpCode, <<_Content/binary>> = ResponseData, HttpHeader} ->
					Request#request{code = HttpCode, 
									reason = ok, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ResponseData,
									response_header = HttpHeader};
				{HttpCode, ResponseData, HttpHeader} when erlang:is_tuple(ResponseData) ->
					Request#request{code = HttpCode, 
									reason = ok, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ems_schema:to_json(ResponseData),
									response_header = HttpHeader};
				{error, _Reason} = Error ->
					Request#request{code = get_http_code_verb(Method, false), 
									reason = Error, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ems_schema:to_json(Error),
									response_header = #{
										<<"server">> => ?SERVER_NAME,
										<<"content-type">> => <<"application/json; charset=utf-8">>,
										<<"cache-control">> => <<"no-cache">>
									}};
				<<_Content/binary>> -> 
					Request#request{code = get_http_code_verb(Method, true), 
									reason = ok, 
									latency = ems_util:get_milliseconds() - T1,
									response_header = #{
										<<"server">> => ?SERVER_NAME,
										<<"content-type">> => <<"application/json; charset=utf-8">>,
										<<"cache-control">> => <<"no-cache">>
									}};
				Content when is_map(Content) -> 
					Request#request{code = get_http_code_verb(Method, true), 
									reason = ok, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ems_schema:to_json(Content),
									response_header = #{
										<<"server">> => ?SERVER_NAME,
										<<"content-type">> => <<"application/json; charset=utf-8">>,
										<<"cache-control">> => <<"no-cache">>
									}};
				Content = [H|_] when is_map(H) -> 
					Request#request{code = get_http_code_verb(Method, true), 
									reason = ok, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ems_schema:to_json(Content),
									response_header = #{
										<<"server">> => ?SERVER_NAME,
										<<"content-type">> => <<"application/json; charset=utf-8">>,
										<<"cache-control">> => <<"no-cache">>
									}};
				Content = [H|_] when is_tuple(H) -> 
					Request#request{code = get_http_code_verb(Method, true), 
									reason = ok, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ems_schema:to_json(Content),
									response_header = #{
										<<"server">> => ?SERVER_NAME,
										<<"content-type">> => <<"application/json; charset=utf-8">>,
										<<"cache-control">> => <<"no-cache">>
									}};
				Content -> 
					Request#request{code = get_http_code_verb(Method, true), 
									reason = ok, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ems_schema:to_json(Content),
									response_header = #{
										<<"server">> => ?SERVER_NAME,
										<<"cache-control">> => <<"no-cache">>
									}}
			end;
		_ -> 
			case Result of
				{error, _Reason} = Error ->
					Request#request{code = get_http_code_verb(Method, false), 
									reason = Error, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ems_schema:to_json(Error),
									response_header = #{
										<<"server">> => ?SERVER_NAME,
										<<"content-type">> => <<"application/json; charset=utf-8">>,
										<<"cache-control">> => <<"no-cache">>
									}};
				_ ->
					Request#request{code = get_http_code_verb(Method, true), 
									reason = ok, 
									latency = ems_util:get_milliseconds() - T1,
									response_data = ems_page:render(PageModule, Result),
									response_header = #{
										<<"server">> => ?SERVER_NAME,
										<<"content-type">> => PageMimeType,
										<<"cache-control">> => header_cache_control(PageMimeType)
									}}

			end
	end.
	

default_http_header() ->
	#{
		<<"server">> => ?SERVER_NAME
	}.

http_header_options() ->
	#{
		<<"cache-control">> => <<"max-age=290304000, public">>,
		<<"server">> => ?SERVER_NAME
	}.

get_http_code_verb("POST", true)  -> 201;
get_http_code_verb("PUT", false)  -> 400;
get_http_code_verb(_, true)  -> 200;
get_http_code_verb(_, false)  -> 400.

	
header_cache_control(<<"application/x-javascript">>) ->
	<<"max-age=290304000, public"/utf8>>;
header_cache_control(<<"text/css">>) ->
	<<"max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/x-icon">>) ->
	<<"max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/png">>) ->
	<<"max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/gif">>) ->
	<<"max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/jpeg">>) ->
	<<"max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/bmp">>) ->
	<<"max-age=290304000, public"/utf8>>;
header_cache_control(<<"application/font-woff">>) ->
	<<"max-age=290304000, public"/utf8>>;
header_cache_control(<<_MimeType/binary>>) ->
	<<"no-cache"/utf8>>.

	
%terminate(_Reason, _Req, _State) ->  ok.    
