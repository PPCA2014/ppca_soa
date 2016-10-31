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
	io:format("req is ~p\n", [CowboyReq]),
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
				{ok, Request = #request{rowid = Rowid, t1 = Timestamp}} -> 
					case ems_dispatcher:dispatch_request(Request) of
						{ok, #request{result_cache = true}, Cache} ->
							{HttpCode, Reason, ResponseData2, HttpHeader} = Cache,
							Response = cowboy_req:reply(HttpCode, HttpHeader, ResponseData2, CowboyReq);
						{ok, Request2, ResponseData} -> 
							{HttpCode, Reason, ResponseData2, HttpHeader} = encode_result(Request2, ResponseData),
							ems_dispatcher_cache:add(Rowid, Timestamp, {HttpCode, Reason, ResponseData2, HttpHeader}),
							Response = cowboy_req:reply(HttpCode, HttpHeader, ResponseData2, CowboyReq);
						Error ->
							Response = cowboy_req:reply(400, default_http_header(), ems_schema:to_json(Error), CowboyReq)
					end;
				{error, Reason} = Error -> 
					Response = cowboy_req:reply(400, default_http_header(), ems_schema:to_json(Error), CowboyReq)
			end
	end,
	{ok, Response, Opts}.



encode_result(Request = #request{type = Method, 
								    service = #service{page_module = PageModule,
									 				   page_mime_type = PageMimeType}}, Result) ->
	case PageModule of
		null ->
			case Result of
				{ok, <<Content/binary>> = ResponseData} -> 
					HttpCode = get_http_code_verb(Method, true),
					{HttpCode, ok, ResponseData, #{
								<<"server">> => ?SERVER_NAME,
								<<"content-type">> => <<"application/json; charset=utf-8">>,
								<<"cache-control">> => <<"no-cache">>
							}};
				{ok, <<Content/binary>> = ResponseData, <<MimeType/binary>> = MimeType} ->
					HttpCode = get_http_code_verb(Method, true),
					{HttpCode, ok, ResponseData, #{
								<<"server">> => ?SERVER_NAME,
								<<"content-type">> => MimeType,
								<<"cache-control">> => header_cache_control(MimeType)
							}};
				{HttpCode, <<Content/binary>> = ResponseData, HttpHeader} ->
					{HttpCode, ok, ResponseData, HttpHeader};
				{HttpCode, ResponseData, HttpHeader} when erlang:is_tuple(ResponseData) ->
					{HttpCode, ResponseData, ems_schema:to_json(ResponseData), HttpHeader};
				{error, Reason} = Error ->
					HttpCode = get_http_code_verb(Method, false),
					{HttpCode, Error, ems_schema:to_json(Error), #{
								<<"server">> => ?SERVER_NAME,
								<<"content-type">> => <<"application/json; charset=utf-8">>,
								<<"cache-control">> => <<"no-cache">>
							}};
				<<Content/binary>> = ResponseData -> 
					HttpCode = get_http_code_verb(Method, true),
					{HttpCode, ok, ResponseData, #{
								<<"server">> => ?SERVER_NAME,
								<<"content-type">> => <<"application/json; charset=utf-8">>,
								<<"cache-control">> => <<"no-cache">>
							}};
				Content when is_map(Content) -> 
					HttpCode = get_http_code_verb(Method, true),
					ResponseData = ems_schema:to_json(Content),
					{HttpCode, ok, ResponseData, #{
								<<"server">> => ?SERVER_NAME,
								<<"content-type">> => <<"application/json; charset=utf-8">>,
								<<"cache-control">> => <<"no-cache">>
							}};
				Content = [H|_] when is_map(H) -> 
					HttpCode = get_http_code_verb(Method, true),
					ResponseData = ems_schema:to_json(Content),
					{HttpCode, ok, ResponseData, #{
								<<"server">> => ?SERVER_NAME,
								<<"content-type">> => <<"application/json; charset=utf-8">>,
								<<"cache-control">> => <<"no-cache">>
							}};
				Content = [H|_] when is_tuple(H) -> 
					HttpCode = get_http_code_verb(Method, true),
					ResponseData = ems_schema:to_json(Content),
					{HttpCode, ok, ResponseData, #{
								<<"server">> => ?SERVER_NAME,
								<<"content-type">> => <<"application/json; charset=utf-8">>,
								<<"cache-control">> => <<"no-cache">>
							}};
				Content -> 
					HttpCode = get_http_code_verb(Method, true),
					{HttpCode, ok, Content, #{
								<<"server">> => ?SERVER_NAME
							}}
			end;
		_ -> 
			case Result of
				{error, Reason} = Error ->
					HttpCode = get_http_code_verb(Method, false),
					{HttpCode, Error, ems_schema:to_json(Error), #{
								<<"server">> => ?SERVER_NAME,
								<<"content-type">> => <<"application/json; charset=utf-8">>,
								<<"cache-control">> => <<"no-cache">>
							}};
				_ ->
					HttpCode = get_http_code_verb(Method, true),
					ResponseData = ems_page:render(PageModule, Result),
					{HttpCode, ok, ResponseData, #{
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
