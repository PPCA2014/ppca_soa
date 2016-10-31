%%********************************************************************
%% @title Módulo ems_static_file_service
%% @version 1.0.0
%% @doc Módulo para gerenciamento de arquivos estáticos.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_static_file_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).


-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%% Server API
-export([start/1, start_link/1, stop/0]).

%% Cliente interno API
-export([execute/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do service. 
-record(state, {cache}). 


%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
execute(Request, From) ->
	ems_pool:cast(ems_static_file_service, {get_file, Request, From}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    create_shared_cache(),
    {ok, #state{}}.

    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({get_file, Request, _From}, State) ->
	Result = do_get_file(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {static_file, Request, Result}),
	{noreply, State}.
    
handle_call({get_file, Request}, _From, State) ->
	Result = do_get_file(Request, State),
	{reply, Result, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================

do_get_file(Request = #request{if_modified_since = IfModifiedSinceReq, 
							   if_none_match = IfNoneMatchReq,
							   timestamp = {{Year,Month,Day},{Hour,Min,Secs}}}, _State) ->
	FileName = ?STATIC_FILE_PATH ++ Request#request.url,
	case file_info(FileName) of
		{error, _Reason} = Err -> {404, Err, error_http_header()};
		{FSize, MTime} -> 
			MimeType = ems_http_util:mime_type(filename:extension(FileName)),
			ETag = generate_etag(FSize, MTime),
			LastModified = cowboy_clock:rfc1123(MTime),
			Expires = cowboy_clock:rfc1123({{Year+1,Month,Day},{Hour,Min,Secs}}),
			HttpHeader = generate_header(MimeType, ETag, LastModified, Expires),
			case ETag == IfNoneMatchReq orelse LastModified == IfModifiedSinceReq of
				true -> {304, <<>>, HttpHeader};
				false ->
					ems_cache:get(static_file_cache, Request#request.service#service.result_cache, FileName, 
						fun() -> 
							case file:read_file(FileName) of
								{ok, FileContent} -> {200, FileContent, HttpHeader};
								{error, enoent} = Err -> {404, Err, error_http_header()};
								{error, _Reason} = Err -> {400, Err, error_http_header()}
							end
						end)
			end
	end.
	
		

file_info(FileName) ->
	case file:read_file_info(FileName, [{time, universal}]) of
		{ok,{file_info, FSize, _Type, _Access, _ATime, MTime, _CTime, _Mode,_,_,_,_,_,_}} = 
			Result -> Result,
			{FSize, MTime};
		Error -> Error
	end.
	

generate_header(MimeType, ETag, LastModified, Expires) ->
	#{
		<<"server">> => ?SERVER_NAME,
		<<"content-type">> => MimeType,
		<<"cache-control">> => <<"max-age=290304000, public">>,
		<<"etag">> => ETag,
		<<"last-modified">> => LastModified,
		<<"expires">> => Expires
	}.


error_http_header() ->
	#{
		<<"server">> => ?SERVER_NAME,
		<<"content_type">> => <<"application/json; charset=utf-8">>,
		<<"cache-control">> => <<"no-cache">>
	}.


generate_etag(FSize, MTime) -> integer_to_binary(erlang:phash2({FSize, MTime}, 16#ffffffff)).

create_shared_cache() ->
	try
		ems_cache:new(static_file_cache)
	catch
		_Exception:_Reason ->  ok
	end.
