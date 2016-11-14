%%********************************************************************
%% @title Module ems_static_file_service
%% @version 1.0.0
%% @doc Reads static files
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_static_file_service).

-behavior(gen_server). 

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%% Server API
-export([start/1, start_link/1, stop/0]).

%% Cliente interno API
-export([execute/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do service. 
-record(state, {}). 


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
 
execute(Request = #request{url = Url,
						   if_modified_since = IfModifiedSinceReq, 
						   if_none_match = IfNoneMatchReq,
						   timestamp = {{Year,Month,Day},{Hour,Min,Secs}}}) ->
	FileName = ?STATIC_FILE_PATH ++ Url,
	case file_info(FileName) of
		{error, Reason} = Err -> {error, Request#request{code = case Reason of enoent -> 404; _ -> 400 end, 
														 reason = Reason,	
														 response_data = Err, 
														 response_header = error_http_header()}
								  };
		{FSize, MTime} -> 
			MimeType = ems_http_util:mime_type(filename:extension(FileName)),
			ETag = generate_etag(FSize, MTime),
			LastModified = cowboy_clock:rfc1123(MTime),
			Expires = cowboy_clock:rfc1123({{Year+1,Month,Day},{Hour,Min,Secs}}),
			HttpHeader = generate_header(MimeType, ETag, LastModified, Expires),
			case ETag == IfNoneMatchReq orelse LastModified == IfModifiedSinceReq of
				true -> {ok, Request#request{code = 304, 
											 response_data = <<>>, 
											 response_header = HttpHeader}
						 };
				false ->
					ems_cache:get(static_file_cache, Request#request.service#service.result_cache, FileName, 
						fun() -> 
							case file:read_file(FileName) of
								{ok, FileData} -> {ok, Request#request{code = 200, 
																	   response_data = FileData, 
																	   response_header = HttpHeader}
												   };
								{error, Reason} = Err -> {error, Request#request{code = case Reason of enoent -> 404; _ -> 400 end, 
																				 reason = Reason,
																				 response_data = Err, 
																				 response_header = error_http_header()}
														 }
							end
						end)
			end
	end.


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    create_shared_cache(),
    {ok, #state{}}.

    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.
    
handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

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

file_info(FileName) ->
	case file:read_file_info(FileName, [{time, universal}]) of
		{ok,{file_info, FSize, _Type, _Access, _ATime, MTime, _CTime, _Mode,_,_,_,_,_,_}} = 
			Result -> Result,
			{FSize, MTime};
		Error -> Error
	end.
	

generate_header(MimeType, ETag, LastModified, Expires) ->
	#{
		<<"content-type">> => MimeType,
		<<"cache-control">> => <<"max-age=290304000, public">>,
		<<"etag">> => ETag,
		<<"last-modified">> => LastModified,
		<<"expires">> => Expires
	}.


error_http_header() ->
	#{
		<<"content-type">> => <<"application/json; charset=utf-8">>,
		<<"cache-control">> => <<"no-cache">>
	}.


generate_etag(FSize, MTime) -> integer_to_binary(erlang:phash2({FSize, MTime}, 16#ffffffff)).

create_shared_cache() ->
	try
		ems_cache:new(static_file_cache)
	catch
		_Exception:_Reason ->  ok
	end.
