%%********************************************************************
%% @title Módulo ems_request
%% @version 1.0.0
%% @doc Módulo repositório de requisições
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_request).
-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API
-export([start/1, start_link/1, stop/0]).

%% Client API
-export([registra_request/1, 
		 finaliza_request/1,
		 get_requests_periodo/1,
		 get_requests_periodo/2,
		 get_request_by_rid/1,
		 get_request_em_andamento/1,
		 load_from_file_req/1]).

-export([get_property_request/2, 
		 get_param_url/3,
		 get_querystring/3,
		 get_querystring/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {checkpoint_time_ref = undefined}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?SERVER, Args, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================

%% @doc Registra ou atualiza um request
registra_request(Request) -> 
	gen_server:cast(?SERVER, {registra_request, Request}).

finaliza_request(Request) -> 
	gen_server:cast(?SERVER, {finaliza_request, Request}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) -> 
	ets:new(tbl_request_cache, [set, 
								private, 
								named_table,
								{write_concurrency, true},
								{keypos, 2}]),
	ets:new(tbl_request_andamento, [set, 
									public, 
									named_table,
									{write_concurrency, true},
									{keypos, 2}]),
	%fprof:trace([start, {procs, [self()]}]),
	{ok, #state{}}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({registra_request, Request}, State) ->
	case State#state.checkpoint_time_ref of
		undefined -> ok;
		Time_ref -> erlang:cancel_timer(Time_ref, [])
	end,
	do_registra_request(Request),
	Time_ref2 = erlang:send_after(?REQ_CACHE_SYNC_CHECKPOINT, self(), checkpoint),
	{noreply, #state{checkpoint_time_ref = Time_ref2}};
	
handle_cast({finaliza_request, Request}, State) ->	
	do_finaliza_request(Request),
	{noreply, State}.

handle_call(Msg, _From, State) ->
   {reply, Msg, State}.

handle_info(checkpoint, State) ->
   do_sync_buffer(),
   {noreply, State};

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

do_registra_request(Request) ->
	ets:insert(tbl_request_cache, Request),
	ets:insert(tbl_request_andamento, Request).
	
do_finaliza_request(Request) ->
	ets:delete(tbl_request_cache, Request#request.rid),
	ets:delete(tbl_request_andamento, Request#request.rid).
		
do_sync_buffer() ->
	SyncBuffer = fun() ->  
						ets:foldl(fun(Request, DontCare) ->
									mnesia:write(Request),
									ets:delete(ems_request_cache, Request#request.rid),
									DontCare
								  end, notused, ems_request_cache)
				 end,
	mnesia:transaction(SyncBuffer).


%% @doc Retorna a lista de requisições de um período
get_requests_periodo(Periodo) ->
	Query = fun() ->
		  qlc:e(
			 qlc:sort(
				 qlc:q([R || R <- mnesia:table(request), 
							 ems_util:no_periodo(R#request.timestamp, Periodo)]), [{order, descending}]
							 
				)
		  )
	   end,
	{atomic, Requests} = mnesia:transaction(Query),
	Requests.

%% @doc Retorna a lista de requisições por período e por code
get_requests_periodo(Periodo, Code) ->
	Query = fun() ->
		  qlc:e(
			 qlc:sort(
				 qlc:q([R || R <- mnesia:table(request), 
							 ems_util:no_periodo(R#request.timestamp, Periodo),
							 R#request.code == Code
							 ]), [{order, descending}]
				)
		  )
	   end,
	{atomic, Requests} = mnesia:transaction(Query),
	Requests.

%% @doc Retorna uma requisição pelo seu id
get_request_by_rid(RID) -> 
	ems_db:get(request, RID).

%% @doc Retorna uma requisição pelo seu id
get_request_em_andamento(RID) -> 
	case ets:lookup(tbl_request_andamento, RID) of
		[] -> {error, enoent};
		[Request] -> {ok, Request}
	end.

%% @doc Retorna a URL do request
get_property_request(<<"url">>, Request) ->
	Request#request.url;

%% @doc Retorna o tipo do request
get_property_request(<<"metodo">>, Request) ->
	Request#request.type;

get_property_request(<<"type">>, Request) ->
	Request#request.type;

%% @doc Retorna a URL do request
get_property_request(<<"http_version">>, Request) ->
	Request#request.version;

%% @doc Retorna o payload/body do request
get_property_request(<<"payload">>, Request) ->
	Request#request.payload_map;

%% @doc Retorna o payload/body do request
get_property_request(<<"body">>, Request) ->
	Request#request.payload_map.

%% @doc Retorna um parâmetro do request
get_param_url(NomeParam, Default, Request) ->
	ParamsUrl = Request#request.params_url,
	NomeParam2 = iolist_to_binary(NomeParam),
	maps:get(NomeParam2, ParamsUrl, Default).

%% @doc Retorna uma querystring do request
get_querystring(QueryName, Default, #request{querystring_map = QuerystringMap}) ->
	Value = maps:get(QueryName, QuerystringMap, Default),
	case erlang:is_list(Value) of
		true -> list_to_binary(Value);
		false -> Value
	end.

get_querystring(QueryName, OrQueryName2, Default, #request{querystring_map = QuerystringMap}) ->
	case maps:is_key(QueryName, QuerystringMap) of
		true ->	Value = maps:get(QueryName, QuerystringMap, Default);
		false -> Value = maps:get(OrQueryName2, QuerystringMap, Default)
	end,
	case erlang:is_list(Value) of
		true -> list_to_binary(Value);
		false -> Value
	end.


load_from_file_req(Request = #request{url = Url,
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
			ETag = integer_to_binary(erlang:phash2({FSize, MTime}, 16#ffffffff)),
			LastModified = cowboy_clock:rfc1123(MTime),
			ExpireDate = ems_util:date_add_minute(Timestamp, ExpiresMinute + 120), % add +120min (2h) para ser horário GMT
			Expires = cowboy_clock:rfc1123(ExpireDate),
			HttpHeader =	#{
								<<"content-type">> => MimeType,
								<<"cache-control">> => CacheControl,
								<<"etag">> => ETag,
								<<"last-modified">> => LastModified,
								<<"expires">> => Expires
							},
			case ETag == IfNoneMatchReq orelse LastModified == IfModifiedSinceReq of
				true -> {ok, Request#request{code = 304, 
											 reason = enot_modified,
											 etag = ETag,
											 filename = FileName,
											 response_data = <<>>, 
											 response_header = HttpHeader}
						 };
				false ->
					case file:read_file(FileName) of
						{ok, FileData} -> 
							{ok, Request#request{code = 200, 
											     reason = ok,
											     etag = ETag,
											     filename = FileName,
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

file_info(FileName) ->
	case file:read_file_info(FileName, [{time, universal}]) of
		{ok,{file_info, FSize, _Type, _Access, _ATime, MTime, _CTime, _Mode,_,_,_,_,_,_}} = 
			Result -> Result,
			{FSize, MTime};
		Error -> Error
	end.

