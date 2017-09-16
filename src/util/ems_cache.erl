%%********************************************************************
%% @title Module ems_cache
%% @version 1.0.0
%% @doc Module for cache management.
%%      Based on solution http://inaka.net/blog/2013/03/05/ETS-simple-cache.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_cache).

-behavior(gen_server). 

%% Server API
-export([start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-export([new/1, get/4, get/5, flush/1, flush/2, flush_future/3, flush_future/4, add/4]).

%  Armazena o estado do service. 
-record(state, {}). 
-type state():: #state{}.

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	NewState = #state{},
    {ok, NewState}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.
    
handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({expire, CacheName, Key}, State) ->
	flush(CacheName, Key),
	{noreply, State};
  
handle_info({expire, CacheName, Key, FunAfterFlush}, State) ->
	case ets:lookup(CacheName, Key) of
		[] -> ok;
		_ -> 
			flush(CacheName, Key),
			FunAfterFlush(Key)
	end,
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
%% Funções internas
%%====================================================================

%% @doc Initializes a cache.
-spec init(string()) -> ok.
new(CacheName) ->
	ets:new(CacheName, [named_table, 
						{read_concurrency, true}, 
						 public, 
						{write_concurrency, true}]),
	ok.

%% @doc Deletes the keys that match the given ets:matchspec() from the cache.
-spec flush(string(), term()) -> true.
flush(CacheName, Key) ->
	ets:delete(CacheName, Key).

%% @doc Deletes the keys that match the given ets:matchspec() from the cache.
-spec flush_future(string(), pos_integer(), term()) -> true.
flush_future(_, infinity, _) -> 
	ok;
flush_future(CacheName, LifeTime, Key) ->
	erlang:send_after(LifeTime, ems_cache, {expire, CacheName, Key}),
	ok.

flush_future(_, infinity, _, _) -> ok;
flush_future(CacheName, LifeTime, Key, FunAfterFlush) ->
	erlang:send_after(LifeTime, ems_cache, {expire, CacheName, Key, FunAfterFlush}),
	ok.


%% @doc Deletes all keys in the given cache.
-spec flush(string()) -> true.
flush(CacheName) ->
	true = ets:delete_all_objects(CacheName).

%% @doc Tries to lookup Key in the cache, and execute the given FunResult
%% on a miss.
-spec get(string(), infinity|pos_integer(), term(), function()) -> term().
get(_CacheName, 0, _Key, FunResult) ->
	FunResult();

get(CacheName, LifeTime, Key, FunResult) ->
	case ets:lookup(CacheName, Key) of
		[] ->
		  % Not found, create it.
		  V = FunResult(),
		  ets:insert(CacheName, {Key, V}),
		  flush_future(CacheName, LifeTime, Key),
		  V;
		[{Key, R}] -> R
	end.

get(CacheName, LifeTime, Key, FunResult, FunAfterFlush) ->
	case ets:lookup(CacheName, Key) of
		[] ->
		  % Not found, create it.
		  V = FunResult(),
		  ets:insert(CacheName, {Key, V}),
		  flush_future(CacheName, LifeTime, Key, FunAfterFlush),
		  V;
		[{Key, R}] -> R
	end.

add(CacheName, LifeTime, Key, Value) ->
  ets:insert(CacheName, {Key, Value}),
  flush_future(CacheName, LifeTime, Key).
  
