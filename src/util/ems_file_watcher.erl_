%%********************************************************************
%% @title Módulo ems_file_watcher
%% @version 1.0.0
%% @doc Módulo para monitoramento de arquivos no OS
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_file_watcher).

-behavior(gen_server). 

-include("../../deps/inotify/include/inotify.hrl").

%% Server API
-export([start/0, start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-export([watch/2, unwatch/1, inotify_event/3]).

% State of server
-record(state, {}).


%%====================================================================
%% Server API
%%====================================================================

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, shutdown).


%%====================================================================
%% Client API
%%====================================================================


inotify_event(Fun, EventTag, Msg) -> gen_server:cast(?MODULE, {inotify_event, Fun, EventTag, Msg}).
	
watch(Path, Fun) -> gen_server:cast(?MODULE, {watch, Path, Fun}).

unwatch(Path) -> gen_server:cast(?MODULE, {unwatch, Path}).


%%====================================================================
%% gen_server callbacks
%%====================================================================


init(_) ->
	application:start(inotify),
	ets:new(ems_file_watcher_watch, [named_table, public]),
	ets:new(ems_file_watcher_events, [named_table, public]),
    {ok, #state{}}.

handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({inotify_event, Fun, EventTag, Msg}, State) ->
	do_inotify_event(Fun, EventTag, Msg),
	{noreply, State, 0};

handle_cast({watch, Path, Fun}, State) ->
	do_watch(Path, Fun),
	{noreply, State, 0};

handle_cast({unwatch, Path}, State) ->
	do_unwatch(Path),
	{noreply, State, 0}.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(_Msg, State) ->
	{noreply, State, 0}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
	application:stop(inotify),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

	

do_watch(Path, Fun) ->
	case ets:lookup(ems_file_watcher_watch, Path) of
		[] -> 
			Path2 = ems_util:remove_ult_backslash_url(Path),
			EventTag = inotify:watch(Path2, ?ALL),
			Ref = erlang:ref_to_list(EventTag),
			ets:insert(ems_file_watcher_watch, {Path2, {Ref, EventTag}}),
			ets:insert(ems_file_watcher_events, {Ref, {Path2, EventTag}}),
			inotify:add_handler(EventTag, ?MODULE, Fun);
		[_] -> ok
	end.

do_unwatch(Path) ->
	Path2 = ems_util:remove_ult_backslash_url(Path),
	case ets:lookup(ems_file_watcher_watch, Path2) of
		[] -> ok;
		[{_, {Ref, EventTag}}] -> 
			inotify:unwatch(EventTag),
			ets:delete(ems_file_watcher_watch, Path2),
			ets:delete(ems_file_watcher_events, Ref),
			ok
	end.
	
do_inotify_event(Fun, EventTag, Msg) ->	
	Ref = erlang:ref_to_list(EventTag),
	case ets:lookup(ems_file_watcher_events, Ref) of
		[] -> ok;
		[{_, {Path, EventTag}}] -> 
			case Msg of
				{inotify_msg,[close_write], 0, []} -> Fun({change, Path});
				{inotify_msg,[close_write], 0, FileName} -> Fun({change, Path ++ "/" ++ FileName});
				{inotify_msg,[delete_self], 0, []} -> 
					ets:delete(ems_file_watcher_events, Ref),
					ets:delete(ems_file_watcher_watch, Path),
					ems_util:sleep(350),
					case filelib:is_file(Path) of
						true -> watch(Path, Fun);
						false -> Fun({delete, Path})
					end;
				{inotify_msg,[delete_self], 0, FileName} -> Fun({delete, Path ++ "/" ++ FileName});
				{inotify_msg,[delete], 0, []} -> Fun({delete, Path});
				{inotify_msg,[delete], 0, FileName} -> Fun({delete, Path ++ "/" ++ FileName});
				{inotify_msg,[move], 0, []} -> Fun({move, Path});
				{inotify_msg,[move], 0, FileName} -> Fun({move, Path ++ "/" ++ FileName});
				{inotify_msg,[move_from], _, FileName} -> Fun({delete, Path ++ "/" ++ FileName});
				{inotify_msg,[move_to], _, FileName} -> Fun({change, Path ++ "/" ++ FileName});
				{inotify_msg,[ignored],0,[]} -> 
					unwatch(Path),
					ets:delete(ems_file_watcher_events, Ref),
					ets:delete(ems_file_watcher_watch, Path);
				_ -> ok
			end
	end.
