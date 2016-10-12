%%********************************************************************
%% @title Módulo ems_file_watcher
%% @version 1.0.0
%% @doc Módulo para monitoramento de arquivos no OS
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_file_watcher).

-export([start/0, stop/0, watch/2, inotify_event/3]).

start() -> application:start(inotify).

stop() -> application:stop(inotify).

watch(File, Fun) ->
	Ref = inotify:watch(File),
	inotify:add_handler(Ref, ?MODULE, Fun).
	
inotify_event(Fun, _EventTag, _Msg) ->	
	Fun().
