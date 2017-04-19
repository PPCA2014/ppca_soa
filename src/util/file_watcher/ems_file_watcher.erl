%%********************************************************************
%% @title Módulo ems_file_watcher
%% @version 1.0.0
%% @doc Module for monitoring files in OS
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_file_watcher).

%-define(inotity_monitor, true).

-ifdef(inotity_monitor).
-include("ems_file_watcher_inotify").
-else.
-include("ems_file_watcher_generic").
-endif.	
