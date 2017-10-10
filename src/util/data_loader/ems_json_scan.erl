%%********************************************************************
%% @title Module ems_json_scan
%% @version 1.0.0
%% @doc Module responsible for load json files
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_json_scan).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([scan/2, scan/3, scan_with_filter/4, scan_with_filter/5]).

-spec scan(string() | binary() | list(tuple()), #config{}) -> list(map()).
scan([{_NodeName, _JsonFileName}|_] = L, Conf) -> 
	{ok, scan_files(L, [], Conf, undefined, undefined)}.
	
-spec scan(string() | binary() | list(tuple()), binary(), #config{}) -> list(map()).	
scan(FileName, RootPath, Conf) -> 
	{ok, scan_file(FileName, [], RootPath, Conf, undefined, undefined)}.
	
-spec scan_with_filter(string() | binary() | list(tuple()), #config{}, binary(), any()) -> list(map()).
scan_with_filter([{_NodeName, _JsonFileName}|_] = L, Conf, FilterKey, FilterValue) -> 
	{ok, scan_files(L, [], Conf, FilterKey, FilterValue)}.
	
-spec scan_with_filter(string() | binary() | list(tuple()), binary(), #config{}, binary(), any()) -> list(map()).	
scan_with_filter(FileName, RootPath, Conf, FilterKey, FilterValue) -> 
	{ok, scan_file(FileName, [], RootPath, Conf, FilterKey, FilterValue)}.


%% internal functions

-spec scan_files(list(tuple()), list(), #config{}, binary(), any()) -> list().
scan_files([], Result, _, _, _) -> Result;
scan_files([{NodeName, JsonFileName}|Rest], Result, Conf, FilterKey, FilterValue) ->
	RootPath = filename:dirname(JsonFileName),
	case parse_filename_path(JsonFileName, RootPath, Conf) of
		{ok, FileName} ->
			io:format("ems_json_scan loading ~p from ~p.\n", [binary_to_list(NodeName), FileName]),
			Result2 = scan_file(FileName, Result, RootPath, Conf, FilterKey, FilterValue),
			scan_files(Rest, Result2, Conf, FilterKey, FilterValue);
		{error, FileName} ->
			ems_logger:format_warn("ems_json_scan failed to scan invalid file ~p. Ignoring this file.\n", [FileName])
	end.
		
		
-spec scan_file(string() | binary(), list(map()), binary(), #config{}, binary(), any()) -> list(map()) | {error, atom()}.
scan_file(JsonFileName, Result, RootPath, Conf, FilterKey, FilterValue) ->
	case parse_filename_path(JsonFileName, RootPath, Conf) of
		{ok, FileName} ->
			CurrentDir = filename:dirname(FileName),
			case ems_util:read_file_as_map(FileName) of
				{ok, FileList} when is_list(FileList) -> 
					scan_file_entry(FileList, CurrentDir, FileName, Result, RootPath, Conf, FilterKey, FilterValue);
				{ok, FileMap} -> 
					scan_file_entry([FileMap], CurrentDir, FileName, Result, RootPath, Conf, FilterKey, FilterValue);
				{error, enoent} ->
					ems_logger:format_warn("ems_json_scan file ~p does not exist, ignoring this file.\n", [FileName]),
					Result;
				_ -> 
					ems_logger:format_warn("ems_json_scan failed to read invalid file ~p. Ignoring this file.\n", [FileName]),
					Result
			end;
		{error, FileName} ->
			ems_logger:format_warn("ems_json_scan failed to scan invalid file ~p. Ignoring this file.\n", [FileName])
	end.
	
-spec scan_file_entry(list(), string(), string(), list(), binary(), #config{}, binary(), any()) -> list().
scan_file_entry([], _, _, Result, _, _, _, _) -> Result;
scan_file_entry([Map|MapTail], CurrentDir, CurrentFilenameMap, Result, RootPath, Conf, FilterKey, FilterValue) ->
	case maps:is_key(<<"file">>, Map) of
		true -> 
			case parse_filename_path(maps:get(<<"file">>, Map), CurrentDir, Conf) of
				{ok, FilenameMap} ->
					?DEBUG("ems_json_scan scan ~p.", [FilenameMap]),
					Result2 = scan_file(FilenameMap, Result, RootPath, Conf, FilterKey, FilterValue),
					scan_file_entry(MapTail, CurrentDir, CurrentFilenameMap, Result2, RootPath, Conf, FilterKey, FilterValue);			
				{error, FilenameMap} ->
					ems_logger:format_warn("ems_json_scan scan invalid file ~p. Ignoring this file.\n", [FilenameMap]),
					?DEBUG("~p: ~p.", [FilenameMap, Map]),
					scan_file_entry(MapTail, CurrentDir, CurrentFilenameMap, Result, RootPath, Conf, FilterKey, FilterValue)
			end;
		false -> 
			case FilterKey =/= undefined andalso maps:is_key(FilterKey, Map) andalso maps:get(FilterKey, Map) =/= FilterValue of
				true ->
					scan_file_entry(MapTail, CurrentDir, CurrentFilenameMap, Result, RootPath, Conf, FilterKey, FilterValue);
				false ->
					Map2 = Map#{<<"file_path">> => CurrentDir,
								<<"file_name">> => CurrentFilenameMap},
					scan_file_entry(MapTail, CurrentDir, CurrentFilenameMap, [Map2 | Result], RootPath, Conf, FilterKey, FilterValue)
			end
	end.

-spec parse_filename_path(string() | binary(), binary(), #config{}) -> {ok, string()} | {error, string()}.
parse_filename_path(JsonFileName, RootPath, #config{static_file_path = StaticFilePath}) -> 
	ems_util:parse_file_name_path(JsonFileName, StaticFilePath, RootPath).
	
