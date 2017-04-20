%%********************************************************************
%% @title ems_django
%% @version 1.0.0
%% @doc Contains functions for compiling and rendering django templates
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_django).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([compile_file/2, compile_file/3, render/2, load_module_template/1]).
		 
-spec compile_file(string(), atom()) -> {ok, atom()} | {error, atom()}.
compile_file(FileName, ModuleName) -> erlydtl:compile_file(FileName, ModuleName, [{out_dir, "ebin"}]).


-spec compile_file(string(), atom(), string()) -> {ok, atom()} | {error, atom()}.
compile_file(FileName, ModuleName, OutputDir) -> erlydtl:compile_file(FileName, ModuleName, [{out_dir, OutputDir}]).


-spec render(atom(), list(tuple())) -> {ok, binary()} | {error, atom()}.
render(ModuleName, Args) when is_tuple(Args) ->	
	render(ModuleName, [Args]);
render(ModuleName, Args) ->	
	case ModuleName:render(Args) of
		{ok, Response} -> {ok, erlang:iolist_to_binary(Response)};
		Error -> Error
	end.

	
-spec load_module_template(string()) -> {ok, atom()} | {error, atom()}.
load_module_template(FileName) ->
	ModuleName = list_to_atom("mod_djt_" ++ integer_to_list(erlang:phash2(FileName))),
	case code:ensure_loaded(ModuleName) of
		{module, _} -> {ok , ModuleName};
		_Error -> 
			FileNamePath = filename:dirname(FileName), 
			case compile_file(FileName, ModuleName, FileNamePath) of
				{ok, ModuleName} -> 
					ems_logger:info("ems_django compile file ~p << ~p >>.", [FileName, ModuleName]),
					{ok , ModuleName};
				{error, Reason} = Error ->
					ems_logger:error("ems_django compile invalid file ~p. Reason: ~p.", [FileName, Reason]),
					Error
			end
	end.
