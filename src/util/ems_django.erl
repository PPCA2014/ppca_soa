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
compile_file(Filename, ModuleName) -> erlydtl:compile_file(Filename, ModuleName, [{out_dir, "ebin"}]).


-spec compile_file(string(), atom(), string()) -> {ok, atom()} | {error, atom()}.
compile_file(Filename, ModuleName, OutputDir) -> erlydtl:compile_file(Filename, ModuleName, [{out_dir, OutputDir}]).


-spec render(atom(), list(tuple())) -> {ok, binary()} | {error, atom()}.
render(ModuleName, Args) when is_tuple(Args) ->	
	render(ModuleName, [Args]);
render(ModuleName, Args) ->	
	case ModuleName:render(Args) of
		{ok, Response} -> {ok, erlang:iolist_to_binary(Response)};
		Error -> Error
	end.

	
-spec load_module_template(string()) -> {ok, atom()} | {error, atom()}.
load_module_template(Filename) ->
	ModuleName = list_to_atom("mod_djt_" ++ integer_to_list(erlang:phash2(Filename))),
	case code:ensure_loaded(ModuleName) of
		{module, _} -> {ok , ModuleName};
		_Error -> 
			FilenamePath = filename:dirname(Filename), 
			case compile_file(Filename, ModuleName, FilenamePath) of
				{ok, ModuleName} -> 
					ems_logger:info("ems_django compile file ~p << ~p >>.", [Filename, ModuleName]),
					{ok , ModuleName};
				{error, Reason} = Error ->
					ems_logger:error("ems_django compile invalid file ~p. Reason: ~p.", [Filename, Reason]),
					Error
			end
	end.
