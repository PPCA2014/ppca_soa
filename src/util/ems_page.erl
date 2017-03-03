%%********************************************************************
%% @title Módulo ems_page
%% @version 1.0.0
%% @doc Contém funções para compilação de paginas Django
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_page).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-export([compile_file/2, render/2]).
		 
compile_file(Page, ModuleName) -> erlydtl:compile_file(Page, ModuleName, [{out_dir, "ebin"}]).

render(PageModule, Args) ->	
	case PageModule:render(Args) of
		{ok, Response} -> erlang:iolist_to_binary(Response);
		Reason -> throw({einvalid_page, Reason, page,  PageModule})
	end.


