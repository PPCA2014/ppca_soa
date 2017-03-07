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

-export([compile_file/2, render/2, handle/3]).
		 
compile_file(Page, ModuleName) -> erlydtl:compile_file(Page, ModuleName, [{out_dir, "ebin"}]).

%% comando para chamar a função, antes deve ser criado um Json 
%% file:write_file("c:\\temp\\template.txt", io_lib:fwrite("~p.\n", [binary_to_list(ems_page:render(page114357434,Json))])).

render(PageModule, Args) ->	
	case PageModule:render(compile_json(Args)) of
		{ok, Response} -> erlang:iolist_to_binary(Response);
		Reason -> throw({einvalid_page, Reason, page,  PageModule})
	end.
	
handle(Req,PageModule,Args) ->
     Body = render(PageModule,compile_json(Args)),
				io:format(">>>>>>>>>>>>>>>>>>>> ~p",[Body]),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Body, Req),
    {ok, Req2}.

compile_json(Json) ->
	jsx:decode(unicode:characters_to_binary(Json)).

