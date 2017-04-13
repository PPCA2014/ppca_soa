%%********************************************************************
%% @title Module ems_permission_loader
%% @version 1.0.0
%% @doc ems_permission_loader
%% @author Renato Carauta Ribeiro <rcarauta6@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_template_loader).

%-behavior(gen_server).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

% Caminho do diretório atual
-define(CURRENT_PATH, file:get_cwd()).

%%Server API
-export([start/1,stop/0]).

%%get_setver callbacks
-export([init/1, terminate/2, render/2]).

-record(state,{service,
			   operation}).

-define(SERVER,?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Service) ->
    gen_server:start_link({local,?SERVER}, ?MODULE, Service, []).
    
stop() ->
    get_server:cast(?SERVER, shutdown).
    
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

init(#service{service = Service,
               properties = Props}) ->
        Template = maps:get(<<"url_template">>, Props),
        {ok,Path} = ?CURRENT_PATH,
        Page = Path++"/priv/www/"++binary_to_list(Template),
        SiteMap = maps:get(<<"file_path">>, Props),
        {Rowid, _Secoundid, _Thirdid} = random:seed(),
        State = #state{service = Service,
        			   operation = load_operation(Page,"C:/desenvolvimento/workspaceTmp/portal/",Rowid)},
        {ok, State}.
 

terminate(_Reason, _State) ->
	  ok.
	 
	

	
    
%%====================================================================
%% Internal functions
%%====================================================================

load_operation(Page,SiteMap,Rowid) ->
	ModuleNamePage =  "page" ++ integer_to_list(Rowid),
	case compile_file(Page, ModuleNamePage) of
		{ok, PageModule} ->
			JsonFile = SiteMap++"sitemap.json",
		    TemplateInput = SiteMap++"/navigator/navigator.html",
			{ok, File} = file:read_file(JsonFile),
			Content = unicode:characters_to_list(File),
			Rendered =  render(PageModule, erlang:iolist_to_binary(Content)),
			file:write_file("C:/desenvolvimento/workspaceTmp/portal/app/dashboard/navigator/navigator.html", Rendered),
			ok;
		_ -> throw({einvalid_page, Page})
	end.
	
render(PageModule, Args) ->	
	case PageModule:render(compile_json(Args)) of
		{ok, Response} -> 
		PageCompile = change_values(Response),
		erlang:iolist_to_binary(PageCompile);
		Reason -> throw({einvalid_page, Reason, page,  PageModule})
	end.
		
compile_file(Page, ModuleName) -> 
	erlydtl:compile_file(Page, ModuleName, [{out_dir, "ebin"}]).
	
	
compile_json(Json) ->
	jsx:decode(Json).
	     
change_values(PageModule) ->
	Page1 = re:replace(PageModule, "&lt;", "<", [global, {return, list}]),
	Page2 = re:replace(Page1, "&gt;", ">", [global, {return, list}]),
	Page2.
	
	
	
	
 

		










