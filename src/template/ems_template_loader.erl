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

%%Server API
-export([start/1,stop/0]).

%%get_setver callbacks
-export([init/1, terminate/2]).

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
        Page = maps:get(<<"url_template">>, Props),
        {Rowid, _Secoundid, _Thirdid} = random:seed(),
        State = #state{service = Service,
        			   operation = load_operation(Page,Rowid)},
        {ok, State}.
 

terminate(_Reason, _State) ->
	  ok.
	  
 
    
%%====================================================================
%% Internal functions
%%====================================================================

load_operation(Page,Rowid) ->
	io:format("Verificar ate aqui   ~n"),
	ModuleNamePage =  "page" ++ integer_to_list(Rowid),
	io:format("Verificar se foi formado corretamente   ~p~n",[ModuleNamePage]),
	io:format("Page name is ~p~n",[ModuleNamePage]),
	case compile_file(binary_to_list(Page), ModuleNamePage) of
		{ok, PageModule} ->  PageModule;
		_ -> throw({einvalid_page, Page})
	end.
		
compile_file(Page, ModuleName) -> 
	erlydtl:compile_file(Page, ModuleName, [{out_dir, "ebin"}]).
	     
	  

		












