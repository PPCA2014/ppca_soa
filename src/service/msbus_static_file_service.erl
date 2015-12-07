%%********************************************************************
%% @title Módulo msbus_static_file_service
%% @version 1.0.0
%% @doc Módulo para gerenciamento de arquivos estáticos.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_static_file_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).


-include("../../include/msbus_config.hrl").
-include("../../include/msbus_schema.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Cliente interno API
-export([execute/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
-record(state, {cache}). 


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
execute(Request, From) ->
	msbus_pool:cast(msbus_static_file_service_pool, {get_file, Request, From}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    create_shared_cache(),
    {ok, #state{}}.

    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({get_file, Request, _From}, State) ->
	Result = do_get_file(Request, State),
	msbus_eventmgr:notifica_evento(ok_request, {static_file, Request, Result}),
	{noreply, State}.
    
handle_call({get_file, Request}, _From, State) ->
	Result = do_get_file(Request, State),
	{reply, Result, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================

do_get_file(Request, _State) ->
	FilePath = ?STATIC_FILE_PATH ++ Request#request.url,
	msbus_cache:get(static_file_cache, Request#request.servico#servico.result_cache, FilePath, 
		fun() -> 
			case file:read_file(FilePath) of
				{ok, Arquivo} -> 
					ContentType = msbus_http_util:mime_type(filename:extension(FilePath)),
					{ok, Arquivo, ContentType};
				{error, enoent} -> 
					{error, file_not_found};
				{error, Reason} -> 
					{error, servico_falhou, Reason}
			end
		end).

create_shared_cache() ->
	try
		msbus_cache:new(static_file_cache)
	catch
		_Exception:_Reason ->  ok
	end.
