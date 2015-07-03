%%********************************************************************
%% @title Módulo para gerenciamento de arquivos estáticos
%% @version 1.0.0
%% @doc Lê os arquivos do SO e envia para o servidor
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_static_file).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").

%% Server API
-export([start/0, stop/0]).

%% Cliente interno API
-export([execute/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
-record(state, {}). 


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    msbus_logger:info("msbus_static_file iniciado."),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
execute(Request, From)	->
	gen_server:cast(?SERVER, {get_file, Request, From}).
	


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	NewState = #state{},
    {ok, NewState}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({get_file, Request, From}, State) ->
	Result = do_get_file(Request),
	From ! Result, 
	{noreply, State}.
    
handle_call({get_file, Request}, _From, State) ->
	Result = do_get_file(Request),
	{reply, Result, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    msbus_logger:info("msbus_static_file finalizado."),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================

do_get_file(Request) ->
	FilePath = ?STATIC_FILE_PATH ++ msbus_util:get_property_request(<<"url">>, Request),
	case file:read_file(FilePath) of
		{ok, Arquivo} -> 
			ContentType = msbus_util:mime_type(filename:extension(FilePath)),
			{ok, Arquivo, ContentType};
		{error, enoent} -> 
			{error, file_not_found, FilePath};
		{error, Reason} -> 
			{error, servico_falhou, Reason}
	end.

