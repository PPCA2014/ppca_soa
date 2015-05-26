%% ---
%%  ppca_favicon_service
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Aluno: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%---

-module(ppca_favicon_service).

-behavior(gen_server). 

-include("../include/ppca_config.hrl").

%% Server API
-export([start/0, stop/0]).

%% Cliente interno API
-export([execute/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
-record(state, {arquivo}). 


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    ppca_logger:info_msg("ppca_favicon_service iniciado."),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
execute(HeaderDict, From)	->
	gen_server:cast(?SERVER, {favicon, HeaderDict, From}).
	


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	Arquivo = get_favicon_from_disk(),
	NewState = #state{arquivo=Arquivo},
    {ok, NewState}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({favicon, HeaderDict, From}, State) ->
	{Result, NewState} = do_get_favicon(HeaderDict, State),
	From ! {ok, Result}, 
	{noreply, NewState}.
    
handle_call({favicon, HeaderDict}, _From, State) ->
	{Result, NewState} = do_get_favicon(HeaderDict, State),
	{reply, Result, NewState}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ppca_logger:info_msg("ppca_favicon_service finalizado."),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================

get_favicon_from_disk()->
	{ok, Arquivo} = file:read_file(?FAVICON_PATH),
	Arquivo.
    
do_get_favicon(_HeaderDict, State) ->
	Result = {favicon, State#state.arquivo},
	{Result, State}.

