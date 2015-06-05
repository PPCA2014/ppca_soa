%% ---
%%  ppca_catalogo_service
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Aluno: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%---

-module(ppca_catalogo_service).

-behavior(gen_server). 

-include("../include/ppca_config.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client
-export([get_catalogo/0, lista_catalogo/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
-record(state, {catalogo}). 


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    ppca_logger:info_msg("ppca_catalogo_service iniciado."),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
lista_catalogo(HeaderDict, From) ->
	gen_server:cast(?SERVER, {lista_catalogo, HeaderDict, From}).
	
get_catalogo() ->
	gen_server:call(?SERVER, lista_catalogo).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	Cat = do_get_catalogo(),
	NewState = #state{catalogo=Cat},
    {ok, NewState}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({lista_catalogo, _HeaderDict, From}, State) ->
	{Result, NewState} = do_lista_catalogo(State),
	From ! {ok, Result}, 
	{noreply, NewState}.
    
handle_call(lista_catalogo, _From, State) ->
	{Result, NewState} = do_lista_catalogo(State),
	{reply, Result, NewState}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ppca_logger:info_msg("ppca_catalogo_service finalizado."),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================

%% @doc Serviço que lista o catálogo
do_lista_catalogo(State) ->
	Cat = State#state.catalogo,
	{Cat, State}.

%% @doc Lê o catálogo do disco e retorna uma variável do tipo map
do_get_catalogo() -> 
	Cat = do_get_catalogo_from_disk(),
	{ok, Cat2} = ppca_util:json_decode_as_map(Cat),
	Cat2.

%% @doc Lê o catálogo do disco
do_get_catalogo_from_disk() ->
	{ok, Cat} = file:read_file(?CATALOGO_PATH),
	Cat.



