%% ---
%%  helloworld_service
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Aluno: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%---

-module(helloworld_service).

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
-record(state, {}). 


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    ppca_logger:info_msg("helloworld_service iniciado."),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
execute(HeaderDict, From)	->
	gen_server:cast(?SERVER, {hello_world, HeaderDict, From}).
	


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({hello_world, HeaderDict, From}, State) ->
	{Response, NewState} = do_hello_world(HeaderDict, State),
	From ! {ok, Response}, 
	{noreply, NewState}.
    
handle_call({hello_world, HeaderDict}, _From, State) ->
	{Response, NewState} = do_hello_world(HeaderDict, State),
	{reply, Response, NewState}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ppca_logger:info_msg("helloworld_service finalizado."),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================
    
do_hello_world(_HeaderDict, State) ->
	% simular operação demorada
	%timer:sleep(2 * 1000),
	Response = "{\"message\": \"Ola mundo!!!\"}",
	NewState = State#state{},
	{Response, NewState}.

