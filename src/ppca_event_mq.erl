%% ---
%%  PPCA_SOA
%%  Publish and subscribe message queue
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Aluno: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%---

-module(ppca_event_mq).
-behavior(gen_server).

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([adiciona_evento/1, 
		 registra_interesse/2, 
		 desregistra_interesse/2, 
		 lista_evento/0, 
		 lista_interesse/0, 
		 notifica_evento/2,
		 cancela_evento/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {lista_evento = [], 
			    lista_interesse = []}).


%%====================================================================
%% Server API
%%====================================================================

start() ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    ppca_logger:info_msg("ppca_event_mq iniciado."),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Client API
%%====================================================================
 
adiciona_evento(Evento) -> 
	gen_server:call(?SERVER, {adiciona_evento, Evento}). 
cancela_evento(Evento) -> 
	gen_server:call(?SERVER, {cancela_evento, Evento}). 
registra_interesse(Evento, Fun) -> 
	gen_server:call(?SERVER, {registra_interesse, Evento, Fun}). 
desregistra_interesse(Evento, Fun) -> 
	gen_server:call(?SERVER, {desregistra_interesse, Evento, Fun}). 
notifica_evento(QualEvento, Motivo) -> 
	gen_server:call(?SERVER, {notifica_evento, QualEvento, Motivo}). 
lista_evento() -> 
	gen_server:call(?MODULE, msg_lista_evento). 
lista_interesse() -> 
	gen_server:call(?SERVER, lista_interesse). 


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State}.
    
handle_call({adiciona_evento, Evento}, _From, State) ->
	{Reply, NewState} = novo_evento(Evento, State),
	{reply, Reply, NewState};

handle_call({cancela_evento, Evento}, _From, State) ->
	{Reply, NewState} = cancela_evento(Evento, State),
	{reply, Reply, NewState};

handle_call({registra_interesse, Evento, Fun}, _From, State) ->
	{Reply, NewState} = novo_interesse(Evento, Fun, State),
	{reply, Reply, NewState};

handle_call({desregistra_interesse, Evento, Fun}, _From, State) ->
	{Reply, NewState} = remove_interesse(Evento, Fun, State),
	{reply, Reply, NewState};
    
handle_call({notifica_evento, QualEvento, Motivo}, _From, State) ->
	Reply = notifica_evento(QualEvento, Motivo, State),
	{reply, Reply, State};
    
handle_call(msg_lista_evento, _From, State) ->
	Reply = lista_evento(State),
	{reply, Reply, State};

handle_call(lista_interesse, _From, State) ->
	Reply = lista_interesse(State),
	{reply, Reply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ppca_logger:info_msg("ppca_event_mq finalizado."),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================
    
existe_evento(Evento, State) ->
	lists:member(Evento, State#state.lista_evento).

existe_interesse(Interesse, State) ->
	lists:member(Interesse, State#state.lista_interesse).
    
novo_evento(Evento, State) ->
	case existe_evento(Evento, State) of
		true  -> {eeventojacadastrado, State};
		false -> {ok, State#state{lista_evento=[Evento|State#state.lista_evento]}}
	end.

cancela_evento(Evento, State) ->
	{ok, State#state{lista_evento = State#state.lista_evento -- [Evento], 
		  			 lista_interesse = [I || {E,_} = I <- State#state.lista_interesse, E /= Evento]}}.

novo_interesse(Evento, Fun, State) ->
	case existe_evento(Evento, State) of
		true -> 
			case existe_interesse({Evento, Fun}, State) of
				true  -> {einteressejacadastrado, State};
				false -> {ok, State#state{lista_interesse=[{Evento, Fun}|State#state.lista_interesse]}}
			end;
		false -> {eeventonaocadastrado, State}
	end.

remove_interesse(Evento, Fun, State) ->
	case existe_interesse({Evento, Fun}, State) of
		true  -> {ok, State#state{lista_interesse = State#state.lista_interesse -- [{Evento, Fun}]}};
		false -> {einteressenaocadastrado, State}
	end.
		
notifica_evento(QualEvento, Motivo, State) ->
	case existe_evento(QualEvento, State) of
		true  -> 
			[Fun(QualEvento, Motivo) || {Evento, Fun} <- State#state.lista_interesse, Evento == QualEvento],
			ok;
		false -> eeventonaocadastrado
	end.

lista_evento(State) -> State#state.lista_evento.	

lista_interesse(State) -> State#state.lista_interesse.	
