%%********************************************************************
%% @title Module ems_eventmgr
%% @version 1.0.0
%% @doc Module publisher/subscribe of ErlangMS.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_eventmgr).

-behavior(gen_server).

-include("../include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).

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

-record(state, {lista_evento = [new_request, ok_request,              %% predefined events
								erro_request, close_request, 
								send_error_request], 
			    lista_interesse = []}).


%%====================================================================
%% Server API
%%====================================================================

start(_) ->
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
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
notifica_evento(ok_request, Msg = {_, #request{worker_send = Worker}, _}) -> 
	gen_server:cast(Worker, Msg),  %% envia a mensagem para quem solicitou antes para obter certa prioridade
	gen_server:cast(?SERVER, {notifica_evento, new_request, Msg});  %% notifica os demais serviços que estão interessados (Ex. ems_logger)
notifica_evento(erro_request, Msg = {_, #request{worker_send = Worker}, _}) -> 
	gen_server:cast(Worker, Msg),  %% envia a mensagem para quem solicitou antes para obter certa prioridade
	gen_server:cast(?SERVER, {notifica_evento, erro_request, Msg});  %% notifica os demais serviços que estão interessados (Ex. ems_logger)
notifica_evento(close_request, Msg) -> 
	ems_logger:log_request(Msg),
	gen_server:cast(?SERVER, {notifica_evento, close_request, Msg});  %% notifica os demais serviços que estão interessados (Ex. ems_logger)
notifica_evento(QualEvento, Msg) -> 
	gen_server:cast(?SERVER, {notifica_evento, QualEvento, Msg}).
lista_evento() -> 
	gen_server:call(?SERVER, msg_lista_evento). 
lista_interesse() -> 
	gen_server:call(?SERVER, lista_interesse). 


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
	%fprof:trace([start, {procs, [self()]}]),
    {ok, #state{}}.
    
handle_cast({notifica_evento, QualEvento, Motivo}, State) ->
	notifica_evento(State#state.lista_interesse, QualEvento, Motivo),
	{noreply, State};
    
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
    
handle_call(msg_lista_evento, _From, State) ->
	Reply = lista_evento(State),
	{reply, Reply, State};

handle_call(lista_interesse, _From, State) ->
	Reply = lista_interesse(State),
	{reply, Reply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
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
		
notifica_evento([], _QualEvento, _) -> ok;
notifica_evento([{Evento, Fun} = _H|T], QualEvento, Msg) ->
	case Evento == QualEvento of
		true ->
			try
				Fun(QualEvento, Msg)
			after
				notifica_evento(T, QualEvento, Msg)		
			end;
		false ->
			notifica_evento(T, QualEvento, Msg)
	end.
	
lista_evento(State) -> State#state.lista_evento.	

lista_interesse(State) -> State#state.lista_interesse.	
