%%********************************************************************
%% @title Módulo ppca_user
%% @version 1.0.0
%% @doc Gerencia informações sobre os users
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(ppca_user).

-behavior(gen_server). 

-include("../include/ppca_config.hrl").
-include("../include/ppca_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API  
-export([start/0, stop/0]).

%% Cliente interno API
-export([call/1	, cast/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
-record(state, {}). 



%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
call(Msg) -> 
	gen_server:call(?SERVER, Msg).

cast(Msg) -> 
	gen_server:cast(?SERVER, Msg).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({get, Id, From}, State) ->
	Reply = do_get(Id),
	From ! Reply, 
	{noreply, State};

handle_cast({insert, User, From}, State) ->
	Reply = do_insert(User),
	From ! Reply, 
	{noreply, State};

handle_cast({update, User, From}, State) ->
	Reply = do_update(User),
	From ! Reply, 
	{noreply, State};

handle_cast({delete, Id, From}, State) ->
	Reply = do_delete(Id),
	From ! {ok, Reply}, 
	{noreply, State};

handle_cast({all, From}, State) ->
	Reply = do_all(),
	From ! Reply, 
	{noreply, State}.
    
handle_call({get, Id}, _From, State) ->
	Reply = do_get(Id),
	{reply, Reply, State};

handle_call({insert, User}, _From, State) ->
	Reply = do_insert(User),
	{reply, Reply, State};

handle_call({update, User}, _From, State) ->
	Reply = do_update(User),
	{reply, Reply, State};

handle_call({delete, Id}, _From, State) ->
	Reply = do_delete(Id),
	{reply, Reply, State};

handle_call(all, _From, State) ->
	Reply = do_all(),
	{reply, Reply, State}.

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

do_get(Id) -> ppca_dao:get(user, Id).

do_insert(User) -> 
	case valida(User, insert) of
		ok -> ppca_dao:insert(User);
		Error -> Error
	end.

do_update(User) -> 
	case valida(User, update) of
		ok -> ppca_dao:update(User);
		Error -> Error
	end.

do_all() -> ppca_dao:all(user).

do_delete(Id) -> 
	case valida(null, delete) of
		ok -> 
			ppca_dao:delete(user, Id);
		Error -> Error
	end.

valida(User, insert) ->
	Msgs = ppca_util:mensagens(
				[ppca_util:msg_campo_obrigatorio("nome", User#user.nome),
				 ppca_util:msg_campo_obrigatorio("email", User#user.email),
				 ppca_util:msg_email_invalido("email", User#user.email)]),
	case Msgs of
		[] -> ok;
		_ -> {error, Msgs}
	end;

valida(User, update) ->	valida(User, insert);

valida(_User, delete) -> ok.	
	




