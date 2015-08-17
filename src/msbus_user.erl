%%********************************************************************
%% @title Módulo msbus_user
%% @version 1.0.0
%% @doc Gerencia informações sobre os users
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_user).

-behavior(gen_server). 
-behaviour(poolboy_worker).


-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API  
-export([start/0, start_link/1, stop/0]).

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
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
call(Msg) -> 
	poolboy:transaction(msbus_user_pool, fun(Worker) ->
		gen_server:call(Worker, Msg)
    end).

cast(Msg) -> 
	poolboy:transaction(msbus_user_pool, fun(Worker) ->
		gen_server:cast(Worker, Msg)
    end).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
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

do_get(Id) -> msbus_db:get(user, Id).

do_insert(User) -> 
	case valida(User, insert) of
		ok -> msbus_db:insert(User);
		Error -> 
			io:format("~p\n", [Error]),
			Error
	end.

do_update(User) -> 
	case valida(User, update) of
		ok -> msbus_db:update(User);
		Error -> Error
	end.

do_all() -> msbus_db:all(user).

do_delete(Id) -> 
	case valida(null, delete) of
		ok -> 
			msbus_db:delete(user, Id);
		Error -> Error
	end.

valida(User, insert) ->
	case msbus_consiste:mensagens([msbus_consiste:msg_campo_obrigatorio("nome", User#user.nome),
								   msbus_consiste:msg_campo_obrigatorio("email", User#user.email),
								   msbus_consiste:msg_campo_obrigatorio("senha", User#user.senha)]) of
		[] -> 
			case msbus_consiste:msg_email_invalido("email", User#user.email) of
				[] ->
					case msbus_consiste:msg_registro_ja_existe({user, '_', User#user.nome, '_', '_'}, 
																<<"O nome do usuário já está cadastrado."/utf8>>) of
						[] -> 
							case msbus_consiste:msg_registro_ja_existe({user, '_', '_', User#user.email, '_'}, 
																	    <<"O email do usuário já está cadastrado."/utf8>>) of
								[] -> ok; 
								Msg -> {error, Msg}
							end;
						Msg -> {error, Msg}
					end;
				Msg -> {error, Msg}
			end;
		Msgs -> {error, Msgs}
	end;

valida(User, update) ->	valida(User, insert);

valida(_User, delete) -> ok.	
	
