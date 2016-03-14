%%********************************************************************
%% @title Module ems_user
%% @version 1.0.0
%% @doc Manages information about users
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user).

-behavior(gen_server). 
-behaviour(poolboy_worker).


-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Server API  
-export([start/0, start_link/1, stop/0]).

%% Cliente interno API
-export([call/1	, cast/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do service. 
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
 
call(Msg) -> ems_pool:call(ems_user_pool, Msg).

cast(Msg) -> ems_pool:cast(ems_user_pool, Msg).


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

handle_call({find_by_username_and_password, Username, Password}, _From, State) ->
	Reply = find_by_username_and_password(Username, Password),
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
%% Internal functions
%%====================================================================

do_get(Id) -> ems_db:get(user, Id).

do_insert(User) -> 
	case valida(User, insert) of
		ok -> ems_db:insert(User);
		Error -> 
			io:format("~p\n", [Error]),
			Error
	end.

do_update(User) -> 
	case valida(User, update) of
		ok -> ems_db:update(User);
		Error -> Error
	end.

do_all() -> ems_db:all(user).

do_delete(Id) -> 
	case valida(null, delete) of
		ok -> 
			ems_db:delete(user, Id);
		Error -> Error
	end.

valida(User, insert) ->
	case ems_consist:mensagens([ems_consist:msg_campo_obrigatorio("name", User#user.name),
								   ems_consist:msg_campo_obrigatorio("email", User#user.email),
								   ems_consist:msg_campo_obrigatorio("password", User#user.password)]) of
		[] -> 
			case ems_consist:msg_email_invalido("email", User#user.email) of
				[] ->
					case ems_consist:msg_registro_ja_existe({user, '_', User#user.name, '_', '_'}, 
																<<"O name do usuário já está cadastrado."/utf8>>) of
						[] -> 
							case ems_consist:msg_registro_ja_existe({user, '_', '_', User#user.email, '_'}, 
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

find_by_username_and_password(Username, Password) ->
	Query = fun() ->
		  qlc:e(
			 qlc:q([R || R <- mnesia:table(user), 
						 R#user.name == Username,
						 R#user.password == Password])
		  )
	   end,
	case mnesia:transaction(Query) of
		{atomic, []} -> {error, notfound};
		{atomic, [Record|_]} -> {ok, Record};
		{aborted, _Reason} -> {error, aborted}
	end.

	
