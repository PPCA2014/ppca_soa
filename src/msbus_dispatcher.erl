%%********************************************************************
%% @title Módulo dispatcher
%% @version 1.0.0
%% @doc Responsável pelo encaminhamento das requisições ao serviço REST.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_dispatcher).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include("../include/msbus_http_messages.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Client API
-export([dispatch_request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {}).

-define(SERVER, ?MODULE).

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
%% Client API
%%====================================================================

dispatch_request(Request, From) -> 
	msbus_pool:cast(msbus_dispatcher_pool, {dispatch_request, Request, From}).

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
 
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({dispatch_request, Request, From}, State) ->
	do_dispatch_request(Request, From),
	{noreply, State}.
    
handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

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

%% @doc Despacha o request para o service registrado no catálogo
do_dispatch_request(Request, From) ->
	case msbus_catalogo:lookup(Request) of
		{ok, Request1} -> 
			case msbus_request:registra_request(Request1) of
				{ok, Request2} ->
					msbus_eventmgr:notifica_evento(new_request, Request2),
					case executa_servico(Request2, From) of
						ok -> ok;
						Error -> msbus_eventmgr:notifica_evento(erro_request, {servico, Request2, Error})
					end;
				Error -> 
					msbus_eventmgr:notifica_evento(erro_request, {servico, Request1, Error})
			end;
		notfound -> 
			msbus_eventmgr:notifica_evento(erro_request, {servico, Request, {error, notfound}})
	end,
	ok.

%% @doc Executa o serviço local
executa_servico(Request=#request{servico=#servico{host='', module=Module, function=Function}}, From) ->
	try
		case whereis(Module) of
			undefined -> 
				Module:start(),
				apply(Module, Function, [Request, From]),
				ok;
			_Pid -> 
				apply(Module, Function, [Request, From]),
				ok
		end
	catch
		_Exception:ErroInterno ->  {error, servico_falhou, ErroInterno}
	end;

%% @doc Executa o serviço Java
executa_servico(Request=#request{servico=#servico{host=Host, module=Module, function=_Function}}, From) ->
	{Module, Host} ! {Request, From},
	%aguarda_msg_java(Request),
	ok.


%aguarda_msg_java(Request) ->
%			receive
%				{ok, V} -> 
%					io:format("recebido: ~p\n", [V]),
%					gen_server:cast(self(), {servico, Request, V});
%				Msg -> 
%					io:format("outra msg: ~p\n", [Msg]),
%					aguarda_msg_java(Request)
%			end.
			
	
	
