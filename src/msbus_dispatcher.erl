%%********************************************************************
%% @title Módulo dispatcher
%% @version 1.0.0
%% @doc Responsável pelo encaminhamento das requisições de/para os serviços REST.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
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
-export([dispatch_request/1]).

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

dispatch_request(Request) -> 
	msbus_pool:cast(msbus_dispatcher_pool, {dispatch_request, Request}).

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
 
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({dispatch_request, Request}, State) ->
	do_dispatch_request(Request),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.
    
handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State) ->
   {noreply, State}.

handle_info({servico, RID, Reply}, State) ->
	io:format("aqui1\n"),
	{ok, Request} = msbus_request:get_request_rid(RID),
	io:format("aqui2\n"),
	msbus_eventmgr:notifica_evento(ok_request, {servico, Request, Reply}),
	{noreply, State};

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Despacha o request para o serviço registrado no catálogo
do_dispatch_request(Request) ->
	case msbus_catalogo:lookup(Request) of
		{ok, Request1} -> 
			msbus_request:registra_request(Request1),
			case msbus_auth_user:autentica(Request1) of
				{ok, User} ->
					Request2 = Request1#request{user = User},
					msbus_eventmgr:notifica_evento(new_request, Request2),
					case executa_servico(Request2) of
						ok -> ok;
						Error -> msbus_eventmgr:notifica_evento(erro_request, {servico, Request2, Error})
					end;
				{error, no_authorization} -> msbus_eventmgr:notifica_evento(erro_request, {servico, Request1, {error, no_authorization}})
			end;
		notfound -> 
			msbus_request:registra_request(Request),
			msbus_eventmgr:notifica_evento(erro_request, {servico, Request, {error, notfound}})
	end,
	ok.

%% @doc Executa o serviço local (Serviço escrito em Erlang)
executa_servico(Request=#request{servico=#servico{host='', 
												  host_name = HostName,	
												  module=Module, 
												  module_name = ModuleName, 
												  function_name = FunctionName, 
												  function=Function}}) ->
	msbus_logger:info("CAST ~s:~s em ~s {RID: ~p, URI: ~s}.", [ModuleName, FunctionName, HostName, Request#request.rid, Request#request.uri]),
	try
		case whereis(Module) of
			undefined -> 
				Module:start(),
				apply(Module, Function, [Request, self()]),
				ok;
			_Pid -> 
				apply(Module, Function, [Request, self()]),
				ok
		end
	catch
		_Exception:ErroInterno ->  {error, servico_falhou, ErroInterno}
	end;

%% @doc Executa o serviço em outro host (Serviço escrito em outra plataforma/linguagem)
executa_servico(Request=#request{servico=#servico{host = Host, 
												  host_name = HostName,	
												  module_name = ModuleName, 
												  function_name = FunctionName, 
												  module = Module}}) ->
	msbus_logger:info("CAST ~s:~s em ~s {RID: ~p, URI: ~s}.", [ModuleName, FunctionName, HostName, Request#request.rid, Request#request.uri]),
	{Module, Host} ! {{Request#request.rid, 
					   Request#request.uri, 
					   Request#request.type, 
					   Request#request.params_url, 
					   Request#request.querystring_map,
					   Request#request.payload,	
					   Request#request.content_type,	
					   ModuleName,
					   FunctionName}, 
					   self()
					  },
	ok.
