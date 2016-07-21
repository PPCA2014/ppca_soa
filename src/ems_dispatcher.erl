%%********************************************************************
%% @title Module ems_dispatcher
%% @version 1.0.0
%% @doc Responsible for forwarding the requests to / from the REST services.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_dispatcher).

-behavior(gen_server). 
-behaviour(poolboy_worker).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

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
	ems_pool:cast(ems_dispatcher_pool, {dispatch_request, Request}).

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    createEtsControle(),
    %fprof:trace([start, {procs, [self()]}]),
    {ok, #state{}}.
 
createEtsControle() ->
    try
		ets:new(ctrl_node_dispatch, [set, named_table, public])
	catch
		_:_ -> ok
	end.
 
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

handle_info({Code, RID, Reply}, State) ->
	case ems_request:get_request_em_andamento(RID) of
		{ok, Request} -> 
			ems_request:registra_request(Request),
			ems_eventmgr:notifica_evento(ok_request, {Code, Request, Reply}),
			{noreply, State};
		{erro, notfound} -> {noreply, State}
	end;

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Dispatches the request to the service registered in the catalog
do_dispatch_request(Request) ->
	case ems_catalog:lookup(Request) of
		{Service, ParamsMap, QuerystringMap} -> 
			case ems_auth_user:autentica(Service, Request) of
				{ok, User} ->
					case get_work_node(Service#service.host, 
									   Service#service.host,	
									   Service#service.host_name, 
									   Service#service.module_name, 1) of
						{ok, Node} ->
							io:format("aqui1\n\n"),

							Request2 = Request#request{user = User, 
													   node_exec = Node,
													   service = Service,
													   params_url = ParamsMap,
													   querystring_map = QuerystringMap},
							ems_request:registra_request(Request2),
							ems_eventmgr:notifica_evento(new_request, Request2),
							case executa_service(Node, Request2) of
								ok -> ok;
								Error -> ems_eventmgr:notifica_evento(erro_request, {service, Request2, Error})
							end;
						Error -> 
							ems_eventmgr:notifica_evento(erro_request, {service, Request, Error})
					end;
				{error, no_authorization} -> 
					ems_eventmgr:notifica_evento(erro_request, {service, Request, {error, no_authorization}})
			end;
		notfound -> 
			ems_request:registra_request(Request),
			ems_eventmgr:notifica_evento(erro_request, {service, Request, {error, notfound}});
		Erro ->
			io:format("ERRO -> ~p\n", [Erro])
	end,
	ok.

%% @doc Executa o serviço local (Serviço escrito em Erlang)
executa_service(_Node, Request=#request{service=#service{host='', 
														 host_name = HostName,	
														 module=Module, 
														 module_name = ModuleName, 
														 function_name = FunctionName, 
														 function=Function}}) ->
	try
		io:format("executa_service hostname=~p\n", [HostName]),
		io:format("Msg local enviada para ~p: ~p.", [Module, Request]),
		case whereis(Module) of
			undefined -> 
				Module:start(),
				ems_logger:debug("Serviço ~p não está ativo. Iniciando...", [Module]),
				apply(Module, Function, [Request, self()]);
			_Pid -> 
				apply(Module, Function, [Request, self()])
		end,
		ems_logger:info("CAST ~s:~s em ~s {RID: ~p, URI: ~s}.", [ModuleName, 
																   FunctionName, 
																   HostName, 
																   Request#request.rid, 
																   Request#request.uri]),
		ok
	catch
		_Exception:ErroInterno ->  {error, service_falhou, ErroInterno}
	end;

%% @doc Executa um serviço remotamente
executa_service(Node, Request=#request{service=#service{host = _HostList, 
														host_name = _HostNames,	
														module_name = ModuleName, 
														function_name = FunctionName, 
														module = Module}}) ->
	io:format("send do java!!!\n\n"),


	% Envia uma mensagem assíncrona para o serviço
	Msg = {{Request#request.rid, 
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
	ems_logger:debug("Msg enviada para ~p: ~p.", [Node, Msg]),
	{Module, Node} ! {{Request#request.rid, 
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
	ems_logger:info("CAST ~s:~s em ~s {RID: ~p, URI: ~s}.", [ModuleName, 
															   FunctionName, 
															   atom_to_list(Node), 
															   Request#request.rid, 
															   Request#request.uri]),
	ok.

get_work_node('', _, _, _, _) -> {ok, node()};

get_work_node([], HostList, HostNames, ModuleName, 1) -> 
	get_work_node(HostList, HostList, HostNames, ModuleName, 2);

get_work_node([], _HostList, HostNames, _ModuleName, 2) -> 
	Motivo = lists:flatten(string:join(HostNames, ", ")),
	{error, service_fora, Motivo};

get_work_node([_|T], HostList, HostNames, ModuleName, Tentativa) -> 
	%% Localiza a entrada do módulo na tabela hash
	case ets:lookup(ctrl_node_dispatch, ModuleName) of
		[] -> 
			% não encontrou, vamos selecionar o índice do primeiro node
			Index = 1;
		[{_, Idx}] -> 
			% encontrou um node que foi utilizado anteriormente, vamos usar o próximo
			ets:delete(ctrl_node_dispatch, ModuleName),
			Index = Idx+1
	end,
	% Pegamos o primeiro node quando Index maior que o tamanho da lista de nodes disponíveis
	case Index > length(HostList) of
		true -> Index2 = 1;
		false -> Index2 = Index
	end,
	% Inserimos na tabela hash os dados de controle
	ets:insert(ctrl_node_dispatch, {ModuleName, Index2}),

	% Qual node vamos selecionar
	Node = lists:nth(Index2, HostList),
	
	% Este node está vivo? Temos que rotear para um node existente
	case net_adm:ping(Node) of
		pong -> {ok, Node};
		pang -> get_work_node(T, HostList, HostNames, ModuleName, Tentativa)
	end.
		
