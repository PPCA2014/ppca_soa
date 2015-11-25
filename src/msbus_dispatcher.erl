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
    createEtsControle(),
    {ok, #state{}}.
 
createEtsControle() ->
    try
		ets:new(ctrl_node_dispatch, [ordered_set, named_table, public])
	catch
		_Exception:_Reason -> ok
	end,

    try
		ets:new(ctrl_ping_cache, [ordered_set, named_table, public])
	catch
		_Exception2:_Reason2 -> ok
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

handle_info({servico, RID, Reply}, State) ->
	{ok, Request} = msbus_request:get_request_rid(RID),
	msbus_eventmgr:notifica_evento(ok_request, {servico, Request, Reply}),
	{noreply, State};

handle_info({request, Reply, From}, State) ->
	From ! {{"Pong", Reply}, self()},
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
						{ok, Request3} -> 
							msbus_request:registra_request(Request3),
							ok;
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
				Request2 = Request#request{node_exec = node()},
				apply(Module, Function, [Request2, self()]),
				{ok, Request2};
			_Pid -> 
				Request2 = Request#request{node_exec = node()},
				apply(Module, Function, [Request, self()]),
				{ok, Request2}
		end
	catch
		_Exception:ErroInterno ->  {error, servico_falhou, ErroInterno}
	end;

%% @doc Executa um serviço remotamente
executa_servico(Request=#request{servico=#servico{host = HostList, 
												  host_name = HostNames,	
												  module_name = ModuleName, 
												  function_name = FunctionName, 
												  module = Module,
												  node = NodeList}}) ->
	case get_work_node(HostList, HostNames, ModuleName, NodeList) of
		{ok, Node} ->
			msbus_logger:info("CAST ~s:~s em ~s {RID: ~p, URI: ~s}.", [ModuleName, FunctionName, atom_to_list(Node), Request#request.rid, Request#request.uri]),
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
			{ok, Request#request{node_exec = Node}};
		Error -> Error
	end.


get_work_node([], HostNames, _ModuleName, _NodeList) -> 
	Motivo = lists:flatten(string:join(HostNames, ", ")),
	{error, servico_fora, Motivo};

get_work_node([_H|T]=HostList, HostNames, ModuleName, NodeList) -> 
	case ets:lookup(ctrl_node_dispatch, ModuleName) of
		[] -> 
			Index = 1;
		[{_, Idx}] -> 
			ets:delete(ctrl_node_dispatch, ModuleName),
			Index = Idx+1
	end,
	case Index > length(HostList) of
		true -> Index2 = 1;
		false -> Index2 = Index
	end,
	ets:insert(ctrl_node_dispatch, {ModuleName, Index2}),
	Node = lists:nth(Index2, HostList),
	case is_node_alive(Node) of
		true  -> {ok, Node};
		false -> get_work_node(T, HostNames, ModuleName, NodeList)
	end.

is_node_alive(Node) -> 
	case ets:lookup(ctrl_ping_cache, Node) of
		[] -> 
		io:format("aqui1\n"),
		Hit = net_adm:ping(Node) =:= pong;
		{Node, Time, Hit} -> 
		io:format("aqui2\n"),
			Time2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),	
			case (Time2 - Time) < 10 of
				true -> io:format("aqui4\n"), ok;
				false ->
				io:format("aqui3\n"),
				 Hit = net_adm:ping(Node) =:= pong
			end
	end,
	io:format("aqui5\n"),
	NewTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	ets:delete(ctrl_ping_cache, Node),
	ets:insert(ctrl_ping_cache, {Node, NewTime, Hit}).
	
		
						
	
	
	


