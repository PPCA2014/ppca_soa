%%********************************************************************
%% @title Module ems_dispatcher
%% @version 1.0.0
%% @doc Responsible for forwarding the requests to services.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_dispatcher).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Client API
-export([start/0, dispatch_request/1]).


start() -> 
	ets:new(ctrl_node_dispatch, [set, named_table, public]),
	ems_dispatcher_cache:start().


dispatch_request(Request = #request{type = "GET", 
									url_hash = UrlHash, 
									t1 = Timestamp}) -> 
	case ems_dispatcher_cache:lookup(UrlHash, Timestamp) of
		{true, RequestCache} -> 
			{ok, Request#request{result_cache = true,
								 code = RequestCache#request.code,
								 reason = RequestCache#request.reason,
								 response_data = RequestCache#request.response_data,
								 response_header = RequestCache#request.response_header,
								 result_cache_rid = RequestCache#request.rid,
								 latency = RequestCache#request.latency}};
		false -> lookup_request(Request)
	end;
dispatch_request(Request) -> lookup_request(Request).
	
lookup_request(Request) -> 
	case ems_catalog:lookup(Request) of
		{Service, ParamsMap, QuerystringMap} -> 
			case ems_auth_user:autentica(Service, Request) of
				{ok, User} ->
					case get_work_node(Service#service.host, 
									   Service#service.host,	
									   Service#service.host_name, 
									   Service#service.module_name, 1) of
						{ok, Node} ->
							Request2 = Request#request{user = User, 
													   node_exec = Node,
													   service = Service,
													   params_url = ParamsMap,
													   querystring_map = QuerystringMap},
							ems_request:registra_request(Request2),
							case executa_service(Node, Request2) of
								ok -> 
									receive
										{_, {_, _, {ok, Response}}} -> 
											{ok, Request2#request{response_data = Response}};
										{_, {_, _, Response}} -> 
											{ok, Request2#request{response_data = Response}};
										Msg -> io:format("Msg service desconhecida: ~p\n", [Msg])
										after Request2#request.service#service.timeout -> {error, etimeout_service}
									end;
								Error -> Error
							end;
						Error ->  Error
					end;
				Error -> Error
			end;
		Error -> Error
	end.


%% @doc Executa um web service local
executa_service(_Node, Request=#request{service=#service{host='', 
														 module=Module, 
														 function=Function}}) ->
	try
		case whereis(Module) of
			undefined -> 
				ems_pool:cast(ems_web_service, Request);
			_Pid -> apply(Module, Function, [Request, self()])
		end,
		ok
	catch
		_Exception:ErroInterno ->  {error, eservice_fail, {Module, ErroInterno}}
	end;
	
%% @doc Executa um web service remoto
executa_service(Node, Request=#request{service=#service{host = _HostList, 
														host_name = _HostNames,	
														module_name = ModuleName, 
														function_name = FunctionName, 
														module = Module}}) ->
	WebService = ems_pool:checkout(ems_web_service),
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
					   WebService
					  },
	{Module, Node} ! Msg,
	ems_pool:checkin(ems_web_service, WebService),
	ok.

get_work_node('', _, _, _, _) -> {ok, node()};
get_work_node([], HostList, HostNames, ModuleName, 1) -> 
	get_work_node(HostList, HostList, HostNames, ModuleName, 2);
get_work_node([], _, _, _, 2) -> {error, eunavailable_service};
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
		
