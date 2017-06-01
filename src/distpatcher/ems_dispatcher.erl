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

%% Client API
-export([start/0, dispatch_request/1, dispatch_service_work/2]).


start() -> 
	ets:new(ctrl_node_dispatch, [set, named_table, public]),
	ems_dispatcher_cache:start().


dispatch_request(Request = #request{type = "GET", 
									req_hash = ReqHash, 
									t1 = Timestamp}) -> 
	case ems_dispatcher_cache:lookup(ReqHash, Timestamp) of
		{true, RequestCache} -> 
			?DEBUG("ems_dispatcher lookup request in cache. ReqHash: ~p.", [ReqHash]),
			{ok, request, Request#request{result_cache = true,
										  code = RequestCache#request.code,
										  reason = RequestCache#request.reason,
										  response_data = RequestCache#request.response_data,
										  response_header = RequestCache#request.response_header,
										  result_cache_rid = RequestCache#request.rid,
										  latency = RequestCache#request.latency,
										  service = RequestCache#request.service,
										  querystring_map = RequestCache#request.querystring_map}};
		false -> lookup_request(Request)
	end;
dispatch_request(Request) -> lookup_request(Request).
	
lookup_request(Request = #request{url = Url,
								  ip = Ip,
								  ip_bin = IpBin,
								  content_type = ContentTypeReq}) -> 
	?DEBUG("ems_dispatcher lookup request ~p.", [Request]),
	case ems_catalog:lookup(Request) of
		{Service = #service{content_type = ContentTypeService,
							tcp_allowed_address_t = AllowedAddress}, 
		 ParamsMap, 
		 QuerystringMap} -> 
			?DEBUG("ems_dispatcher lookup request found."),
			case ems_tcp_util:allow_ip_address(Ip, AllowedAddress) of
				true ->
					case ems_auth_user:authenticate(Service, Request) of
						{ok, User} -> 
							ContentType = case ContentTypeReq of
											  undefined -> ContentTypeService;
											  _ -> ContentTypeReq
										  end,
							Request2 = Request#request{service = Service,
														params_url = ParamsMap,
														querystring_map = QuerystringMap,
														user = User,
														content_type = ContentType},
							dispatch_service_work(Request2, Service);
						Error -> Error
					end;
				false -> 
					ems_logger:warn("ems_dispatcher does not grant access to IP ~p. Reason: IP denied.", [IpBin]),
					{error, host_denied}
			end;
		{error, Reason} = Error -> 
			ems_logger:warn("ems_dispatcher request ~p not found. Reason: ~p.", [Url, Reason]),
			Error
	end.


dispatch_service_work(Request = #request{type = Type,
										 req_hash = ReqHash,
										 t1 = T1},
					 _Service = #service{name = ServiceName,
										 owner = ServiceOwner,
										 host = '',
										 module_name = ModuleName,
										 module = Module,
										 function = Function,
										 result_cache = ResultCache}) ->
	ems_logger:info("ems_dispatcher send local msg to ~s.", [ModuleName]),
	{Reason, Request3 = #request{response_header = ResponseHeader}} = apply(Module, Function, [Request]),
	AllowResultCache = Reason =:= ok andalso Type =:= "GET",
	Request4 = Request3#request{response_header = ResponseHeader#{<<"ems-node">> => node_binary(),
																  <<"ems-catalog">> => ServiceName,
																  <<"ems-owner">> => ServiceOwner,
																  <<"ems-result-cache">> => case AllowResultCache of true -> integer_to_binary(ResultCache); _ -> <<"0">> end},
								latency = ems_util:get_milliseconds() - T1},
	case dispatch_middleware_function(Request4, onrequest) of
		{ok, Request5} ->
			case AllowResultCache of
				true -> ems_dispatcher_cache:add(ReqHash, T1, Request5, ResultCache);
				false -> ems_dispatcher_cache:invalidate()
			end,
			{Reason, request, Request5};
		{error, Reason} = Error ->
			{error, request, Request#request{code = 500,
											 reason = Reason,
											 response_header = #{<<"ems-node">> => node_binary(),
																 <<"ems-catalog">> => ServiceName,
																 <<"ems-owner">> => ServiceOwner},
											 response_data = ems_schema:to_json(Error),
											 latency = ems_util:get_milliseconds() - T1}}
	end;
dispatch_service_work(Request = #request{rid = Rid,
										  type = Type,
										  url = Url,
										  req_hash = ReqHash,
										  payload = Payload,
										  t1 = T1,
										  content_type = ContentType,  
										  params_url = ParamsMap,
										  querystring_map = QuerystringMap,
										  user = User},
					  Service = #service{name = ServiceName,
										 owner = ServiceOwner,
										 host = Host,
										 host_name = HostName,
										 module_name = ModuleName,
										 module = Module,
										 function_name = FunctionName, 
										 timeout = Timeout,
										 result_cache = ResultCache}) ->
	case get_work_node(Host, Host, HostName, ModuleName, 1) of
		{ok, Node} ->
			Msg = {{Rid, Url, Type, ParamsMap, QuerystringMap, Payload, ContentType, ModuleName, FunctionName}, self()},
			{Module, Node} ! Msg,
			NodeBin = erlang:atom_to_binary(Node, utf8),
			ems_logger:info("ems_dispatcher send msg to ~p with timeout ~pms.", [{Module, Node}, Timeout]),
			receive 
				{Code, RidRemote, {Reason, ResponseData}} when RidRemote == Rid -> 
					?DEBUG("ems_dispatcher received msg from ~p: ~p.", [{Module, Node}, {Code, RidRemote, {Reason, ResponseData}}]),
					AllowResultCache = Reason =:= ok andalso Type =:= "GET",
					Request2 = Request#request{service = Service,
											   params_url = ParamsMap,
											   querystring_map = QuerystringMap,
											   user = User,
											   content_type = ContentType,
											   code = Code,
											   reason = Reason,
											   response_header = #{<<"ems-node">> => NodeBin,
																   <<"ems-catalog">> => ServiceName,
																   <<"ems-owner">> => ServiceOwner,
																   <<"content-type">> => ContentType,
																   <<"ems-result-cache">> => case AllowResultCache of true -> integer_to_binary(ResultCache); _ -> <<"0">> end},
											   response_data = ResponseData,
											   latency = ems_util:get_milliseconds() - T1},
					case dispatch_middleware_function(Request2, onrequest) of
						{ok, Request3} ->
							case AllowResultCache of
								true -> ems_dispatcher_cache:add(ReqHash, T1, Request3, ResultCache);
								false -> ems_dispatcher_cache:invalidate()
							end,
							{Reason, request, Request3};
						{error, Reason} = Error ->
							{error, request, Request#request{code = 500,
															 reason = Reason,
															 response_header = #{<<"ems-node">> => NodeBin,
																				 <<"ems-catalog">> => ServiceName,
																				 <<"ems-owner">> => ServiceOwner},
															 response_data = ems_schema:to_json(Error),
															 latency = ems_util:get_milliseconds() - T1}}
					end;
				Msg -> 
					ems_logger:error("ems_dispatcher received invalid message ~p.", [Msg]), 
					{error, request, Request#request{code = 500,
													 reason = einvalid_rec_message,
													 response_header = #{<<"ems-node">> => NodeBin,
																		 <<"ems-catalog">> => ServiceName,
																		 <<"ems-owner">> => ServiceOwner},
													 response_data = ems_schema:to_json({error, einvalid_rec_message}),
													 latency = ems_util:get_milliseconds() - T1}}
				after Timeout ->
					?DEBUG("ems_dispatcher received a timeout while waiting ~pms for the result of a service from ~p.", [Timeout, {Module, Node}]),
					{error, request, Request#request{code = 503,
													 reason = etimeout_service,
													 response_header = #{<<"ems-node">> => NodeBin,
																		 <<"ems-catalog">> => ServiceName,
																		 <<"ems-owner">> => ServiceOwner},
													 response_data = ems_schema:to_json({error, etimeout_service}),
													 latency = ems_util:get_milliseconds() - T1}}
			end;
		Error ->  
			ems_logger:warn("ems_dispatcher could not get a node for request ~p.", [Url]),
			Error
	end.


get_work_node('', _, _, _, _) -> {ok, node()};
get_work_node([], _, _, _, _) -> {error, eunavailable_service};
get_work_node([_|T], HostList, HostNames, ModuleName, Tentativa) -> 
	QtdHosts = length(HostList),
	case QtdHosts == 1 of
		true -> Node = hd(HostList);
		false ->
			% ========= faz round robin ===========
			%% Localiza a entrada do módulo na tabela hash
			case ets:lookup(ctrl_node_dispatch, ModuleName) of
				[] -> 
					% não encontrou, vamos selecionar o primeiro host mas o próximo será o segundo
					Index = 2,
					Node = hd(HostList);
				[{_, Idx}] -> 
					% Se o idx não existe pega o primeiro e o próximo será o segundo
					case Idx > QtdHosts of
						true -> 
							Index = 2,
							Node = hd(HostList);
						false -> 
							Node = lists:nth(Idx, HostList),
							Index = Idx + 1
					end
			end,
			% Inserimos na tabela hash os dados de controle
			ets:insert(ctrl_node_dispatch, {ModuleName, Index})
	end,

	
	% Este node está vivo? Temos que rotear para um node existente
	Ping = net_adm:ping(Node),
	case Ping of
		pong -> {ok, Node};
		pang -> 
			?DEBUG("ems_dispatcher pang ~p.", [Node]),
			get_work_node(T, HostList, HostNames, ModuleName, Tentativa)
	end.
		

-spec node_binary() -> binary().
node_binary() -> erlang:atom_to_binary(node(), utf8).

-spec dispatch_middleware_function(#request{}, atom()) -> {ok, #request{}} | {error, atom()}.
dispatch_middleware_function(Req = #request{service = #service{middleware = undefined}}, _) -> {ok, Req};
dispatch_middleware_function(Req = #request{service = #service{middleware = Middleware}}, Function) ->
	try
		case code:ensure_loaded(Middleware) of
			{module, _} ->
				case erlang:function_exported(Middleware, Function, 1) of
					true -> apply(Middleware, Function, [Req]);
					false -> {ok, Req}
				end;
			_Error -> 
				ems_logger:error("ems_dispatcher does not load invalid middleware ~p.", [Middleware]),
				{error, einvalid_middleware}
		end
	catch 
		_Exception:Error2 -> 
			ems_logger:error("ems_dispatcher invoque middleware ~p:~p with error. Reason: ~p.", [Middleware, Function, Error2]),
			{error, emiddleware_exception}
	end.
	
