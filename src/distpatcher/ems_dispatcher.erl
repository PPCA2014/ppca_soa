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

dispatch_request(Request = #request{req_hash = ReqHash, 
								  url = Url,
								  ip = Ip,
								  ip_bin = IpBin,
								  content_type = ContentTypeReq,
								  type = Method,
								  t1 = T1}) -> 
	?DEBUG("ems_dispatcher lookup request ~p.", [Request]),
	RequestLookup = case Method of
						"OPTIONS" -> Request#request{type = "GET"};
						"HEAD" -> Request#request{type = "GET"};
						_ -> Request
				  end,
	case ems_catalog:lookup(RequestLookup) of
		{Service = #service{content_type = ContentTypeService,
							tcp_allowed_address_t = AllowedAddress}, 
		 ParamsMap, 
		 QuerystringMap} -> 
			case ems_util:allow_ip_address(Ip, AllowedAddress) of
				true ->
					ContentType = case ContentTypeReq of
									  undefined -> ContentTypeService;
									  _ -> ContentTypeReq
								  end,
					case ems_auth_user:authenticate(Service, Request) of
						{ok, Client, User, AccessToken, Scope} -> 
							Request2 = Request#request{service = Service,
														params_url = ParamsMap,
														querystring_map = QuerystringMap,
														client = Client,
														user = User,
														scope = Scope,
														access_token = AccessToken,
														content_type = ContentType},
							case Method of
								"OPTIONS" -> 
										{ok, request, Request2#request{code = 200, 
																	   response_data = ems_catalog:get_metadata_json(Service),
																	   latency = ems_util:get_milliseconds() - T1}
										};
								"HEAD" -> 
										{ok, request, Request2#request{code = 200, 
																	   latency = ems_util:get_milliseconds() - T1}
										};
								"GET" ->
									case ems_dispatcher_cache:lookup(ReqHash, T1) of
										{true, RequestCache} -> 
											?DEBUG("ems_dispatcher lookup request in cache. ReqHash: ~p.", [ReqHash]),
											{ok, request, Request2#request{result_cache = true,
																		   code = RequestCache#request.code,
																		   reason = RequestCache#request.reason,
																		   response_data = RequestCache#request.response_data,
																		   response_header = RequestCache#request.response_header,
																		   result_cache_rid = RequestCache#request.rid,
																		   latency = RequestCache#request.latency,
																		   filename = RequestCache#request.filename}};
										false -> dispatch_service_work(Request2, Service)
									end;
								_ ->
									dispatch_service_work(Request2, Service)
							end;
						{error, Reason} = Error -> 
							Request2 = Request#request{service = Service,
													   params_url = ParamsMap,
													   querystring_map = QuerystringMap,
													   content_type = ContentType},
							case Method of
								"OPTIONS" -> 
										{ok, request, Request2#request{code = 200, 
																	  response_data = ems_catalog:get_metadata_json(Service),
																	  latency = ems_util:get_milliseconds() - T1}
										};
								"HEAD" -> 
										{ok, request, Request2#request{code = 200, 
																	  latency = ems_util:get_milliseconds() - T1}
										};
								 _ -> 
									{error, request, Request2#request{code = 400, 
					 											      reason = Reason, 
																	  response_data = ems_schema:to_json(Error), 
																	  latency = ems_util:get_milliseconds() - T1}
									}
							end
					end;
				false -> 
					ems_logger:warn("ems_dispatcher does not grant access to IP ~p. Reason: IP denied.", [IpBin]),
					{error, host_denied}
			end;
		{error, Reason} = Error2 -> 
			if 
				Method =:= "OPTIONS" orelse Method =:= "HEAD" ->
						{ok, request, Request#request{code = 200, 
													  latency = ems_util:get_milliseconds() - T1}
						};
				true ->
					ems_logger:warn("ems_dispatcher service request ~p not found. Reason: ~p.", [Url, Reason]),
					Error2
			end
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
										  client = Client,
										  user = User,
										  scope = Scope,
										  content_type = ContentType,  
										  params_url = ParamsMap,
										  querystring_map = QuerystringMap},
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
			case erlang:is_tuple(Client) of
				false -> 
					ClientJson = <<"{id=0, codigo=0, name=\"public\", description=\"\", secret=null, redirect_uri=null, active=1.0, scope=null, ctrl_insert=null, ctrl_update=null}">>;
				_ -> 
					ClientJson = ems_schema:to_json(Client)
			end,
			case erlang:is_tuple(User) of
				false -> 
					UserJson = <<"{id=0, codigo=0, name=\"public\", login=null, password=null, cpf=null, active=1.0, ctrl_insert=null, ctrl_update=null}">>;
				_ -> 
					UserJson = ems_schema:to_json(User)
			end,
			Msg = {{Rid, Url, Type, ParamsMap, QuerystringMap, Payload, ContentType, ModuleName, FunctionName, ClientJson, UserJson, undefined, Scope, undefined, undefined}, self()},
			{Module, Node} ! Msg,
			NodeBin = erlang:atom_to_binary(Node, utf8),
			ems_logger:info("ems_dispatcher send msg to ~p with timeout ~pms.", [{Module, Node}, Timeout]),
			receive 
				{Code, RidRemote, {Reason, ResponseData = <<H1:25/binary, _H2:2/binary, ResponseDataBin/binary>> }} when RidRemote == Rid  -> 
					case H1 =:= <<172,237,0,5,117,114,0,2,91,66,172,243,23,248,6,8,84,224,2,0,0,120,112,0,0>> of
						true ->
							AllowResultCache = Reason =:= ok andalso Type =:= "GET",
							Request2 = Request#request{service = Service,
													   params_url = ParamsMap,
													   querystring_map = QuerystringMap,
													   content_type = ContentType,
													   code = Code,
													   reason = Reason,
													   response_header = #{<<"ems-node">> => NodeBin,
																		   <<"ems-catalog">> => ServiceName,
																		   <<"ems-owner">> => ServiceOwner,
																		   <<"content-type">> => ContentType,
																		   <<"ems-result-cache">> => case AllowResultCache of true -> integer_to_binary(ResultCache); _ -> <<"0">> end},
													   response_data = ResponseDataBin,
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
						_ ->
							AllowResultCache = Reason =:= ok andalso Type =:= "GET",
							Request2 = Request#request{service = Service,
													   params_url = ParamsMap,
													   querystring_map = QuerystringMap,
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
							end
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
	
