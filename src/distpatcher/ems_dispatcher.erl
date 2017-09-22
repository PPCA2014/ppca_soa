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
	ems_cache:new(ets_result_cache_get),
	ets:new(ctrl_node_dispatch, [set, named_table, public]).


check_result_cache(ReqHash, Timestamp2) ->
	case ets:lookup(ets_result_cache_get, ReqHash) of
		[] -> false; 
		[{_, {Timestamp, _, ResultCache}}] when Timestamp2 - Timestamp > ResultCache ->	false;
		[{_, {_, Request, _}}] -> {true, Request}
	end.

dispatch_request(Request = #request{req_hash = ReqHash, 
					 			    url = Url,
								    ip = Ip,
								    ip_bin = IpBin,
								    content_type = ContentTypeReq,
								    type = Type,
								    t1 = T1}) -> 
	?DEBUG("ems_dispatcher lookup request ~p.", [Request]),
	RequestLookup = case Type of
						"OPTIONS" -> Request#request{type = "GET"};
						"HEAD" -> Request#request{type = "GET"};
						_ -> Request
				  end,
	case ems_catalog:lookup(RequestLookup) of
		{Service = #service{content_type = ContentTypeService,
							tcp_allowed_address_t = AllowedAddress,
							result_cache = ResultCache}, 
		 ParamsMap, 
		 QuerystringMap} -> 
			case ems_util:allow_ip_address(Ip, AllowedAddress) of
				true ->
					case ems_auth_user:authenticate(Service, Request) of
						{ok, Client, User, AccessToken, Scope} -> 

							Request2 = Request#request{service = Service,
														params_url = ParamsMap,
														querystring_map = QuerystringMap,
														client = Client,
														user = User,
														scope = Scope,
														access_token = AccessToken,
														content_type = 	case ContentTypeService of
																			  undefined -> ContentTypeReq;
																			  _ -> ContentTypeService
																		end},
							case Type of
								"OPTIONS" -> 
										{ok, request, Request2#request{code = 200, 
																	   content_type = ?CONTENT_TYPE_JSON,
																	   response_data = ems_catalog:get_metadata_json(Service),
																	   response_header = #{<<"ems-node">> => ems_util:node_binary()},
																	   latency = ems_util:get_milliseconds() - T1}
										};
								"HEAD" -> 
										{ok, request, Request2#request{code = 200, 
																	   response_header = #{<<"ems-node">> => ems_util:node_binary()},
																	   latency = ems_util:get_milliseconds() - T1}
										};
								"GET" ->
									case ResultCache > 0 of
										true ->
											case check_result_cache(ReqHash, T1) of
												{true, RequestCache} -> 
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
										false -> dispatch_service_work(Request2, Service)
									end;
								_ ->
									dispatch_service_work(Request2, Service)
							end;
						{error, Reason} = Error -> 
							Request2 = Request#request{service = Service,
													   params_url = ParamsMap,
													   querystring_map = QuerystringMap},
							case Type of
								"OPTIONS" -> 
										{ok, request, Request2#request{code = 200, 
																	   content_type = ?CONTENT_TYPE_JSON,
																	   response_data = ems_catalog:get_metadata_json(Service),
																	   response_header = #{<<"ems-node">> => ems_util:node_binary()},
																	   latency = ems_util:get_milliseconds() - T1}
										};
								"HEAD" -> 
										{ok, request, Request2#request{code = 200, 
																	   response_header = #{<<"ems-node">> => ems_util:node_binary()},
																	   latency = ems_util:get_milliseconds() - T1}
										};
								 _ -> 
									{error, request, Request2#request{code = 400, 
																	  content_type = ?CONTENT_TYPE_JSON,
					 											      reason = Reason, 
																	  response_data = ems_schema:to_json(Error), 
																	  response_header = #{<<"ems-node">> => ems_util:node_binary()},
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
				Type =:= "OPTIONS" orelse Type =:= "HEAD" ->
						{error, request, Request#request{code = 200, 
													     reason = Reason, 
													     response_header = #{<<"ems-node">> => ems_util:node_binary()},
													     latency = ems_util:get_milliseconds() - T1}
						};
				true ->
					ems_logger:warn("ems_dispatcher service ~p not found. Reason: ~p.", [Url, Reason]),
					Error2
			end
	end.


dispatch_service_work(Request = #request{t1 = T1},
					 _Service = #service{name = ServiceName,
										 owner = ServiceOwner,
										 host = '',
										 module_name = ModuleName,
										 module = Module,
										 function = Function}) ->
	ems_logger:info("ems_dispatcher send local msg to ~s.", [ModuleName]),
	{Reason, Request3 = #request{response_header = ResponseHeader}} = apply(Module, Function, [Request]),
	Request4 = Request3#request{response_header = ResponseHeader#{<<"ems-node">> => ems_util:node_binary(),
																  <<"ems-catalog">> => ServiceName,
																  <<"ems-owner">> => ServiceOwner},
								latency = ems_util:get_milliseconds() - T1},
	dispatch_middleware_function(Request4, Reason);
dispatch_service_work(Request = #request{rid = Rid,
										  type = Type,
										  url = Url,
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
										 timeout = Timeout}) ->
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
			Msg = {{Rid, Url, Type, ParamsMap, QuerystringMap, Payload, ContentType, ModuleName, FunctionName, 
					ClientJson, UserJson, ems_catalog:get_metadata_json(Service), Scope, 
					undefined, undefined}, self()
				  },
			{Module, Node} ! Msg,
			NodeBin = erlang:atom_to_binary(Node, utf8),
			ems_logger:info("ems_dispatcher send msg to ~p with timeout ~pms.", [{Module, Node}, Timeout]),
			receive 
				{Code, RidRemote, {Reason, ResponseDataReceived}} when RidRemote == Rid  -> 
					case byte_size(ResponseDataReceived) >= 27 of
						true ->
							case ResponseDataReceived of
								% Os dados recebidos do Java pode ser um array de bytes que possui um "header especial" que precisa ser removido do verdadeiro conteúdo
								<<HeaderJavaSerializable:25/binary, _H2:2/binary, DataBin/binary>> -> 
									case HeaderJavaSerializable =:= <<172,237,0,5,117,114,0,2,91,66,172,243,23,248,6,8,84,224,2,0,0,120,112,0,0>> of
										true -> ResponseData = DataBin;
										false -> ResponseData = ResponseDataReceived
									end;
								_ -> ResponseData = ResponseDataReceived
							end;
						false -> ResponseData = ResponseDataReceived
					end,
					Request2 = Request#request{service = Service,
											   params_url = ParamsMap,
											   querystring_map = QuerystringMap,
											   code = Code,
											   reason = Reason,
											   response_header = #{<<"ems-node">> => NodeBin,
																   <<"ems-catalog">> => ServiceName,
																   <<"ems-owner">> => ServiceOwner},
											   response_data = ResponseData,
											   latency = ems_util:get_milliseconds() - T1},
					dispatch_middleware_function(Request2, Reason);
				Msg -> 
					ems_logger:error("ems_dispatcher received invalid message ~p.", [Msg]), 
					{error, request, Request#request{code = 500,
													 content_type = ?CONTENT_TYPE_JSON,
													 service = Service,
													 params_url = ParamsMap,
													 querystring_map = QuerystringMap,
													 reason = einvalid_rec_message,
													 response_header = #{<<"ems-node">> => NodeBin,
																		 <<"ems-catalog">> => ServiceName,
																		 <<"ems-owner">> => ServiceOwner},
													 response_data = ems_schema:to_json({error, einvalid_rec_message}),
													 latency = ems_util:get_milliseconds() - T1}}
				after Timeout + 1000 ->
					?DEBUG("ems_dispatcher received a timeout while waiting ~pms for the result of a service from ~p.", [Timeout, {Module, Node}]),
					{error, request, Request#request{code = 503,
													 content_type = ?CONTENT_TYPE_JSON,
													 service = Service,
													 params_url = ParamsMap,
													 querystring_map = QuerystringMap,
													 reason = etimeout_service,
													 response_header = #{<<"ems-node">> => NodeBin,
																		 <<"ems-catalog">> => ServiceName,
																		 <<"ems-owner">> => ServiceOwner},
													 response_data = ems_schema:to_json({error, etimeout_service}),
													 latency = ems_util:get_milliseconds() - T1}}
			end;
		Error ->  Error
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
		


-spec dispatch_middleware_function(#request{}, atom()) -> {ok, request, #request{}} | {error, request, #request{}}.
dispatch_middleware_function(Request = #request{req_hash = ReqHash,
												t1 = T1,
												type = Type,
												service = Service = #service{middleware = Middleware,
																			 result_cache = ResultCache}}, 
							 Reason) ->
	try
		case Middleware of 
			undefined -> Result = {ok, Request};
			_ ->
				case code:ensure_loaded(Middleware) of
					{module, _} ->
						Result = case erlang:function_exported(Middleware, onrequest, 1) of
									true -> apply(Middleware, onrequest, [Request]);
									false -> {ok, Request}
								 end;
					_ ->  Result = {error, einvalid_middleware}
				end
		end,
		case Result of
			{ok, Request2 = #request{response_header = ResponseHeader}} ->
				case Reason =:= ok andalso Type =:= "GET" of
					true -> 
						case ResultCache > 0 of
							true ->
								ems_cache:add(ets_result_cache_get, ResultCache, ReqHash, {T1, Request2, ResultCache}),
								{Reason, request, Request2#request{response_header = ResponseHeader#{<<"ems-result-cache">> => integer_to_binary(ResultCache)}}};
							_ -> 
								{Reason, request, Request2#request{response_header = ResponseHeader#{<<"ems-result-cache">> => <<"0"/utf8>>}}}
						end;
					false ->
						ems_cache:flush(ets_result_cache_get),
						{Reason, request, Request2}
				end;
			{error, Reason2} = Error ->
				{error, request, Request#request{code = 500,
												 content_type = ?CONTENT_TYPE_JSON,
											     reason = Reason2,
												 service = Service,
												 response_data = ems_schema:to_json(Error)}}
		end
	catch 
		_Exception:Error2 -> 
			ems_logger:error("ems_dispatcher middleware ~p:~p exception. Reason: ~p.", [Middleware, onrequest, Error2]),
			{error, request, Request#request{code = 500,
											 content_type = ?CONTENT_TYPE_JSON,
											 reason = emiddleware_exception,
											 service = Service,
											 response_data = ems_schema:to_json(Error2)}}
	end.
