%%********************************************************************
%% @title Module ems_dns_server
%% @version 1.0.0
%% @doc dns server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_dns_server).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

-include_lib("stdlib/include/ms_transform.hrl"). 
-include_lib("kernel/src/inet_dns.hrl").


%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-record(state, {sock, table}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Service = #service{name = Name}) -> 
 	ServerName = list_to_atom(binary_to_list(Name)),
    gen_server:start_link({local, ServerName}, ?MODULE, Service, []).

 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 


 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{tcp_listen_address_t = _ListenAddress_t,
			  tcp_port = Port}) ->
    Tid = ets:new(resolve_table, [bag, public, named_table, {keypos, #dns_rr.domain}]),
    case gen_udp:open(Port, [binary, {active, true}]) of
		{ok, Sock} -> {ok, #state{sock = Sock, table = Tid}};
		{error, Reason} -> {stop, Reason}
    end.		
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({udp, Sock, FromIP, FromPort, Data}, #state{sock = Sock, table = Tid} = State) ->
    ems_logger:info("ems_dns_server got packet from ~p:~p.", [FromIP, FromPort]),
    handle_dns_packet(Data, fun(D, normal) ->
									gen_udp:send(Sock, FromIP, FromPort, D),
									save_dns_result_to_table(Tid, D);
								(D, cached) ->
									gen_udp:send(Sock, FromIP, FromPort, D)
							end, Tid),
    {noreply, State};
handle_info(_Info, State) ->
    ems_logger:warn("ems_dns_server unhandled message: ~p.", [_Info]),
    {noreply, State}.

handle_info(State) ->
   {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{sock=Sock}) ->
    gen_udp:close(Sock),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_dns_packet(Data, SendFunc, Tid) ->
    {ok, Packet} = inet_dns:decode(Data),
    #dns_rec{header=Header, qdlist=Questions} = Packet,
    handle_dns_header(Header),
    case Header#dns_header.qr of
		true ->
			%% this is response
			handle_dns_header(Header);
		false ->
			%% this is a request
			case query_table_by_dns_query(Tid, Questions) of
				[] ->
					?DEBUG("ems_dns_server query not found."),
					handle_dns_request(Packet, SendFunc);
				Cached ->
					SendFunc(inet_dns:encode(dns_rec_fill_answer(Packet, Cached)), cached)
			end
    end,

    handle_dns_questions(Questions),
    %% io:format("got dns packet ~p~n", [Packet]),
    ok.

handle_dns_request(#dns_rec{header=#dns_header{qr=false,opcode='query'}, qdlist=_Queries} = Packet,
					SendFunc) ->
    QuerySendFunc =
	fun() ->
		Qdlist = lists:filter(fun(#dns_query{type=aaaa}) -> false;
													(_)  -> true
							  end,
				       Packet#dns_rec.qdlist),
		case Qdlist of
		    [] -> SendFunc(inet_dns:encode(dns_rec_requst_to_response(Packet)), normal);
		    _  -> query_google_and_send_response(Packet, SendFunc)
		end
	end,
    spawn(QuerySendFunc).

dns_rec_requst_to_response(Packet) ->
    _OldHeader = Packet#dns_rec.header,
    Packet#dns_rec{header=_OldHeader#dns_header{qr=true}}.

dns_rec_fill_answer(Packet, Answers) when is_list(Answers) ->
    Packet#dns_rec{anlist=Answers}.

query_google_and_send_response(Packet, SendFunc) ->
    {ok, S} = gen_udp:open(0, [binary]),
    gen_udp:send(S, "8.8.8.8", 53, inet_dns:encode(Packet)),
    receive
		{udp, S, _Ip, 53, Data} -> SendFunc(Data, normal)
	after 3000 ->
			ems_logger:info("ems_dns_server query timeout.")
	end,
    gen_udp:close(S).

		   
handle_dns_header(_DnsHeader) ->  ok.
	    

handle_dns_questions([#dns_query{domain=Domain,type=Type,class=Class}|Rest]) ->
    ems_logger:info("ems_dns_server query type:~p domain:~p class:~p.", [Type, Domain, Class]),
    handle_dns_questions(Rest);
handle_dns_questions([]) ->
    ok.

    
save_dns_result_to_table(T, Data) when is_binary(Data) ->
    {ok, Packet} = inet_dns:decode(Data),
    save_dns_result_to_table(T, Packet);
save_dns_result_to_table(T, Packet) when is_record(Packet, dns_rec) ->
    case Packet#dns_rec.anlist of
		[] ->
			ok;
		Records ->
			Records1 = lists:map(fun(R) -> dns_rr_set_ttl(R, 1000) end,
					 Records),
			ets:insert(T, Records1)
    end.

query_table_by_dns_query(T, Query) when is_record(Query, dns_query) ->
    #dns_query{domain=Domain} = Query,
    Result = table_query(T, Domain),
    fill_cname_query(T, Result);
query_table_by_dns_query(T, [Query|Rest]=Queries) when is_list(Queries),
						       is_record(Query, dns_query) ->
    lists:flatten([query_table_by_dns_query(T, Query),
		   query_table_by_dns_query(T, Rest)]);
query_table_by_dns_query(_, []) ->
    [].


fill_cname_query(Tid, [R|Rest]) ->
    #dns_rr{type=Type, data=Data} = R,
    case Type of
		cname ->
			[R|fill_cname_query(Tid, table_query(Tid, Data))] ++
			fill_cname_query(Tid, Rest);
		_Other ->
			[R|fill_cname_query(Tid, Rest)]
    end;
fill_cname_query(_, []) ->
    [].

%% make ttl longer and store to a `bag`
dns_rr_set_ttl(Query, TTL) when is_record(Query, dns_rr), is_integer(TTL) ->
    Query#dns_rr{ttl=TTL}.

table_query(Tid, Domain) ->
    %% fun2ms is bad here
    A = ets:select(Tid, [{{dns_rr,Domain,'_','_','_','_','_','_','_','_'},[],['$_']}]),
    A.
