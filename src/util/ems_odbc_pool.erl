%%********************************************************************
%% @title Module ems_odbc_pool
%% @version 1.0.0
%% @doc Module ems_webservice
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_odbc_pool).

-behavior(gen_server). 

-include("../..//include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%% Server API
-export([start/1, start_link/1, stop/0, get_connection/1, sqconnection/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do service. 
-record(state, {}). 


%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
get_connection(Datasource = #service_datasource{connection = Connection, 
												timeout = Timeout}) ->
	io:format("aqui1\n"),
	Key = erlang:pid_to_list(self()),
	case erlang:get(Key) of
		undefined ->
			io:format("aqui2\n"),
			ConnectFun = fun() ->
				io:format("create connect... ~p\n", [Connection]),
				case odbc:connect(Connection, []) of
					{ok, Conn}	-> 
						io:format("ok...\n"),
						{ok, Datasource#service_datasource{conn_ref = Conn, 
														  pid_module = self()}};
					{error, Reason} -> 
						io:format("erro ~p...\n", [Reason]),
						{error, Reason}
				end
			end,
			ReleaseFun = fun(_) ->
				io:format("release.\n"),
				ok
			end,
			io:format("aqui3\n"),
			Result = ems_cache:get(ems_odbc_pool_cache, 
						  infinity, 
						  Key, 
						  ConnectFun,
						  ReleaseFun),
			io:format("result is ~p\n", [Result]),
			Result;
		DatasourceCache -> 
			io:format("aqui4\n"),
			{ok, DatasourceCache}
	end.
 
sqconnection() ->
	#service_datasource{type = sqlite, connection = ?DATABASE_SQLITE_STRING_CONNECTION}.
	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    io:format("criado cache ems_odbc_pool_cache\n"),
    ems_cache:new(ems_odbc_pool_cache),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
	{noreply, State}.
    
handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(Msg, State) ->
   io:format("msg... ~p\n", [Msg]),
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
										 
