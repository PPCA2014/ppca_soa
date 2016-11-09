%%********************************************************************
%% @title Module ems_odbc_pool_worker
%% @version 1.0.0
%% @doc Module ems_webservice
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_odbc_pool_worker).

-behavior(gen_server). 

-include("../..//include/ems_config.hrl").
-include("../../include/ems_schema.hrl").

%% Server API
-export([start/1, start_link/1, stop/0, get_datasource/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  State
-record(state, {datasource}). 


%%====================================================================
%% Server API
%%====================================================================

start(_) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
get_datasource(Worker) -> gen_server:call(Worker, get_datasource).

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(Datasource) -> 
	case do_connect(Datasource) of
		{ok, Datasource2} -> {ok, #state{datasource = Datasource2}};
		_Error -> ignore
	end.
	
    
handle_cast(shutdown, State) ->
	do_disconnect(State),
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.
    
handle_call({param_query, Sql, Params, Timeout}, _From, State) ->
	Reply = do_param_query(Sql, Params, Timeout, State),
	{reply, Reply, State};

handle_call(get_datasource, _From, State) ->
	{reply, State#state.datasource, State}.

handle_info(State) ->
   {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid2, _Reason}, State) ->
	io:format("MORREU!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n"),
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

    
do_connect(Datasource = #service_datasource{connection = Connection, 
											timeout = _Timeout}) -> 

	try
		case odbc:connect(Connection, []) of
			{ok, ConnRef}	-> 
				Datasource2 = Datasource#service_datasource{owner = self(), conn_ref = ConnRef},
				erlang:monitor(process, ConnRef),
				io:format("conexao criada\n"),
				{ok, Datasource2};
			{error, Reason} -> {error, Reason}
		end
	catch 
		_Exception:_Reason -> {error, eodbc_connection_fail}
	end.

do_disconnect(#state{datasource = #service_datasource{conn_ref = ConnRef}}) -> 
	io:format("disconnect...\n"), 
	odbc:disconnect(ConnRef),
	io:format("disconnect ok\n").

do_param_query(Sql, Params, Timeout, #state{datasource = #service_datasource{conn_ref = ConnRef}}) ->
	io:format("do para query\n"),
	try
		case odbc:param_query(ConnRef, Sql, Params, Timeout) of
			{error, Reason} -> {error, Reason};
			Result -> Result
		end
	catch
		_Exception:Reason2 -> 
			io:format("falha ao acessar banco ~p\n", [Reason2]),
			{error, eodbc_connection_fail}
	end.

    
						