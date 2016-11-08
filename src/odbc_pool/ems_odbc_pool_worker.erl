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
 
init(Datasource = #service_datasource{connection = Connection, 
								      timeout = _Timeout}) -> 
	case odbc:connect(Connection, []) of
		{ok, Conn}	-> 
			Datasource2 = Datasource#service_datasource{conn_ref = Conn, 
														owner = self()},
			{ok, #state{datasource = Datasource2}};
		{error, Reason} -> 
			{stop, Reason}
	end.
		
    
handle_cast(shutdown, State = #state{datasource = #service_datasource{conn_ref = Conn}}) ->
	odbc:disconnect(Conn),
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.
    
handle_call({param_query, Datasource, Sql, Params, Timeout}, _From, State) ->
	Reply = do_param_query(Datasource, Sql, Params, Timeout),
	{reply, Reply, State};

handle_call(get_datasource, _From, State) ->
	{reply, State#state.datasource, State}.

handle_info(State) ->
   {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid2, Reason}, State) ->
	{noreply, State};

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

do_param_query(#service_datasource{conn_ref = Conn}, Sql, Params, Timeout) ->
	try
		case odbc:param_query(Conn, Sql, Params, Timeout) of
			{error, Reason} -> {error, Reason};
			Result -> Result
		end
	catch
		_Exception:Reason2 -> {error, Reason2}
	end.

    
						
