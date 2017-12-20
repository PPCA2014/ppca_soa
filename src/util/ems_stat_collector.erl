%%********************************************************************
%% @title Module ems_stat_collector
%% @version 1.0.0
%% @doc Module responsible for the system monitor
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_stat_collector).

-behavior(gen_server). 

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).

%% Client API
-export([]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, collect/0]).

-define(SERVER, ?MODULE).


-record(state, {timeout}). 


%%====================================================================
%% Server API
%%====================================================================

start(Service) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Service, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Client API
%%====================================================================
 
collect() ->
	gen_server:cast(?SERVER, collect).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{start_timeout = Timeout}) ->
    State = #state{timeout = Timeout},
    {ok, State, Timeout}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(collect, State = #state{timeout = Timeout}) ->
	NewState = collect_stats(State),
	{noreply, NewState, Timeout};

handle_cast(_, State) ->
	{noreply, State}.

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_info(timeout, State = #state{timeout = Timeout}) ->
   NewState = collect_stats(State),
   {noreply, NewState, Timeout}.

terminate(_, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Internal functions
%%====================================================================

collect_stats(State) ->
	ems_logger:info("ems_stat_collector collect system statistics."),
	Timestamp = calendar:local_time(),
	{ok, Counters} = ems_db:all(counter),
	mnesia:clear_table(counter),
	persist_stats(Counters, Timestamp),
	State.
	
persist_stats([], _) -> ok;
persist_stats([{counter, StatName, StatValue}|T], Timestamp) ->
	Record = #stat_counter_hist{id = ems_db:sequence(stat_counter_hist),
						stat_name = StatName, 
					    stat_value = StatValue,
						stat_timestamp = Timestamp},
	mnesia:dirty_write(stat_counter_hist, Record),
	persist_stats(T, Timestamp).
	
	

