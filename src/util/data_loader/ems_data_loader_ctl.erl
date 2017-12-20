%%******************************************************************** 
%% @title Module ems_data_loader_ctl  
%% @version 1.0.0 %%
%% @doc Module responsible for load records from database
%% @author Everton de Vargas Agilar  <evertonagilar@gmail.com> 
%% @copyright ErlangMS Team 
%%********************************************************************

-module(ems_data_loader_ctl).

-behavior(gen_server). 

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, 
		 permission_to_execute/2, notify_finish_work/2, rand_next_interval/0, is_loading/0]).

% estado do servidor
-record(state, {what,	
				process_name,
				timestamp,
				prior
			}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(_Service) -> 
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
permission_to_execute(What, ProcessName) -> gen_server:call(?MODULE, {permission_to_execute, What, ProcessName}).
	
notify_finish_work(What, ProcessName) -> gen_server:cast(?MODULE, {notify_finish_work, What, ProcessName}).

-spec rand_next_interval() -> non_neg_integer().
rand_next_interval() -> 1000 + rand:uniform(3000).

% Retorna true/false se os loaders estão sendo executados
-spec is_loading() -> boolean().
is_loading() -> 
	case gen_server:call(?MODULE, is_loading) of
		true -> true;
		false ->
			ems_util:sleep(1000),
			gen_server:call(?MODULE, is_loading)
	end.

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_) ->
	{ok, #state{}}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({notify_finish_work, What, ProcessName}, State) ->
	State2 = do_notify_finish_work(What, ProcessName, State),
	{noreply, State2};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call({permission_to_execute, What, ProcessName}, _From, State) ->
	case do_permission_to_execute(What, ProcessName, State) of
		{ok, State2} -> {reply, true, State2};
		{error, State2} -> {reply, false, State2}
	end;

handle_call(is_loading, _From, State) ->
	{reply, State#state.process_name =/= undefined, State};

handle_call(Msg, _From, State) ->
	{reply, Msg, State}.
		
terminate(Reason, #service{name = Name}) ->
    ems_logger:warn("~s was terminated. Reason: ~p.", [Name, Reason]),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	

%%====================================================================
%% Internal functions
%%====================================================================

do_permission_to_execute(What2, ProcessName2, State = #state{process_name = ProcessName, 
															 timestamp = Timestamp}) ->
	Timestamp2 = ems_util:get_milliseconds(),
	case ProcessName2 == ProcessName of
		true -> 
			{ok, #state{what = What2, 
						process_name = ProcessName2,
						timestamp = Timestamp2,
						prior = State}
			};
		false ->
			case Timestamp of
				undefined -> 
					{ok, #state{what = What2, 
								process_name = ProcessName2,
								timestamp = Timestamp2}
					};
				Timestamp when Timestamp2 - Timestamp > 120000 -> 
					MetricName = binary_to_atom(iolist_to_binary([<<"ems_data_loader_ctl_lock_expired_">>, ProcessName]), utf8),
					ems_db:inc_counter(MetricName),
					{ok, #state{what = What2, 
								process_name = ProcessName2,
								timestamp = Timestamp2}
					};
				_ -> 
					MetricName = binary_to_atom(iolist_to_binary([<<"ems_data_loader_ctl_lock_">>, ProcessName2]), utf8),
					ems_db:inc_counter(MetricName),
					MetricName2 = binary_to_atom(iolist_to_binary([<<"ems_data_loader_ctl_wait_">>, ProcessName]), utf8),
					ems_db:inc_counter(MetricName2),
					{error, State}
			end
	end.


do_notify_finish_work(What2, ProcessName2, State = #state{what = What,
														  process_name = ProcessName,
														  prior = Prior}) ->
	case ProcessName2 == ProcessName andalso What2 == What of
		true -> 
			case Prior == undefined of
				true ->	#state{};
				false -> Prior
			end;
		false -> State
	end.


