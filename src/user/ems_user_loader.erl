%%********************************************************************
%% @title Module ems_user_loader
%% @version 1.0.0
%% @doc Main module HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_user_loader).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {datasource,
				mode}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Service) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, Service, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 


 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{datasource = Datasource}) ->
	State = #state{datasource = Datasource, mode = load_users},
	{ok, State, 4000}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(Msg, _From, State) ->
	{reply, Msg, State}.

handle_info(State) ->
   {noreply, State, 1000}.

handle_info(timeout, State = #state{mode = update_users}) ->
   update_users(),
   {noreply, State, 2000};

handle_info(timeout, State = #state{datasource = Datasource,
									mode = load_users}) ->
   load_users(Datasource),
   State2 = State#state{mode = update_users},
   {noreply, State2, 15000}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

update_users() -> 
	
	ok.

load_users(Datasource = #service_datasource{connection = Connection}) -> 
	try
		case ems_odbc_pool:get_connection(Datasource) of
			{ok, Datasource2} -> 
				case ems_odbc_pool:param_query(Datasource2, 
														sql_load_users(), 
														[], 
														?MAX_TIME_ODBC_QUERY) of
					{_,_,[]} -> ok;
					{_, _, Records} ->
						F = fun() ->
							Count = insert_users(Records, 0),
							ems_logger:info("Load ~p users from datasource: ~p.", [Count, Connection])
						end,
						mnesia:activity(transaction, F);
					{error, Reason} -> 
						ems_logger:error("Fail load users from datasource: ~p. Error: ~p.", [Connection, Reason])
				end,
				ems_db:release_connection(Datasource2);
			_Error -> 
				ems_logger:warn("~p has no connection to load users from datasource.", [?SERVER])
		end
	catch
		_Exception:Reason3 -> {error, Reason3}
	end.

insert_users([], Count) -> Count;
insert_users([{Codigo, Login, Name, Cpf, Email, Password}|T], Count) ->
	User = #user{id = ems_db:sequence(user),
				 codigo = Codigo,
				 login = list_to_binary(string:to_lower(ems_util:utf8_list_to_string(Login))),
				 name = list_to_binary(ems_util:utf8_list_to_string(Name)),
				 cpf = list_to_binary(ems_util:utf8_list_to_string(Cpf)),
				 email = list_to_binary(ems_util:utf8_list_to_string(Email)),
				 password = list_to_binary(ems_util:utf8_list_to_string(Password))},
	mnesia:write(User),
	insert_users(T, Count+1).

sql_load_users() ->	 
	"select p.PesCodigoPessoa, rtrim(u.UsuLogin), rtrim(p.PesNome), p.PesCpf, rtrim(p.PesEmail), u.UsuSenha
	 from BDPessoa.dbo.TB_Pessoa p join BDAcesso.dbo.TB_Usuario u on p.PesCodigoPessoa = u.UsuPesIdPessoa".

