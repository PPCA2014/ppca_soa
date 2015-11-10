%%********************************************************************
%% @title Módulo auth_user
%% @version 1.0.0
%% @doc Módulo responsável pela autenticação dos usuários.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_auth_user).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").

%% Server API
-export([start/0, stop/0]).

%% Cliente interno API
-export([autentica/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3, start_link/1]).

-define(SERVER, ?MODULE).

%  Armazena o estado do servico. 
-record(state, {}). 


%%====================================================================
%% Server API
%%====================================================================

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
autentica(Request) ->
	gen_server:call(?SERVER, {autentica, Request}).
	


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State}.

handle_call({autentica, Request}, _From, State) ->
	Reply = do_autentica(Request),
	{reply, Reply, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================
    
do_autentica(Request) ->
	try
		Contract = Request#request.servico,
		case Contract#servico.authentication of
			<<"Basic">> -> do_basic_authorization(Request);
			<<>> -> {ok, anonimo}
		end
	catch
		_Exception:_Reason ->  {error, no_authorization} 
	end.

do_basic_authorization(Request) ->
	case Request#request.authorization /= "" of
		true -> 
			[Authorization|[UserNameEPassword|_]] = string:tokens(Request#request.authorization, " "),
			case Authorization =:= "Basic" of
				true -> 
					UserNameEPassword2 = base64:decode_to_string(UserNameEPassword),
					[UserName|[Password|_]] = string:tokens(UserNameEPassword2, ":"),
					case msbus_user:call({find_by_username_and_password, list_to_binary(UserName), list_to_binary(Password)}) of
						{ok, User} -> {ok, User};
						_ -> {error, no_authorization}
					end;
				false -> {error, no_authorization}
			end;
		false -> {error, no_authorization}
	end.
 	


