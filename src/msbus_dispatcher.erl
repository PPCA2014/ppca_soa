%%********************************************************************
%% @title Módulo dispatcher
%% @version 1.0.0
%% @doc Módulo responsável pelo componente dispatcher do erlangMS.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_dispatcher).

-behavior(gen_server). 

-include("../include/msbus_config.hrl").
-include("../include/msbus_http_messages.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([dispatch_request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================

dispatch_request(Request) -> 
	gen_server:call(?SERVER, {dispatch_request, Request}).

	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Param, State) ->
	{noreply, State}.
    
handle_call({dispatch_request, Request}, _From, State) ->
	Reply = do_dispatch_request(Request),
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
%% Internal functions
%%====================================================================

%% @doc Trata o request e retorna o response do resultado
do_dispatch_request(Request) ->
	case msbus_catalogo:lookup(Request#request.url, Request#request.metodo) of
		{ok, Servico, ParamsUrl} -> 
			Request1 = Request#request{params_url = ParamsUrl, servico = Servico},
			case executa_servico(Request1) of
				{error, servico_falhou, ErroInterno} -> 
					Response = msbus_http_util:encode_response(<<"502">>, ?HTTP_ERROR_502(ErroInterno)),
					{error, <<"502">>, Request1, Response, servico_falhou};
				Pid -> 
					aguarda_conclusao_servico(Request1, Pid)
			end;
		notfound -> 
			Response = msbus_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404),
			{error, <<"404">>, Request, Response, servico_nao_encontrado}
	end.

%% @doc Aguarda a conclusão do resultado do serviço
aguarda_conclusao_servico(Request, Pid) ->
	receive
		{ok, Result} ->
			Response = msbus_http_util:encode_response(<<"200">>, Result),
			{ok, <<"200">>, Request, Response};
		{ok, Result, MimeType} ->
			Response = msbus_http_util:encode_response(<<"200">>, Result, MimeType),
			{ok, <<"200">>, Request, Response};
		{error, servico_nao_disponivel, _ErroInterno} ->
			Response = msbus_http_util:encode_response(<<"503">>, ?HTTP_ERROR_503),
			{error, <<"503">>, Request, Response, servico_nao_disponivel};
		{error, file_not_found} ->
			Response = msbus_http_util:encode_response(<<"404">>, ?HTTP_ERROR_404_FILE_NOT_FOUND),
			{error, <<"404">>, Request, Response, file_not_found}
		after 3000 ->
			case is_process_alive(Pid) of
				true -> ok;
				false -> 
					Response = msbus_http_util:encode_response(<<"502">>, ?HTTP_ERROR_502),
					{error, <<"502">>, Request, Response, servico_falhou}
			end
	end.


%% @doc Executa o serviço correspondente
executa_servico(Request) ->
	Servico = Request#request.servico,
	Module = binary_to_list(msbus_catalogo:get_property_servico(<<"module">>, Servico)),
	Function = binary_to_list(msbus_catalogo:get_property_servico(<<"function">>, Servico)),
	Module2 = list_to_atom(Module),
	Function2 = list_to_atom(Function),
	try
		case whereis(Module2) of
			undefined -> 
				Pid = Module2:start(),
				apply(Module2, Function2, [Request, self()]),
				Pid;
			Pid -> 
				apply(Module2, Function2, [Request, self()]),
				Pid
		end
	catch
		_Exception:ErroInterno ->  {error, servico_falhou, ErroInterno}
	end.
