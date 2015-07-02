%% ---
%%  ppca_dispatcher
%%  Mestrado em Computação Aplicada - Universidade de Brasília
%%  Turma de Construção de Software / PPCA 2014
%%  Professor: Rodrigo Bonifacio de Almeida
%%  Aluno: Everton de Vargas Agilar (evertonagilar@gmail.com)
%%---
-module(ppca_dispatcher).

-behavior(gen_server). 

-include("../include/ppca_config.hrl").
-include("../include/http_messages.hrl").

%% Server API
-export([start/0, stop/0]).

%% Client API
-export([dispatch_request/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start() -> 
    Result = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    ppca_logger:info_msg("ppca_dispatcher iniciado."),
    Result.
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================

dispatch_request(From, HeaderDict, Payload) -> 
	gen_server:cast(?SERVER, {dispatch_request, HeaderDict, Payload, From}).

	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({dispatch_request, HeaderDict, Payload, From}, State) ->
	do_dispatch_request(From, HeaderDict, Payload),
	{noreply, State}.
    
handle_call({dispatch_request, HeaderDict, Payload}, From, State) ->
	do_dispatch_request(From, HeaderDict, Payload),
	{reply, ok, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ppca_logger:info_msg("ppca_dispatcher finalizado."),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Despacha a requisição para o serviço correspondente
do_dispatch_request(From, HeaderDict, Payload) ->
	_Metodo = dict:fetch("Metodo", HeaderDict),
	Url = dict:fetch("Url", HeaderDict),
	case ppca_catalogo_service:lookup(Url) of
		{ok, Servico} -> 
			executa_servico(From, HeaderDict, Payload, Servico, []);
		{ok, Servico, ParamsUrl} -> 
			executa_servico(From, HeaderDict, Payload, Servico, ParamsUrl);
		notfound -> 
			ErroInterno = io_lib:format(?MSG_SERVICO_NAO_ENCONTRADO, [Url]),
			From ! {error, servico_nao_encontrado, ErroInterno}
	end.

%% @doc Executa o serviço correspondente
executa_servico(From, HeaderDict, Payload, Servico, ParamsUrl) ->
	Module = ppca_catalogo_service:get_property_servico(<<"module">>, Servico),
	Function = ppca_catalogo_service:get_property_servico(<<"function">>, Servico),
	Request = encode_request(HeaderDict, Payload, Servico, ParamsUrl),
	case executa_processo_erlang(Module, Function, Request, From) of
		em_andamento -> ok;	%% o serviço se encarrega de enviar mensagem quando estiver pronto
		Error -> From ! Error
	end.

%% @doc Gera um objeto request com os dados da requisição
encode_request(HeaderDict, Payload, Servico, ParamsUrl) ->
	Request = #request{http_headers = HeaderDict,
					   payload = Payload,
					   servico = Servico,
					   params_url = ParamsUrl},
	Request.

%% @doc Executa o processo erlang de um serviço
executa_processo_erlang(Module, Function, Request, From) ->
	try
		case whereis(Module) of
			undefined -> 
				Module:start(),
				apply(Module, Function, [Request, From]);
			Pid -> 
				apply(Module, Function, [Request, From])
		end,
		em_andamento
	catch
		_Exception:ErroInterno ->  {error, servico_falhou, ErroInterno}
	end.
