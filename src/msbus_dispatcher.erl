%%********************************************************************
%% @title Módulo dispatcher
%% @version 1.0.0
%% @doc Responsável pelo encaminhamento das requisições ao serviço REST.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_dispatcher).

-export([dispatch_request/1]).

-include("../include/msbus_config.hrl").
-include("../include/msbus_schema.hrl").
-include("../include/msbus_http_messages.hrl").

%% @doc Trata o request e retorna o response do resultado
dispatch_request(Request) ->
	case msbus_catalogo:lookup(Request) of
		{ok, Request2} -> 
			Async = maps:get(<<"async">>, Request2#request.servico),
			gen_server:cast(self(), {servico, Request2, {async, Async}}),
			case executa_servico(Request2) of
				ok -> ok;
				Error -> gen_server:cast(self(), {servico, Request2, Error})
			end;
		notfound -> 
			gen_server:cast(self(), {servico, Request, {error, notfound}})
	end.

%% @doc Executa o serviço correspondente
executa_servico(Request) ->
	Servico = Request#request.servico,
	Module = binary_to_list(msbus_catalogo:get_property_servico(<<"module">>, Servico)),
	Function = binary_to_list(msbus_catalogo:get_property_servico(<<"function">>, Servico)),
	Host = msbus_catalogo:get_property_servico(<<"host">>, Servico),
	Module2 = list_to_atom(Module),
	Function2 = list_to_atom(Function),
	case Host of
		<<>> ->
			try
				case whereis(Module2) of
					undefined -> 
						Module2:start(),
						apply(Module2, Function2, [Request, self()]),
						ok;
					_Pid -> 
						apply(Module2, Function2, [Request, self()]),
						ok
				end
			catch
				_Exception:ErroInterno ->  {error, servico_falhou, ErroInterno}
			end;
		_ ->
			Host2 = list_to_atom(Host),
			{Module2, Host2} ! {self(), "count"},
			ok
	end.
	
	
