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
			gen_server:cast(self(), {servico, Request2, {async, Request2#request.servico#servico.async}}),
			case executa_servico(Request2) of
				ok -> ok;
				Error -> gen_server:cast(self(), {servico, Request2, Error})
			end;
		notfound -> 
			gen_server:cast(self(), {servico, Request, {error, notfound}})
	end.

%% @doc Executa o serviço local
executa_servico(Request=#request{servico=#servico{host='', module=Module, function=Function}}) ->
	try
		case whereis(Module) of
			undefined -> 
				Module:start(),
				apply(Module, Function, [Request, self()]),
				ok;
			_Pid -> 
				apply(Module, Function, [Request, self()]),
				ok
		end
	catch
		_Exception:ErroInterno ->  {error, servico_falhou, ErroInterno}
	end;

%% @doc Executa o serviço Java
executa_servico(Request=#request{servico=#servico{host=Host, module=Module, function=_Function}}) ->
	{Module, Host} ! {Request, self()},
	%aguarda_msg_java(Request),
	ok.


aguarda_msg_java(Request) ->
			receive
				{ok, V} -> 
					io:format("recebido: ~p\n", [V]),
					gen_server:cast(self(), {servico, Request, V});
				Msg -> 
					io:format("outra msg: ~p\n", [Msg]),
					aguarda_msg_java(Request)
			end.
			
	
	
