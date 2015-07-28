%%********************************************************************
%% @title Módulo de utilitários
%% @version 1.0.0
%% @doc Contém funções de propósito gerais.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_util).

-compile(export_all).

-export([sleep/1,
		 timestamp_str/0,
		 json_encode/1,
		 json_decode/1,
		 hd_or_empty/1,
		 json_decode_as_map/1,
		 tuple_to_binlist/1, 
		 list_to_binlist/1, 
		 msg_campo_obrigatorio/2,
		 msg_email_invalido/2,
		 mensagens/1]).

-include("../include/msbus_config.hrl").

%% @doc Dorme por um determinado tempo
sleep(T) ->
    receive
	    after T -> true
    end.

%% @doc Retorna o timestamp em formato texto
timestamp_str() ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("~p/~p/~p ~p:~p:~p", [Dia, Mes, Ano, Hora, Min, Seg])).


tuple_to_binlist(T) ->
	L = tuple_to_list(T),
	list_to_binlist(L).

list_to_binlist([]) -> [];
list_to_binlist(<<V/binary>>) -> [V];
list_to_binlist([H|T]) -> [item_to_binary(H)|list_to_binlist(T)].

item_to_binary([]) -> <<>>;

item_to_binary(<<I/binary>>) -> I;

item_to_binary(T) when is_tuple(T) -> 
	tuple_to_binlist(T);

item_to_binary(L) when is_list(L) -> 
	case io_lib:printable_list(L) of
		true -> iolist_to_binary(L);
		false -> list_to_binlist(L)
	end;
	
item_to_binary(I) when is_integer(I) -> 
	I;

item_to_binary(I) when is_atom(I) -> 
	[I2] = io_lib:format("~p", [I]),
	iolist_to_binary(I2);

item_to_binary(I) when is_map(I) -> I;

item_to_binary(I) -> iolist_to_binary(I).


%% @doc Converte dados Erlang para JSON
json_encode([]) -> <<>>;

json_encode(T) when is_tuple(T) ->
	L = tuple_to_binlist(T),
	jsx:encode(L);

json_encode(L) when is_list(L) ->
	case io_lib:printable_list(L) of
		true -> L2 = iolist_to_binary(L);
		false -> L2 = list_to_binlist(L)
	end,
	jsx:encode(L2);

json_encode(Value)->
	jsx:encode(Value).

%% @doc Converte um JSON para dados Erlang usando map
json_decode_as_map(JSON) ->
	try
		Result = jsx:decode(JSON, [return_maps]),
		{ok, Result}
	catch
		_Exception:Reason -> {error, Reason}
	end.

%% @doc Converte um JSON para dados Erlang
json_decode(JSON) ->
	try
		Result = jsx:decode(JSON),
		{ok, Result}
	catch
		_Exception:Reason -> {error, Reason}
	end.
	
%% @doc Retorna o primeiro item da lista ou vazio
hd_or_empty(List) when length(List) > 0 -> 
	hd(List);

%% @doc Retorna o primeiro item da lista ou vazio	
hd_or_empty(_) -> [].

%% @doc Retorna a string com aspas
% quote(Str) -> [$", Str, $"].

%% @doc Mensagem de campo obrigatório
msg_campo_obrigatorio(NomeCampo, []) -> 
	list_to_binary(io_lib:format("Campo obrigatorio: ~s.", [NomeCampo]));
msg_campo_obrigatorio(NomeCampo, <<>>) -> 
	list_to_binary(io_lib:format("Campo obrigatorio: ~s.", [NomeCampo]));
msg_campo_obrigatorio(_NomeCampo, _Value) -> [].

%% @doc Mensagem de e-mail inválido
msg_email_invalido(_NomeCampo, []) -> [];
msg_email_invalido(NomeCampo, Value) -> 
	case re:run(Value, "\\b[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}\\b") of
		nomatch -> list_to_binary(io_lib:format("E-mail invalido: ~s.", [NomeCampo]));
		_ -> []
	end.

%% @doc Retorna somente mensagens não vazias
mensagens(L) -> lists:filter(fun(X) -> X /= [] end, L).

%% @doc Boolean indicando se DateTime ocorreu na última hora
in_last_hour(DateTime) ->
	S1 = calendar:datetime_to_gregorian_seconds(DateTime),
	S2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	(S2 - S1) =< 3600.

%% @doc Boolean indicando se DateTime ocorreu no último dia	  
in_last_day(DateTime) ->
	S1 = calendar:datetime_to_gregorian_seconds(DateTime),
	S2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	(S2 - S1) =< 86400.
	
%% @doc Boolean indicando se DateTime ocorreu no última semana	  
in_last_week(DateTime) ->
	S1 = calendar:datetime_to_gregorian_seconds(DateTime),
	S2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	(S2 - S1) =< 604800.

%% @doc Boolean indicando se DateTime ocorreu no último mês	  
in_last_month(DateTime) ->
	S1 = calendar:datetime_to_gregorian_seconds(DateTime),
	S2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	(S2 - S1) =< 2629800.
	
%% @doc Boolean indicando se DateTime ocorreu no último ano	  
in_last_year(DateTime) ->
	S1 = calendar:datetime_to_gregorian_seconds(DateTime),
	S2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	(S2 - S1) =< 31557600.

%% @doc Obtém a hora atual em milisegundos
-spec get_milliseconds() -> integer().
get_milliseconds() ->
   {Mega, Sec, Micro} = erlang:now(),
   (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%% @doc Remove o último backslash da Url
remove_ult_backslash_url("/") -> "/";
remove_ult_backslash_url(Url) -> 
	case lists:suffix("/", Url) of
		true -> lists:droplast(Url);
		false -> Url
	end.
