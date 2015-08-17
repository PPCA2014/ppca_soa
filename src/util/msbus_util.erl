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
		 list_to_binlist/1]).

%% @doc Dorme por um determinado tempo
sleep(T) ->
    receive
	    after T -> true
    end.

%% @doc Retorna o timestamp em formato texto
timestamp_str() ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("~p/~p/~p ~p:~p:~p", [Dia, Mes, Ano, Hora, Min, Seg])).

date_to_string({{Ano,Mes,Dia},{_Hora,_Min,_Seg}}) ->
    lists:flatten(io_lib:format("~2..0B/~2..0B/~4..0B", [Dia, Mes, Ano])).

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


%% @doc Boolean indicando se DateTime ocorreu no período (min, hour, day, week, year)
no_periodo(DateTime, Periodo) ->
	S1 = calendar:datetime_to_gregorian_seconds(DateTime),
	S2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	case Periodo of
		"min"   ->  (S2 - S1) =< 60;
		"hour"  ->  (S2 - S1) =< 3600;
		"day"   ->  (S2 - S1) =< 86400;
		"week"  ->  (S2 - S1) =< 604800;
		"month" ->  (S2 - S1) =< 2629800;
		"year"  ->  (S2 - S1) =< 31557600;
		_ -> erlang:error(badarg)
	end.


%% @doc Obtém a hora atual em milisegundos
-spec get_milliseconds() -> integer().
get_milliseconds() ->
   {Mega, Sec, Micro} = erlang:timestamp(),
   (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%% @doc Remove o último backslash da Url
remove_ult_backslash_url("/") -> "/";
remove_ult_backslash_url(Url) -> 
	case lists:suffix("/", Url) of
		true -> lists:droplast(Url);
		false -> Url
	end.

%% @doc Função name case
name_case([H|T]) when H >= $a, H =< $z -> 
	[H + ($A - $a) | T];
name_case(outros) -> outros.


%% @doc Primeiro caracter de cada palabra em caixa alta
modernize([H|T]) -> 
	Tokens = string:tokens([H|T], " "),
	Lista = [name_case(S) || S <- Tokens],
	string:join(Lista, " ").

