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
		 binary_to_bool/1,
		 binary_to_integer/1,
		 mes_extenso/1,
		 binlist_to_list/1,
		 join_binlist/2,
		 list_to_ets/3,
		 profile/0,
		 new_rowid_servico/2]).

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

binlist_to_list([]) -> [];
binlist_to_list([H|T]) -> [binary_to_list(H)|binlist_to_list(T)].

join_binlist([], _) -> "";
join_binlist(BinList, Str) -> string:join(binlist_to_list(BinList), Str).

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

%% @doc Converte boolean binário para o atom true|false
binary_to_bool(<<"true">>) -> true;
binary_to_bool(<<"false">>) -> false;
binary_to_bool(_) -> false.

binary_to_integer(Bin) -> list_to_integer(binary_to_list(Bin)).

%% @doc Retorna o mês por extenso a partir do ordinal
mes_extenso(1) -> "Janeiro";
mes_extenso(2) -> "Fevereiro";
mes_extenso(3) -> "Março";
mes_extenso(4) -> "Abril";
mes_extenso(5) -> "Maio";
mes_extenso(6) -> "Junho";
mes_extenso(7) -> "Julho";
mes_extenso(8) -> "Agosto";
mes_extenso(9) -> "Setembro";
mes_extenso(10) -> "Outubro";
mes_extenso(11) -> "Novembro";
mes_extenso(12) -> "Dezembro";
mes_extenso(_) -> erlang:error(badarg).

%% @doc Retorna um ets a partir de uma lista
list_to_ets(List, Name, Options) ->
	Ets = ets:new(Name, Options),
	lists:foreach(fun(X) -> ets:insert(Ets, X) end, List),
	Ets.
	
profile() ->
	fprof:trace([stop]),
	fprof:profile(),
	fprof:analyse([totals, {dest, "fprof.txt"}]).

new_rowid_servico(<<Url/binary>>, <<Type/binary>>) ->	
	[PrefixUrl|Url2] = binary_to_list(Url),
	case PrefixUrl of
		$^ -> iolist_to_binary([Type, <<"#">>, list_to_binary(Url2)]);
		_  -> iolist_to_binary([Type, <<"#">>, Url])
	end;

new_rowid_servico(Url, Type) ->	
	[PrefixUrl|Url2] = Url,
	case PrefixUrl of
		$^ -> iolist_to_binary([Type, <<"#">>, Url2]);
		_  -> iolist_to_binary([Type, <<"#">>, Url])
	end.

