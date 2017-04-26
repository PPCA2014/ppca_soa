
%%********************************************************************
%% @title Módulo ems_page
%% @version 1.0.0
%% @doc Contém funções para compilação de paginas Django
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_util).

-compile(export_all).

-include("../../include/ems_config.hrl").

-define(UNIXTIME_BASE,62167219200).
-define(DEFAULT_DISAMBIG, prefer_standard).

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
		 make_rowid_from_url/2,
		 get_params_from_url/1,
		 get_rowid_and_params_from_url/2,
		 string_is_integer/1,
		 read_file_as_map/1,
		 node_is_live/1,
		 get_node_name/0,
		 get_priv_dir/0,
		 get_working_dir/0,
		 json_encode_table/2,
		 json_decode_as_map_file/1,
		 date_add_minute/2,
		 date_add_second/2,
		 date_add_day/2,
		 date_to_string/1,
		 date_to_binary/1,
		 timestamp_str/1,
		 timestamp_binary/1,
		 remove_quoted_str/1,
		 boolean_to_binary/1,
		 normalize_field_utf8/1,
		 utf8_string_win/1,
		 utf8_string_linux/1,
		 replace/3,
		 replace_all/2,
		 read_file_as_string/1,
		 is_number/1,
		 encrypt_public_key/2,
		 decrypt_private_key/2,
		 open_file/1,
		 is_cpf_valid/1, is_cnpj_valid/1]).


%% Retorna o hash da url e os parâmetros do request
hashsym_and_params(S) when is_binary(S) -> hashsym_and_params(binary_to_list(S), 1, 0, []);
hashsym_and_params(S) -> hashsym_and_params(S, 1, 0, []).

hashsym_and_params([], _Idx, Hash, Params) -> 
	{Hash, maps:from_list(Params)};
hashsym_and_params([H|[N|_] = L], Idx, Hash, Params) when H == 47 andalso N >= 48 andalso N =< 57 -> 
	{L2, P} = hashsym_and_params_id(L, 0),
	P2 = case Idx of
			1 -> {<<"id">>, P};
			_ -> {list_to_binary("id_" ++ erlang:integer_to_list(Idx)), P}
		 end,
	hashsym_and_params(L2, Idx+1, (Hash + 1) bsl 1, [P2 | Params]);
hashsym_and_params([H|[N|_]], _Idx, _Hash, _Params) when H == 47 andalso N == 45 -> 
	throw(einvalid_id_object_negative);
hashsym_and_params([H|L], Idx, Hash, Params) when H == 47 -> % Ascii /
	hashsym_and_params(L, Idx, Hash, Params);
hashsym_and_params([H|T], Idx, Hash, Params) when (H >= 97 andalso H =< 122)  % Ascii a até z
												 orelse H == 95 % Ascii _
												 orelse (H >= 45 andalso H =< 57) % Ascii - até 9
												 orelse (H >= 64 andalso H =< 90) -> % Ascii @ até Z
	hashsym_and_params(T, Idx, (Hash + H) bsl 1, Params);
hashsym_and_params(_, _, _, _) -> throw(einvalid_url).
												 

hashsym_and_params_id([], P) -> 
	case P > 0 andalso P =< 999999999999 of
		true -> {[], P};
		false -> throw({einvalid_id_object, P})
	end;
hashsym_and_params_id([H|T], P) when H == 47 -> {T, P};
hashsym_and_params_id([H|T], P) when (H >= 48 andalso H =< 57) -> hashsym_and_params_id(T, P * 10 + H - $0);
hashsym_and_params_id(L, _) -> throw({einvalid_id_object, L}).


%% Retorna o hash da url (uso em tempo de execução)
hashsym(S) -> 
	{Hash, _} = hashsym_and_params(S),
	Hash.


%% Retorna o hash da url (uso no carregamento dos catálogos)	
make_rowid(S) when is_binary(S) -> make_rowid(binary_to_list(S), 0);
make_rowid(S) -> make_rowid(S, 0).

make_rowid([], Hash) -> Hash;
make_rowid([H|T], Hash) when H == 47 -> make_rowid(T, Hash);
make_rowid([H|T], Hash) when H == 58 -> make_rowid(make_rowid_id(T), (Hash + 1) bsl 1);
make_rowid([H|T], Hash) -> make_rowid(T, (Hash + H) bsl 1).

make_rowid_id([]) -> [];
make_rowid_id([H|T]) when H == 47 -> T;
make_rowid_id([_|T]) -> make_rowid_id(T).


get_priv_dir() ->
	{ok, Path} = file:get_cwd(),
	Path ++ "/priv".

get_working_dir() ->
	{ok, Path} = file:get_cwd(),
	Path.


%% @doc Dorme por um determinado tempo
sleep(T) ->
    receive
	    after T -> true
    end.

%% @doc Retorna o timestamp em formato texto
timestamp_str() ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("~2..0w/~2..0w/~4..0w ~2..0w:~2..0w:~2..0w", [Dia, Mes, Ano, Hora, Min, Seg])).

timestamp_str(null) -> "";
timestamp_str(undefined) -> "";
timestamp_str({{Ano,Mes,Dia},{Hora,Min,Seg}}) ->
  lists:flatten(io_lib:format("~2..0w/~2..0w/~4..0w ~2..0w:~2..0w:~2..0w", [Dia, Mes, Ano, Hora, Min, Seg])).

timestamp_binary(null) -> <<>>;
timestamp_binary(undefined) -> <<>>;
timestamp_binary({{Ano,Mes,Dia},{Hora,Min,Seg}}) ->
  lists:flatten(io_lib:format("~2..0w/~2..0w/~4..0w ~2..0w:~2..0w:~2..0w", [Dia, Mes, Ano, Hora, Min, Seg])).


date_to_string(null) -> "";
date_to_string(undefined) -> "";
date_to_string({{Ano,Mes,Dia},{_Hora,_Min,_Seg}}) ->
    lists:flatten(io_lib:format("~2..0B/~2..0B/~4..0B", [Dia, Mes, Ano])).


date_to_binary(null) -> <<>>;
date_to_binary(undefined) -> <<>>;
date_to_binary({{Ano,Mes,Dia},{_Hora,_Min,_Seg}}) ->
    iolist_to_binary(io_lib:format("~2..0B/~2..0B/~4..0B", [Dia, Mes, Ano])).


tuple_to_binlist(T) ->
	L = tuple_to_list(T),
	list_to_binlist(L).

list_to_binlist([]) -> [];
list_to_binlist(<<>>) -> [];
list_to_binlist(<<V/binary>>) -> [V];
list_to_binlist([H|T]) -> [item_to_binary(H)|list_to_binlist(T)].

binlist_to_list(<<>>) -> [];
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
item_to_binary(I) when is_float(I) -> 
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
	?JSON_LIB:encode(L);
json_encode(L) when is_list(L) ->
	case io_lib:printable_list(L) of
		true -> L2 = iolist_to_binary(L);
		false -> L2 = list_to_binlist(L)
	end,
	?JSON_LIB:encode(L2);
json_encode(Value)-> ?JSON_LIB:encode(Value).


json_decode_as_map_file(FileName) ->
	case file:read_file(FileName) of
		{ok, JSON} -> json_decode_as_map(JSON);
		{error, enoent} -> {error, einvalid_json_filename}
	end.


%% @doc Converte um JSON para dados Erlang usando map
json_decode_as_map(JSON) ->
	try
		Dados1 = binary_to_list(JSON),
		Dados2 = lists:flatten(re:replace(Dados1, "[\t\r\n]", "", [global, {return,list}])),
		Dados3 = list_to_binary(Dados2),
		Result = ?JSON_LIB:decode(Dados3, [return_maps]),
		{ok, Result}
	catch
		_Exception:Reason -> {error, Reason}
	end.

%% @doc Converte um JSON para dados Erlang
json_decode(JSON) ->
	try
		JSON2 = case check_encoding_bin(JSON) of
			latin1 -> unicode:characters_to_binary(binary_to_list(JSON), latin1, utf8);
			utf8 -> JSON;
			_ -> erlang:raise(einvalid_json_encoding)
		end,
		T = ?JSON_LIB:decode(JSON2),
		{ok, element(1, T)}
	catch
		_Exception:Reason -> {error, Reason}
	end.
	
%% @doc Retorna o primeiro item da lista ou vazio
hd_or_empty(List) when length(List) > 0 -> 
	hd(List);

%% @doc Retorna o primeiro item da lista ou vazio	
hd_or_empty(_) -> [].

%% @doc Retorna a string com aspas
quote(Str) -> lists:flatten([$", Str, $"]).

remove_quoted_str(Str) -> string:substr(Str, 2, length(Str)-2).


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
remove_ult_backslash_url([H|T]) -> 
	remove_ult_backslash_url(T, [H]).

remove_ult_backslash_url([], Result) -> 
	lists:reverse(Result);
remove_ult_backslash_url("/", Result) -> 
	lists:reverse(Result);
remove_ult_backslash_url([H|T], Result) -> 
	remove_ult_backslash_url(T, [H|Result]).

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
binary_to_bool(true) -> true;
binary_to_bool(false) -> false;
binary_to_bool(<<"true">>) -> true;
binary_to_bool(<<"false">>) -> false;
binary_to_bool(<<"1">>) -> true;
binary_to_bool(<<"0">>) -> false;
binary_to_bool(<<1>>) -> true;
binary_to_bool(<<0>>) -> false;
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

mes_abreviado(1) -> "jan";
mes_abreviado(2) -> "fev";
mes_abreviado(3) -> "mar";
mes_abreviado(4) -> "abr";
mes_abreviado(5) -> "maio";
mes_abreviado(6) -> "jun";
mes_abreviado(7) -> "jul";
mes_abreviado(8) -> "ago";
mes_abreviado(9) -> "set";
mes_abreviado(10) -> "out";
mes_abreviado(11) -> "nov";
mes_abreviado(12) -> "dez";
mes_abreviado(_) -> erlang:error(badarg).



%% @doc Retorna um ets a partir de uma lista
list_to_ets(List, Name, Options) ->
	Ets = ets:new(Name, Options),
	lists:foreach(fun(X) -> ets:insert(Ets, X) end, List),
	Ets.
	
profile() ->
	fprof:trace([stop]),
	fprof:profile(),
	fprof:analyse([totals, {dest, "fprof.txt"}]).

new_rowid_service(<<Url/binary>>, <<Type/binary>>) ->	
	[PrefixUrl|Url2] = binary_to_list(Url),
	case PrefixUrl of
		$^ -> iolist_to_binary([Type, <<"#">>, list_to_binary(Url2)]);
		_  -> iolist_to_binary([Type, <<"#">>, Url])
	end;

new_rowid_service(Url, Type) ->	
	[PrefixUrl|Url2] = Url,
	case PrefixUrl of
		$^ -> iolist_to_binary([Type, <<"#">>, Url2]);
		_  -> iolist_to_binary([Type, <<"#">>, Url])
	end.

make_rowid_from_url(<<Url/binary>>, <<Type/binary>>) ->	
	make_rowid_from_url(binary_to_list(Url), binary_to_list(Type));

make_rowid_from_url(Url, Type) ->	
	Ret1 = parse_url(Url),
	Ret2 = lists:map(fun({U, _}) -> U end, Ret1),
	Ret3 = string:join(Ret2, "/"),
	iolist_to_binary([Type, <<"#/">>, Ret3]).

get_rowid_and_params_from_url(<<Url/binary>>, <<Type/binary>>) ->	
	get_rowid_and_params_from_url(binary_to_list(Url), binary_to_list(Type));

get_rowid_and_params_from_url(Url, Type) ->
	UrlParsed = parse_url(Url),
	UrlParsed2 = lists:map(fun({U, _}) -> U end, UrlParsed),
	UrlParsed3 = string:join(UrlParsed2, "/"),
	Rowid = iolist_to_binary([Type, <<"#/">>, UrlParsed3]),
	ParamsUrl = [{list_to_binary(U), P} || {[_|U], P} <- UrlParsed, P /= [] ],
	ParamsUrlMap = maps:from_list(ParamsUrl),
	{Rowid, ParamsUrlMap}.
	

get_params_from_url(Url) -> [X || {_, P} = X <- parse_url(Url), P /= [] ].


parse_url(Url) ->	
	Url2 = string:tokens(Url, "/"),
	try
		parse_url_tail(Url2, 1, [])
	catch error:badarg ->
		erlang:error(einvalid_id_object)
	end.

parse_url_tail([], _, L) -> lists:reverse(L);
	
parse_url_tail([H|T], SeqId, L) ->	
    {UrlParte, Param, SeqId2} = parse_parte_url(H, SeqId),
	parse_url_tail(T, SeqId2, [{UrlParte, Param} | L]).
	
parse_parte_url([H|_] = UrlParte, SeqId) ->
	if
		H >= 49 andalso H =< 57 ->
			SeqId_ = case SeqId of
				1 -> ":id";
				_ -> ":id_" ++ integer_to_list(SeqId)
			end,
			{SeqId_, list_to_integer(UrlParte), SeqId+1};
		H =:= 45 ->
			erlang:error(einvalid_id_object);
		true -> {UrlParte, [], SeqId}
	end.


string_is_integer(S) ->
    try
        _ = list_to_integer(S),
        true
    catch error:badarg ->
        false
    end.

node_is_live(Node) -> 
	case net_adm:ping(Node) of
		pong -> 1;
		_ -> 0
	end.

% Retorna somente a parte do name do node sem a parte do hostname após @
get_node_name() -> hd(string:tokens(atom_to_list(node()), "@")).

json_field_format_table(null) -> [<<"\""/utf8>>, <<"\""/utf8>>];
json_field_format_table(V) when is_float(V) -> list_to_binary(mochinum:digits(V));
json_field_format_table(V) when is_integer(V) -> list_to_binary(mochinum:digits(V));
json_field_format_table(V) when is_boolean(V) -> boolean_to_binary(V);
json_field_format_table(V) when is_binary(V) -> [<<"\""/utf8>>, ?UTF8_STRING(V), <<"\""/utf8>>];
json_field_format_table(V) when is_list(V) -> [<<"\""/utf8>>, ?UTF8_STRING(list_to_binary(V)), <<"\""/utf8>>];
json_field_format_table(Data = {{_,_,_},{_,_,_}}) -> [<<"\""/utf8>>, list_to_binary(date_to_string(Data)), <<"\""/utf8>>];
json_field_format_table(V) -> throw({error, einvalid_value, validation, "Could not serialize " ++ V}).

% Prepara um campo texto para o formato JSON UTF 8
normalize_field_utf8("") ->	"";
normalize_field_utf8(<<>>) -> "";
normalize_field_utf8(V) when is_binary(V) -> normalize_field_utf8(binary_to_list(V));
normalize_field_utf8(V) -> 
	Text = case string:strip(V) of
				[] -> "";
				V2 -> [case Ch of 
							34 -> "\\\""; 
							_ -> Ch 
					  end || Ch <- V2, Ch > 31]
			end,
	unicode:characters_to_binary(Text, utf8).

json_encode_record(_, [], true, RecordJson) -> 	
	[<<"{"/utf8>>, lists:reverse(RecordJson), <<"},"/utf8>>];
json_encode_record(_, [], false, RecordJson) -> 		
	[<<"{"/utf8>>, lists:reverse(RecordJson), <<"}"/utf8>>];
json_encode_record([F|FTail], [V|VTail], HasMoreRecords, RecordJson) -> 	
	Field = case VTail of
		[] -> iolist_to_binary([<<"\""/utf8>>, F, <<"\""/utf8>>, <<":"/utf8>>, json_field_format_table(V)]);
		_ -> 
			iolist_to_binary([<<"\""/utf8>>, F, <<"\""/utf8>>, <<":"/utf8>>, json_field_format_table(V), <<","/utf8>>])
	end,
	json_encode_record(FTail, VTail, HasMoreRecords, [Field | RecordJson]).


json_encode_table(_, [], TableJson) -> 
	iolist_to_binary([<<"["/utf8>>, lists:reverse(TableJson), <<"]"/utf8>>]);
json_encode_table(Fields, [R|RTail], TableJson) -> 
	Values = tuple_to_list(R),
	HasMoreRecords = RTail =/= [],
	R2 = json_encode_record(Fields, Values, HasMoreRecords, []),
	json_encode_table(Fields, RTail, [R2 | TableJson]).

-spec json_encode_table(list(binary()), list(binary())) -> string().
json_encode_table(Fields, Records) -> 
	Result = json_encode_table(Fields, Records, []),
	Result.

json_encode_table2(Fields, Records) ->
	Objects = lists:map(fun(T) -> 
							   lists:zipwith(fun(Fld, Value) -> 
													io_lib:format(<<"\"~s\":~p"/utf8>>, [Fld, json_field_format_table(Value)]) 
											 end,  Fields, tuple_to_list(T))
					end, Records), 
	Objects2 = lists:map(fun(Obj) -> 
									[<<"{"/utf8>>, string:join(Obj, ", "), <<"}"/utf8>>] 
						 end, Objects),
	Objects3 = string:join(Objects2, ", "),
	Result = unicode:characters_to_binary([<<"["/utf8>>, Objects3, <<"]"/utf8>>], utf8),
	Result.


utf8_list_to_string(null) -> "";
utf8_list_to_string(Value) ->
	try
		case check_encoding_bin(list_to_binary(Value)) of
			utf8 -> unicode:characters_to_list(mochiutf8:valid_utf8_bytes(list_to_binary(Value)), utf8);
			latin1 -> unicode:characters_to_list(Value, utf8)
		end
	catch
		_Exception:Reason -> 
			io:format("utf8_list_to_string error ~p with value ~p\n", [Reason, Value]), 
			<<>>
	end.
	

utf8_list_to_binary(Value) -> binary_to_list(utf8_list_to_string(Value)).

utf8_binary_to_list(Value) ->
	case unicode:characters_to_list(Value) of
		{error, _, _ } -> Value;
		Value2 -> Value2
	end.


check_encoding_bin(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin,utf8,utf8) of
	Bin ->
	    utf8;
	_ ->
	    latin1
    end.

date_add_minute(Timestamp, Minutes) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Timestamp) + Minutes * 60).

date_add_second(Timestamp, Seconds) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Timestamp) + Seconds).

date_add_day(Timestamp, Days) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Timestamp) + Days * 86400).

% Return a encrypted password in binary format        
criptografia_sha1(<<>>) -> <<>>;
criptografia_sha1("") -> <<>>;	
criptografia_sha1(undefined) -> <<>>;
criptografia_sha1(null) -> <<>>;
criptografia_sha1(Password) when is_binary(Password) ->	criptografia_sha1(binary_to_list(Password));
criptografia_sha1(Password) -> base64:encode(sha1:binstring(Password)).

boolean_to_binary(true) -> <<"true"/utf8>>;
boolean_to_binary(false) -> <<"false"/utf8>>;
boolean_to_binary(1) -> <<"true"/utf8>>;
boolean_to_binary(0) -> <<"false"/utf8>>;
boolean_to_binary(<<"true"/utf8>>) -> <<"true"/utf8>>;
boolean_to_binary(<<"false"/utf8>>) -> <<"false"/utf8>>;
boolean_to_binary(<<"1"/utf8>>) -> <<"true"/utf8>>;
boolean_to_binary(<<"0"/utf8>>) -> <<"false"/utf8>>;
boolean_to_binary(_) -> <<"false"/utf8>>.


%%melhorar este método para conversão para utf8
utf8_string_win(<<>>) -> <<""/utf8>>;
utf8_string_win("") -> <<""/utf8>>;
utf8_string_win(undefined) -> <<""/utf8>>;
utf8_string_win(null) -> <<""/utf8>>;
utf8_string_win(Text) when is_list(Text) -> 
	utf8_string_win(list_to_binary(Text));
utf8_string_win(Text) when erlang:is_number(Text) -> integer_to_binary(Text);
utf8_string_win(Text) ->
	try
		case ems_util:check_encoding_bin(Text) of
			utf8 -> normalize_field_utf8(Text);
			latin1 -> normalize_field_utf8(Text);
			Other -> Other
		end
	catch
		_Exception:Reason -> 
			?DEBUG("utf8_string_linux convert ~p error: ~p\n", [Text, Reason]),
			Text
	end.

utf8_string_linux(<<>>) -> <<""/utf8>>;
utf8_string_linux("") -> <<""/utf8>>;
utf8_string_linux(undefined) -> <<""/utf8>>;
utf8_string_linux(null) -> <<""/utf8>>;
utf8_string_linux(Text) when is_list(Text) -> 
	utf8_string_linux(list_to_binary(Text));
utf8_string_linux(Text) when erlang:is_number(Text) -> integer_to_binary(Text);
utf8_string_linux(Text) ->
	try
		case ems_util:check_encoding_bin(Text) of
			utf8 -> normalize_field_utf8(Text);
			latin1 -> normalize_field_utf8(Text);
			Other -> Other
		end
	catch
		_Exception:Reason -> 
			?DEBUG("utf8_string_linux convert ~p error: ~p\n", [Text, Reason]),
			Text
	end.
	

-spec read_file_as_map(FileName :: string()) -> map().
read_file_as_map(FileName) -> 	
	case file:read_file(FileName) of
		{ok, Arq} -> json_decode_as_map(Arq);
		Error -> Error
	end.

-spec replace(string(), string(), string()) -> string().
replace(Subject, Var, VarToReplace) -> 
	re:replace(Subject, Var, VarToReplace, [global, {return, list}]).

-spec replace_all(string(), list(tuple())) -> string().
replace_all(Subject, []) -> Subject;
replace_all(Subject, [{Key, Value}|VarTail]) -> 
	NewSubject = replace(Subject, Key, Value),
	replace_all(NewSubject, VarTail).


-spec replace_all_vars(string(), list(tuple())) -> string().
replace_all_vars(Subject, []) -> Subject;
replace_all_vars(Subject, [{Key, Value}|VarTail]) -> 
	NewSubject = replace(Subject, "{{ "++ binary_to_list(Key) ++ " }}", Value),
	replace_all_vars(NewSubject, VarTail).


read_file_as_string(FileName) -> 	
	case file:read_file(FileName) of
		{ok, Arq} -> Arq;
		Error -> throw(Error)
	end.
	

encrypt_public_key(PlainText, PublicKey) ->
	[ RSAEntry2 ] = public_key:pem_decode(PublicKey),
	PubKey = public_key:pem_entry_decode( RSAEntry2 ),
	public_key:encrypt_public(PlainText, PubKey).
	
decrypt_private_key(CryptText,PrivateKey) ->
    [ RSAEntry2 ] = public_key:pem_decode(PrivateKey),
	PrivKey = public_key:pem_entry_decode( RSAEntry2 ),
	Result =  public_key:decrypt_private(CryptText, PrivKey ),
	Result.
   

open_file(FilePath) ->
   {ok, PemBin2 } = file:read_file(FilePath),
    PemBin2.

%% Converte arquivo latin1 para utf8 formatando os unicodes
%% Esta função está desconfigurando os arquivos no formato utf8	
to_utf8(FileName) ->
	try
		{ok, File} = file:open(FileName, [read,binary]),
		Size = filelib:file_size(FileName),
		{ok, Device} = file:read(File,Size),
		{Type, _Bytes} = unicode:bom_to_encoding(Device),
		case Type of
			utf8 -> Device;	
			_ -> unicode:characters_to_binary(Device, latin1, utf8)
		end,
		{ok, Device}
	catch
		_Exception:Reason -> {error, Reason}
	end.

-spec is_letter(string()) -> boolean().
is_letter(V) ->
	is_letter_lower(string:to_lower(V)).
is_letter_lower("c") -> true;
is_letter_lower("d") -> true;
is_letter_lower("e") -> true;
is_letter_lower("f") -> true;
is_letter_lower("g") -> true;
is_letter_lower("h") -> true;
is_letter_lower("i") -> true;
is_letter_lower("j") -> true;
is_letter_lower("k") -> true;
is_letter_lower("l") -> true;
is_letter_lower("m") -> true;
is_letter_lower("n") -> true;
is_letter_lower("o") -> true;
is_letter_lower("p") -> true;
is_letter_lower("q") -> true;
is_letter_lower("r") -> true;
is_letter_lower("s") -> true;
is_letter_lower("t") -> true;
is_letter_lower("u") -> true;
is_letter_lower("v") -> true;
is_letter_lower("x") -> true;
is_letter_lower("z") -> true;
is_letter_lower("a") -> true;
is_letter_lower("b") -> true;
is_letter_lower(_) -> false.


-spec is_number(string()) -> boolean().
is_number("") -> false;
is_number(V) -> [Char || Char <- V, Char < $0 orelse Char > $9] == [].


-spec is_cpf_valid(list() | binary()) -> boolean().
is_cpf_valid(S) when is_binary(S) ->
	is_cpf_valid(binary_to_list(S));
is_cpf_valid(S) ->
	case ems_util:is_number(S) andalso string:len(S) =:= 11 of
		true -> 
			C = [  X || X <- S, X > 47 andalso X < 58 ],
			D = lists:sum( lists:zipwith(fun(X,Y) -> (X-48)*Y end, C, [1,2,3,4,5,6,7,8,9,0,0]) ) rem 11,
			D =:= lists:nth(10, C) - 48 andalso	( lists:sum(lists:zipwith(fun(X,Y) -> (X-48)*Y end, C, [0,1,2,3,4,5,6,7,8,0,0]) ) + D * 9 ) rem 11 =:= lists:nth(11, C) - 48;
		false -> false
	end.

-spec is_cnpj_valid(list() | binary()) -> boolean().		
is_cnpj_valid(S) when is_binary(S) -> 
	is_cnpj_valid(binary_to_list(S));
is_cnpj_valid(S) ->
	case ems_util:is_number(S) andalso string:len(S) =:= 13 of
		true ->
			C = [  X || X <- S, X > 47 andalso X < 58 ],
			D = lists:sum( lists:zipwith(fun(X,Y) -> (X-48) * Y end, C, [6,7,8,9,2,3,4,5,6,7,8,9,0,0]) ) rem 11,
			D =:= lists:nth(13, C) - 48 andalso ( lists:sum(lists:zipwith(fun(X,Y) -> (X-48) * Y end, C, [5,6,7,8,9,2,3,4,5,6,7,8,0,0]) ) + D * 9 ) rem 11 =:= lists:nth(14, C) - 48;
		_ -> false
	end.


load_erlang_module(FileName) ->
	ModuleName = filename:rootname(filename:basename(FileName)),
	ModuleNameAtom = list_to_atom(ModuleName),
	FileNameMod = filename:rootname(FileName) ++ ".erl",
	case filelib:file_size(FileNameMod) > 0 of
		true ->
			case code:ensure_loaded(ModuleNameAtom) of
				{module, _} -> {ok, ModuleNameAtom};
				_Error -> 
					FileNamePath = filename:dirname(FileName), 
					code:add_path(FileNamePath), 
					case compile:file(FileNameMod, [{outdir, FileNamePath ++ "/"}]) of
						error -> 
							io:format("[ ERROR ]\n"),
							{error, einvalid_module_sintax};
						{error, Errors, _Warnings} -> 
							io:format("[ ERROR ]\n"),
							io:format_error("~p\n", [Errors]),
							{error, einvalid_module_sintax};
						_ -> 
							io:format("[ OK ]\n"),
							{ok, ModuleNameAtom}
					end
			end;
		false -> {error, enoent}
	end.
