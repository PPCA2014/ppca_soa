%%********************************************************************
%% @title Module ems_util
%% @version 1.0.0
%% @doc Contains general purpose functions
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_util).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([version/0,
		 server_name/0,
		 sleep/1,
		 json_encode/1,
		 json_decode/1,
		 hd_or_empty/1,
		 json_decode_as_map/1,
		 json_encode_table/2,
		 json_decode_as_map_file/1,
		 json_field_strip_and_escape/1,
		 tuple_to_binlist/1, 
		 binlist_to_atomlist/1,
		 list_to_binlist/1,
		 binary_to_integer/1,
		 mes_extenso/1,
		 binlist_to_list/1,
		 join_binlist/2,
		 list_to_ets/3,
		 profile/0,
		 make_rowid_from_url/2,
		 string_is_integer/1,
		 read_file_as_map/1,
		 read_file_as_list/1,
 		 read_file_as_string/1,
		 tail_file/2,
		 load_from_file_req/1,
		 node_is_live/1,
 		 node_binary/0,
		 get_node_name/0,
		 get_params_from_url/1,
		 get_rowid_and_params_from_url/2,
		 get_priv_dir/0,
		 get_working_dir/0,
 		 get_milliseconds/0,
		 get_property_request/2, 
		 get_param_url/3,
		 get_querystring/3,
		 get_querystring/4,
         get_querystring/2,
         date_add_minute/2,
         date_dec_minute/2,
         date_add_second/2,
         date_dec_second/2,
		 date_add_day/2,
		 date_to_string/1,
		 date_to_binary/1,
 		 no_periodo/2,
 		 timestamp_str/0,
		 timestamp_str/1,
		 timestamp_binary/0,
		 timestamp_binary/1,
		 uptime_str/0,
		 boolean_to_binary/1,
 		 replacenth/3,
		 replace/3,
		 replace_all/2,
		 encrypt_public_key/2,
		 decrypt_private_key/2,
		 open_file/1,
		 file_last_modified/1,
		 is_number/1,
		 is_cpf_valid/1, 
		 is_cnpj_valid/1, 
		 ip_list/0,
		 is_url_valido/1,
 		 is_email_valido/1, 
 		 is_range_valido/3,
		 is_letter/1,
		 is_letter_lower/1,
		 posix_error_description/1,
		 parse_if_modified_since/1,
		 parse_basic_authorization_header/1,
		 parse_result_cache/1,
		 parse_timeout/2,
		 parse_url_service/1,
		 parse_lang/1,
		 parse_name_service/1,
		 parse_name_querystring/1,
		 parse_type_service/1,
		 parse_type_querystring/1,
		 parse_service_service/1,
		 parse_querystring_def/1,
		 parse_file_name_path/3,
 		 parse_bool/1,
		 parse_authorization_type/1,
		 parse_bearer_authorization_header/1,
		 parse_tcp_listen_address/1,
		 parse_allowed_address_t/1,
		 parse_allowed_address/1,
		 parse_tcp_port/1,
		 parse_request_querystring/2,
		 parse_range/3,
		 parse_email/1,
		 match_ip_address/2,
 		 allow_ip_address/2,
		 mask_ipaddress_to_tuple/1,
		 encode_request_cowboy/3,
		 msg_campo_obrigatorio/2, msg_email_invalido/2, mensagens/1,
		 msg_registro_ja_existe/1, msg_registro_ja_existe/2,
		 hashsym_and_params/1,
		 hashsym_and_params/4,
		 hashsym_and_params_id/2,
		 hashsym/1,
		 make_rowid/1,
		 make_rowid/2,
		 make_rowid_id/1,
		 quote/1,
 		 remove_quoted_str/1,
		 remove_ult_backslash_url/1,
		 name_case/1,
		 modernize/1,
		 mes_abreviado/1,
		 new_rowid_service/2,
		 json_encode_table2/2,
		 utf8_list_to_string/1,
		 utf8_list_to_binary/1,
		 utf8_binary_to_list/1,
		 normalize_field_utf8/1,
		 utf8_string_win/1,
		 utf8_string_linux/1,
		 criptografia_sha1/1,
		 head_file/2,
		 replace_all_vars/2,
		 to_utf8/1,
		 load_erlang_module/1,
		 mime_type/1,
		 encode_response/3,
		 encode_response/4,
		 encode_response/2,
		 header_cache_control/1,
		 rid_to_string/1,
		 method_to_string/1,
		 decode_http_header/2,
		 decode_http_request/1,
		 tuple_to_maps_with_keys/2,
		 compile_modulo_erlang/2,
		 print_int_map/1,
		 print_str_map/1,
		 parse_user_agent/1,
		 user_agent_atom_to_binary/1,
		 to_lower_and_remove_backslash/1,
		 check_type_email/2,
		 is_email_institucional/2
		]).

-spec version() -> string().
version() ->
	case application:get_key(ems_bus, vsn) of 
		{ok, Version} -> Version;
		undefined -> "1.0.0"
	end.

-spec server_name() -> string().
server_name() ->
	io_lib:format(<<"ems-bus-~s">>, [case application:get_key(ems_bus, vsn) of 
											{ok, Version} -> Version;
											undefined -> "1.0.0"
									 end]).

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
	case P > 0 andalso P =< 999999999999999999999 of
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
make_rowid(null) -> 0;
make_rowid(undefined) -> 0;
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
-spec sleep(non_neg_integer()) -> true.
sleep(T) ->
    receive
	    after T -> true
    end.


-spec timestamp_str() -> string().
timestamp_str() -> binary_to_list(timestamp_binary()).

-spec timestamp_binary() -> binary().
timestamp_binary() ->
	Timestamp = calendar:local_time(),
	timestamp_binary(Timestamp).
	
-spec timestamp_str(tuple()) -> string().
timestamp_str({{_Ano,_Mes,_Dia},{_Hora,_Min,_Seg}} = Timestamp) ->	binary_to_list(timestamp_binary(Timestamp));
timestamp_str(_) -> "".  

-spec timestamp_binary(tuple()) -> binary().
timestamp_binary({{Ano,Mes,Dia},{Hora,Min,Seg}}) ->
	DiaBin = integer_to_binary(Dia),
	MesBin = integer_to_binary(Mes),
	AnoBin = integer_to_binary(Ano),
	HoraBin = integer_to_binary(Hora),
	MinBin = integer_to_binary(Min),
	SegBin = integer_to_binary(Seg),
	case Seg < 10 of 
		true -> 
			case Min < 10 of 
				true ->
					case Hora < 10 of
						true ->
							case Mes < 10 of
								true -> 
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":0">>, MinBin, <<":0">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":0">>, MinBin, <<":0">>, SegBin])
									end;
								false ->
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":0">>, MinBin, <<":0">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":0">>, MinBin, <<":0">>, SegBin])
									end
							end;
						false ->
							case Mes < 10 of
								true -> 
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":0">>, MinBin, <<":0">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":0">>, MinBin, <<":0">>, SegBin])
									end;
								false ->
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":0">>, MinBin, <<":0">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":0">>, MinBin, <<":0">>, SegBin])
									end
							end
					end;
				false ->
					case Hora < 10 of
						true ->
							case Mes < 10 of
								true -> 
									case Dia < 10 of
										true -> 
											iolist_to_binary([DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":">>, MinBin, <<":0">>, SegBin]);
										false ->
											iolist_to_binary([<<"0">>, DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":">>, MinBin, <<":0">>, SegBin])
									end;
								false ->
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":">>, MinBin, <<":0">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":">>, MinBin, <<":0">>, SegBin])
									end
							end;
						false ->
							case Mes < 10 of
								true -> 
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":">>, MinBin, <<":0">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":">>, MinBin, <<":0">>, SegBin])
									end;
								false ->
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":">>, MinBin, <<":0">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":">>, MinBin, <<":0">>, SegBin])
									end
							end
					end
			end;
		false ->
			case Min < 10 of 
				true ->
					case Hora < 10 of
						true ->
							case Mes < 10 of
								true -> 
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":0">>, MinBin, <<":">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":0">>, MinBin, <<":">>, SegBin])
									end;
								false ->
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":0">>, MinBin, <<":">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":0">>, MinBin, <<":">>, SegBin])
									end
							end;
						false ->
							case Mes < 10 of
								true -> 
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":0">>, MinBin, <<":">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":0">>, MinBin, <<":">>, SegBin])
									end;
								false ->
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":0">>, MinBin, <<":">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":0">>, MinBin, <<":">>, SegBin])
									end
							end
					end;
				false ->
					case Hora < 10 of
						true ->
							case Mes < 10 of
								true -> 
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":">>, MinBin, <<":">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":">>, MinBin, <<":">>, SegBin])
									end;
								false ->
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":">>, MinBin, <<":">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" 0">>, HoraBin, <<":">>, MinBin, <<":">>, SegBin])
									end
							end;
						false ->
							case Mes < 10 of
								true -> 
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":">>, MinBin, <<":">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/0">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":">>, MinBin, <<":">>, SegBin])
									end;
								false ->
									case Dia < 10 of
										true -> 
											iolist_to_binary([<<"0">>, DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":">>, MinBin, <<":">>, SegBin]);
										false ->
											iolist_to_binary([DiaBin, <<"/">>, MesBin, <<"/">>, AnoBin, <<" ">>, HoraBin, <<":">>, MinBin, <<":">>, SegBin])
									end
							end
					end
			end
	end;
timestamp_binary(_) ->  <<>>.


-spec date_to_string(tuple()) -> string().
date_to_string({{Ano,Mes,Dia},{_Hora,_Min,_Seg}}) ->
    lists:flatten(io_lib:format("~2..0B/~2..0B/~4..0B", [Dia, Mes, Ano]));
date_to_string(_) -> "".    

-spec date_to_binary(tuple()) -> binary().
date_to_binary({{Ano,Mes,Dia},{_Hora,_Min,_Seg}}) ->
    iolist_to_binary(io_lib:format("~2..0B/~2..0B/~4..0B", [Dia, Mes, Ano]));
date_to_binary(_) -> <<>>.
    


tuple_to_binlist(T) ->
	L = tuple_to_list(T),
	list_to_binlist(L).

list_to_binlist([]) -> [];
list_to_binlist(<<>>) -> [];
list_to_binlist(<<V/binary>>) -> [V];
list_to_binlist(Value) -> list_to_binlist(Value, []).
	
list_to_binlist([], Result) -> lists:reverse(Result);	
list_to_binlist([H|T], Result) -> 	
	list_to_binlist(T, [item_to_binary(H) | Result]).

binlist_to_list(<<>>) -> [];
binlist_to_list([]) -> [];
binlist_to_list(Value) -> binlist_to_list(Value, []).

binlist_to_list([], Result) -> Result;
binlist_to_list([H|T], Result) ->
	binlist_to_list(T, [binary_to_list(H)|Result]).


join_binlist([], _) -> "";
join_binlist(BinList, Str) -> string:join(binlist_to_list(BinList), Str).


item_to_binary(undefined) -> undefined;
item_to_binary(null) -> undefined;
item_to_binary([]) -> <<>>;
item_to_binary(<<I/binary>>) -> I;
item_to_binary(T) when is_tuple(T) -> 
	tuple_to_binlist(T);
item_to_binary(L) when is_list(L) -> 
	case io_lib:printable_list(L) of
		true -> 
			L2 = [case Ch of 
					34 -> "\\\""; 
					_ -> Ch 
				  end || Ch <- L],
			iolist_to_binary(L2);
		false -> list_to_binlist(L)
	end;
item_to_binary(I) when is_integer(I) -> I;
item_to_binary(I) when is_float(I) -> I;
item_to_binary(I) when is_atom(I) -> 
	[I2] = io_lib:format("~p", [I]),
	iolist_to_binary(I2);
item_to_binary(I) when is_map(I) -> I;
item_to_binary(I) ->
	iolist_to_binary(I).
	
	

%% @doc Converte dados Erlang para JSON
json_encode([]) -> <<"null">>;
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


json_decode_as_map_file(Filename) ->
	case file:read_file(Filename) of
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
-spec hd_or_empty(list()) -> any().
hd_or_empty(List) when length(List) > 0 -> 
	hd(List);
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
	% Fórmula anterior:
		% {Mega, Sec, Micro} = erlang:timestamp(),
		% (Mega*1000000 + Sec)*1000 + round(Micro/1000).
	trunc(erlang:system_time() / 1.0e6).
	
%% @doc Remove o último backslash da Url
-spec remove_ult_backslash_url(string()) -> string().
remove_ult_backslash_url("/") -> "/";
remove_ult_backslash_url(Value) ->
	case lists:reverse(Value) of
		"/" ++ T -> lists:reverse(T);
		_ -> Value
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
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Timestamp) + (Minutes * 60)).

date_dec_minute(Timestamp, Minutes) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Timestamp) - (Minutes * 60)).

date_add_second(Timestamp, Seconds) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Timestamp) + Seconds).

date_dec_second(Timestamp, Seconds) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Timestamp) - Seconds).

date_add_day(Timestamp, Days) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Timestamp) + (Days * 86400)).

% Return a encrypted password in binary format        
criptografia_sha1(<<>>) -> <<>>;
criptografia_sha1("") -> <<>>;	
criptografia_sha1(undefined) -> <<>>;
criptografia_sha1(null) -> <<>>;
criptografia_sha1(Password) when is_binary(Password) ->	criptografia_sha1(binary_to_list(Password));
criptografia_sha1(Password) -> base64:encode(sha1:binstring(Password)).

boolean_to_binary(true) -> <<"true"/utf8>>;
boolean_to_binary(1) -> <<"true"/utf8>>;
boolean_to_binary(<<"true"/utf8>>) -> <<"true"/utf8>>;
boolean_to_binary(<<"1"/utf8>>) -> <<"true"/utf8>>;
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
		case check_encoding_bin(Text) of
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
		case check_encoding_bin(Text) of
			utf8 -> normalize_field_utf8(Text);
			latin1 -> normalize_field_utf8(Text);
			Other -> Other
		end
	catch
		_Exception:Reason -> 
			?DEBUG("utf8_string_linux convert ~p error: ~p\n", [Text, Reason]),
			Text
	end.
	

-spec read_file_as_map(Filename :: string()) -> map().
read_file_as_map(Filename) -> 	
	case file:read_file(Filename) of
		{ok, Arq} -> json_decode_as_map(Arq);
		Error -> Error
	end.

-spec read_file_as_list(string()) -> list().
read_file_as_list(Filename) ->
  {ok, IO} = file:open( Filename, [read] ),
  read_file_as_list( io:get_line(IO, ''), IO, [] ).

read_file_as_list( eof, _IO, Acc ) -> lists:reverse( Acc );
read_file_as_list( {error, _Error}, _IO, Acc ) -> lists:reverse( Acc );
read_file_as_list( Line, IO, Acc ) -> read_file_as_list( io:get_line(IO, ''), IO, [Line | Acc] ).


-spec head_file(string(), non_neg_integer()) -> list().
head_file(Filename, N) ->
	L = read_file_as_list(Filename),
	{ok, lists:sublist(L, N)}.

-spec tail_file(string(), non_neg_integer()) -> list().
tail_file(Filename, N) ->
	L = read_file_as_list(Filename),
	Len = length(L),
	case Len > N of 	
		true ->	{ok, lists:nthtail(Len-N, L)};
		false -> {ok, L}
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


% Process the path "~" and "." wildcards and variable path. Return path
-spec parse_file_name_path(string() | binary(), list(tuple()) | undefined, binary() | undefined) -> string().
parse_file_name_path(undefined, _, _) -> <<>>;
parse_file_name_path(<<>>, _, _) -> <<>>;
parse_file_name_path(Path, StaticFilePathList, RootPath) when is_binary(Path) ->
	parse_file_name_path(binary_to_list(Path), StaticFilePathList, RootPath);
parse_file_name_path(Path, StaticFilePathList, RootPath) ->
	Ch = string:substr(Path, 1, 1),
	Ch2 = string:substr(Path, 2, 1),
	case Ch =:= "/" orelse (is_letter(Ch) andalso Ch2 =:= ":")   of
		true -> remove_ult_backslash_url(Path);  
		false ->
			case Ch == "~" of
				true -> 
					case init:get_argument(home) of
						{ok, [[HomePath]]} -> replace(Path, "~", HomePath);
						{error, Reason} -> erlang:error(Reason)
					end;
				_ -> 
					case Ch == "." of
						true -> 
							case RootPath of
								undefined -> remove_ult_backslash_url(string:substr(Path, 3));
								_ -> remove_ult_backslash_url(remove_ult_backslash_url(RootPath) ++ "/" ++ string:substr(Path, 3))
							end;
						false -> 
							Path2 = replace_all_vars(Path, StaticFilePathList),
							% after process variables, check ~ or . wildcards
							case string:substr(Path2, 1, 1) == "~" of
								true -> 
									case init:get_argument(home) of
										{ok, [[HomePath]]} -> replace(Path2, "~", HomePath);
										{error, Reason} -> erlang:error(Reason)
									end;
								_ -> 
									case Ch == "." of
										true -> 
											case RootPath of
												undefined -> remove_ult_backslash_url(string:substr(Path2, 3));
												_ -> remove_ult_backslash_url(remove_ult_backslash_url(RootPath) ++ "/" ++ string:substr(Path2, 3))
											end;
										false ->  
											case RootPath of
												undefined -> remove_ult_backslash_url(Path2);
												<<>> -> remove_ult_backslash_url(Path2);
												_ -> remove_ult_backslash_url(remove_ult_backslash_url(RootPath) ++ "/" ++ Path2)
											end
									end
							end
					end
			end
	end.


read_file_as_string(Filename) -> 	
	case file:read_file(Filename) of
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

-spec file_last_modified(string()) -> tuple().
file_last_modified(FilePath) ->
	case file:read_file_info(FilePath, [{time, universal}]) of
		{ok, {file_info, _FSize, _Type, _Access, _ATime, MTime, _CTime, _Mode,_,_,_,_,_,_}} -> MTime;
		Error -> Error
	end.
		

%% Converte arquivo latin1 para utf8 formatando os unicodes
%% Esta função está desconfigurando os arquivos no formato utf8	
to_utf8(Filename) ->
	try
		{ok, File} = file:open(Filename, [read,binary]),
		Size = filelib:file_size(Filename),
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


load_erlang_module(Filename) ->
	ModuleName = filename:rootname(filename:basename(Filename)),
	ModuleNameAtom = list_to_atom(ModuleName),
	FilenameMod = filename:rootname(Filename) ++ ".erl",
	case filelib:file_size(FilenameMod) > 0 of
		true ->
			case code:ensure_loaded(ModuleNameAtom) of
				{module, _} -> {ok, ModuleNameAtom};
				_Error -> 
					FilenamePath = filename:dirname(Filename), 
					code:add_path(FilenamePath), 
					case compile:file(FilenameMod, [{outdir, FilenamePath ++ "/"}]) of
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


replacenth(Index,Value,List) ->
 replacenth(Index-1,Value,List,[],0).

replacenth(ReplaceIndex,Value,[_|List],Acc,ReplaceIndex) ->
 lists:reverse(Acc)++[Value|List];
replacenth(ReplaceIndex,Value,[V|List],Acc,Index) ->
 replacenth(ReplaceIndex,Value,List,[V|Acc],Index+1).


-spec ip_list() -> {ok, list(tuple())} | {error, atom()}.
ip_list()->
	 case inet:getifaddrs() of
		{ok, List} ->
			CheckIfUpFunc = fun(P) ->
				{flags, Flags} = lists:keyfind(flags, 1, P),
				lists:member(running, Flags) andalso lists:member(up, Flags)
			end,
			List2 = [ lists:keyfind(addr, 1, P) || {_, P} <- List, CheckIfUpFunc(P) ],
			List3 = [ element(2, X) || X <- List2, is_tuple(X) ],
			List4 = [ X || X <- List3, tuple_size(X) == 4 ],
			{ok, List4};
		Error -> Error
	end.
	 

-spec parse_bool(binary() | string() | boolean() | integer()) -> boolean().
parse_bool(<<"true">>) -> true;
parse_bool("true") -> true;
parse_bool(true) -> true;
parse_bool(1) -> true;
parse_bool(<<"1">>) -> true;
parse_bool(_) -> false.


-spec parse_service_service(binary() | string()) -> {string(), string(), string()}.
parse_service_service(Service) when is_binary(Service) ->
	parse_service_service(binary_to_list(Service));
parse_service_service(Service) ->
	try
		[ModuleName, FunctionName] = string:split(Service, ":"),
		ModuleName2 = ModuleName,
		FunctionName2 = FunctionName,
		ModuleNameCanonical = lists:last(string:tokens(ModuleName2, ".")),
		{ModuleName2, ModuleNameCanonical, FunctionName2}
	catch
		_Exception:_Reason ->  erlang:error(einvalid_service_service)
	end.

	 
%% @doc Translates the code into a more useful description
-spec posix_error_description(atom()) -> string().
posix_error_description(e2big) -> "e2big - argument list too long";
posix_error_description(eacces) -> "eacces - permission denied";
posix_error_description(eaddrinuse) -> "eaddrinuse - address already in use";
posix_error_description(eaddrnotavail) -> "eaddrnotavail - cannot assign requested address";
posix_error_description(eadv) -> "eadv - advertise error";
posix_error_description(eafnosupport) -> "eafnosupport - address family not supported by protocol family";
posix_error_description(eagain) -> "eagain - resource temporarily unavailable";
posix_error_description(ealign) -> "ealign - EALIGN";
posix_error_description(ealready) -> "ealready - operation already in progress";
posix_error_description(ebade) -> "ebade - bad exchange descriptor";
posix_error_description(ebadf) -> "ebadf - bad file number";
posix_error_description(ebadfd) -> "ebadfd - file descriptor in bad state";
posix_error_description(ebadmsg) -> "ebadmsg - not a data message";
posix_error_description(ebadr) -> "ebadr - bad request descriptor";
posix_error_description(ebadrpc) -> "ebadrpc - RPC structure is bad";
posix_error_description(ebadrqc) -> "ebadrqc - bad request code";
posix_error_description(ebadslt) -> "ebadslt - invalid slot";
posix_error_description(ebfont) -> "ebfont - bad font file format";
posix_error_description(ebusy) -> "ebusy - file busy";
posix_error_description(echild) -> "echild - no children";
posix_error_description(echrng) -> "echrng - channel number out of range";
posix_error_description(ecomm) -> "ecomm - communication error on send";
posix_error_description(econnaborted) -> "econnaborted - software caused connection abort";
posix_error_description(econnrefused) -> "econnrefused - connection refused";
posix_error_description(econnreset) -> "econnreset - connection reset by peer";
posix_error_description(edeadlk) -> "edeadlk - resource deadlock avoided";
posix_error_description(edeadlock) -> "edeadlock - resource deadlock avoided";
posix_error_description(edestaddrreq) -> "edestaddrreq - destination address required";
posix_error_description(edirty) -> "edirty - mounting a dirty fs w/o force";
posix_error_description(edom) -> "edom - math argument out of range";
posix_error_description(edotdot) -> "edotdot - cross mount point";
posix_error_description(edquot) -> "edquot - disk quota exceeded";
posix_error_description(eduppkg) -> "eduppkg - duplicate package name";
posix_error_description(eexist) -> "eexist - file already exists";
posix_error_description(efault) -> "efault - bad address in system call argument";
posix_error_description(efbig) -> "efbig - file too large";
posix_error_description(ehostdown) -> "ehostdown - host is down";
posix_error_description(ehostunreach) -> "ehostunreach - host is unreachable";
posix_error_description(eidrm) -> "eidrm - identifier removed";
posix_error_description(einit) -> "einit - initialization error";
posix_error_description(einprogress) -> "einprogress - operation now in progress";
posix_error_description(eintr) -> "eintr - interrupted system call";
posix_error_description(einval) -> "einval - invalid argument";
posix_error_description(eio) -> "eio - I/O error";
posix_error_description(eisconn) -> "eisconn - socket is already connected";
posix_error_description(eisdir) -> "eisdir - illegal operation on a directory";
posix_error_description(eisnam) -> "eisnam - is a named file";
posix_error_description(el2hlt) -> "el2hlt - level 2 halted";
posix_error_description(el2nsync) -> "el2nsync - level 2 not synchronized";
posix_error_description(el3hlt) -> "el3hlt - level 3 halted";
posix_error_description(el3rst) -> "el3rst - level 3 reset";
posix_error_description(elbin) -> "elbin - ELBIN";
posix_error_description(elibacc) -> "elibacc - cannot access a needed shared library";
posix_error_description(elibbad) -> "elibbad - accessing a corrupted shared library";
posix_error_description(elibexec) -> "elibexec - cannot exec a shared library directly";
posix_error_description(elibmax) -> "elibmax - attempting to link in more shared libraries than system limit";
posix_error_description(elibscn) -> "elibscn - .lib section in a.out corrupted";
posix_error_description(elnrng) -> "elnrng - link number out of range";
posix_error_description(eloop) -> "eloop - too many levels of symbolic links";
posix_error_description(emfile) -> "emfile - too many open files";
posix_error_description(emlink) -> "emlink - too many links";
posix_error_description(emsgsize) -> "emsgsize - message too long";
posix_error_description(emultihop) -> "emultihop - multihop attempted";
posix_error_description(enametoolong) -> "enametoolong - file name too long";
posix_error_description(enavail) -> "enavail - not available";
posix_error_description(enet) -> "enet - ENET";
posix_error_description(enetdown) -> "enetdown - network is down";
posix_error_description(enetreset) -> "enetreset - network dropped connection on reset";
posix_error_description(enetunreach) -> "enetunreach - network is unreachable";
posix_error_description(enfile) -> "enfile - file table overflow";
posix_error_description(enoano) -> "enoano - anode table overflow";
posix_error_description(enobufs) -> "enobufs - no buffer space available";
posix_error_description(enocsi) -> "enocsi - no CSI structure available";
posix_error_description(enodata) -> "enodata - no data available";
posix_error_description(enodev) -> "enodev - no such device";
posix_error_description(enoent) -> "enoent - no such file or directory";
posix_error_description(enoexec) -> "enoexec - exec format error";
posix_error_description(enolck) -> "enolck - no locks available";
posix_error_description(enolink) -> "enolink - link has be severed";
posix_error_description(enomem) -> "enomem - not enough memory";
posix_error_description(enomsg) -> "enomsg - no message of desired type";
posix_error_description(enonet) -> "enonet - machine is not on the network";
posix_error_description(enopkg) -> "enopkg - package not installed";
posix_error_description(enoprotoopt) -> "enoprotoopt - bad protocol option";
posix_error_description(enospc) -> "enospc - no space left on device";
posix_error_description(enosr) -> "enosr - out of stream resources or not a stream device";
posix_error_description(enosym) -> "enosym - unresolved symbol name";
posix_error_description(enosys) -> "enosys - function not implemented";
posix_error_description(enotblk) -> "enotblk - block device required";
posix_error_description(enotconn) -> "enotconn - socket is not connected";
posix_error_description(enotdir) -> "enotdir - not a directory";
posix_error_description(enotempty) -> "enotempty - directory not empty";
posix_error_description(enotnam) -> "enotnam - not a named file";
posix_error_description(Code) -> atom_to_list(Code).	 
	 

-spec allow_ip_address(tuple(), atom() | tuple()) -> boolean().
allow_ip_address(_, all) -> true;
allow_ip_address({127, 0, _,_}, _) -> true;
allow_ip_address(Ip, AllowedAddress) -> match_ip_address(AllowedAddress, Ip).


%% @doc Retorna o mime-type do arquivo
-spec mime_type(string()) -> string().
mime_type(".htm") -> <<"text/html">>;
mime_type(".html") -> <<"text/html">>;
mime_type(".xhtml") -> <<"application/xhtml+xml">>;
mime_type(".css") -> <<"text/css">>;
mime_type(".js") -> <<"application/x-javascript">>;
mime_type(".png") -> <<"image/png">>;
mime_type(".xml") -> <<"application/xml">>;
mime_type(".ico") -> <<"image/x-icon">>;
mime_type(".gif") -> <<"image/gif">>;
mime_type(".jpeg") -> <<"image/jpeg">>;
mime_type(".jpg") -> <<"image/jpeg">>;
mime_type(".pdf") -> <<"application/pdf">>;
mime_type(".bmp") -> <<"image/bmp">>;
mime_type(".txt") -> <<"text/plain">>;
mime_type(".ttf") -> <<"application/font-woff">>;
mime_type(".stl") -> <<"application/SLA">>;
mime_type(".stp") -> <<"application/STEP">>;
mime_type(".step") -> <<"application/STEP">>;
mime_type(".dwg") -> <<"application/acad">>;
mime_type(".ez") -> <<"application/andrew-inset">>;
mime_type(".ccad") -> <<"application/clariscad">>;
mime_type(".drw") -> <<"application/drafting">>;
mime_type(".tsp") -> <<"application/dsptype">>;
mime_type(".dxf") -> <<"application/dxf">>;
mime_type(".xls") -> <<"application/excel">>;
mime_type(".csv") -> <<"text/csv">>;
mime_type(".unv") -> <<"application/i-deas">>;
mime_type(".jar") -> <<"application/java-archive">>;
mime_type(".hqx") -> <<"application/mac-binhex40">>;
mime_type(".cpt") -> <<"application/mac-compactpro">>;
mime_type(".pot") -> <<"application/vnd.ms-powerpoint">>;
mime_type(".ppt") -> <<"application/vnd.ms-powerpoint">>;
mime_type(".dms") -> <<"application/octet-stream">>;
mime_type(".lha") -> <<"application/octet-stream">>;
mime_type(".lzh") -> <<"application/octet-stream">>;
mime_type(".oda") -> <<"application/oda">>;
mime_type(".ogg") -> <<"application/ogg">>;
mime_type(".ogm") -> <<"application/ogg">>;
mime_type(".pgp") -> <<"application/pgp">>;
mime_type(".ai") -> <<"application/postscript">>;
mime_type(".eps") -> <<"application/postscript">>;
mime_type(".ps") -> <<"application/postscript">>;
mime_type(".prt") -> <<"application/pro_eng">>;
mime_type(".rtf") -> <<"application/rtf">>;
mime_type(".smi") -> <<"application/smil">>;
mime_type(".smil") -> <<"application/smil">>;
mime_type(".sol") -> <<"application/solids">>;
mime_type(".vda") -> <<"application/vda">>;
mime_type(".xlm") -> <<"application/vnd.ms-excel">>;
mime_type(".cod") -> <<"application/vnd.rim.cod">>;
mime_type(".pgn") -> <<"application/x-chess-pgn">>;
mime_type(".cpio") -> <<"application/x-cpio">>;
mime_type(".csh") -> <<"application/x-csh">>;
mime_type(".deb") -> <<"application/x-debian-package">>;
mime_type(".dcr") -> <<"application/x-director">>;
mime_type(".dir") -> <<"application/x-director">>;
mime_type(".dxr") -> <<"application/x-director">>;
mime_type(".gz") -> <<"application/x-gzip">>;
mime_type(".hdf") -> <<"application/x-hdf">>;
mime_type(".ipx") -> <<"application/x-ipix">>;
mime_type(".ips") -> <<"application/x-ipscript">>;
mime_type(".skd") -> <<"application/x-koan">>;
mime_type(".skm") -> <<"application/x-koan">>;
mime_type(".skp") -> <<"application/x-koan">>;
mime_type(".skt") -> <<"application/x-koan">>;
mime_type(".latex") -> <<"application/x-latex">>;
mime_type(".lsp") -> <<"application/x-lisp">>;
mime_type(".scm") -> <<"application/x-lotusscreencam">>;
mime_type(".mif") -> <<"application/x-mif">>;
mime_type(".com") -> <<"application/x-msdos-program">>;
mime_type(".exe") -> <<"application/octet-stream">>;
mime_type(".cdf") -> <<"application/x-netcdf">>;
mime_type(".nc") -> <<"application/x-netcdf">>;
mime_type(".pl") -> <<"application/x-perl">>;
mime_type(".pm") -> <<"application/x-perl">>;
mime_type(".rar") -> <<"application/x-rar-compressed">>;
mime_type(".sh") -> <<"application/x-sh">>;
mime_type(".shar") -> <<"application/x-shar">>;
mime_type(".swf") -> <<"application/x-shockwave-flash">>;
mime_type(".sit") -> <<"application/x-stuffit">>;
mime_type(".sv4cpio") -> <<"application/x-sv4cpio">>;
mime_type(".sv4crc") -> <<"application/x-sv4crc">>;
mime_type(".tar.gz") -> <<"application/x-tar-gz">>;
mime_type(".tgz") -> <<"application/x-tar-gz">>;
mime_type(".tar") -> <<"application/x-tar">>;
mime_type(".tcl") -> <<"application/x-tcl">>;
mime_type(".texi") -> <<"application/x-texinfo">>;
mime_type(".texinfo") -> <<"application/x-texinfo">>;
mime_type(".man") -> <<"application/x-troff-man">>;
mime_type(".me") -> <<"application/x-troff-me">>;
mime_type(".ms") -> <<"application/x-troff-ms">>;
mime_type(".roff") -> <<"application/x-troff">>;
mime_type(".t") -> <<"application/x-troff">>;
mime_type(".tr") -> <<"application/x-troff">>;
mime_type(".ustar") -> <<"application/x-ustar">>;
mime_type(".src") -> <<"application/x-wais-source">>;
mime_type(".zip") -> <<"application/zip">>;
mime_type(".tsi") -> <<"audio/TSP-audio">>;
mime_type(".au") -> <<"audio/basic">>;
mime_type(".snd") -> <<"audio/basic">>;
mime_type(".kar") -> <<"audio/midi">>;
mime_type(".mid") -> <<"audio/midi">>;
mime_type(".midi") -> <<"audio/midi">>;
mime_type(".mp2") -> <<"audio/mpeg">>;
mime_type(".mp3") -> <<"audio/mpeg">>;
mime_type(".mpga") -> <<"audio/mpeg">>;
mime_type(".aif") -> <<"audio/x-aiff">>;
mime_type(".aifc") -> <<"audio/x-aiff">>;
mime_type(".aiff") -> <<"audio/x-aiff">>;
mime_type(".m3u") -> <<"audio/x-mpegurl">>;
mime_type(".wax") -> <<"audio/x-ms-wax">>;
mime_type(".wma") -> <<"audio/x-ms-wma">>;
mime_type(".rpm") -> <<"audio/x-pn-realaudio-plugin">>;
mime_type(".ram") -> <<"audio/x-pn-realaudio">>;
mime_type(".rm") -> <<"audio/x-pn-realaudio">>;
mime_type(".ra") -> <<"audio/x-realaudio">>;
mime_type(".wav") -> <<"audio/x-wav">>;
mime_type(".pdb") -> <<"chemical/x-pdb">>;
mime_type(".ras") -> <<"image/cmu-raster">>;
mime_type(".ief") -> <<"image/ief">>;
mime_type(".jpe") -> <<"image/jpeg">>;
mime_type(".jp2") -> <<"image/jp2">>;
mime_type(".tif") -> <<"image/tiff">>;
mime_type(".tiff") -> <<"image/tiff">>;
mime_type(".pnm") -> <<"image/x-portable-anymap">>;
mime_type(".pbm") -> <<"image/x-portable-bitmap">>;
mime_type(".pgm") -> <<"image/x-portable-graymap">>;
mime_type(".ppm") -> <<"image/x-portable-pixmap">>;
mime_type(".rgb") -> <<"image/x-rgb">>;
mime_type(".xbm") -> <<"image/x-xbitmap">>;
mime_type(".xwd") -> <<"image/x-xwindowdump">>;
mime_type(".iges") -> <<"model/iges">>;
mime_type(".igs") -> <<"model/iges">>;
mime_type(".mesh") -> <<"model/mesh">>;
mime_type(".msh") -> <<"model/mesh">>;
mime_type(".silo") -> <<"model/mesh">>;
mime_type(".vrml") -> <<"model/vrml">>;
mime_type(".wrl") -> <<"model/vrml">>;
mime_type(".asc") -> <<"text/plain">>;
mime_type(".c") -> <<"text/plain">>;
mime_type(".cc") -> <<"text/plain">>;
mime_type(".f90") -> <<"text/plain">>;
mime_type(".f") -> <<"text/plain">>;
mime_type(".hh") -> <<"text/plain">>;
mime_type(".m") -> <<"text/plain">>;
mime_type(".rtx") -> <<"text/richtext">>;
mime_type(".sgm") -> <<"text/sgml">>;
mime_type(".sgml") -> <<"text/sgml">>;
mime_type(".tsv") -> <<"text/tab-separated-values">>;
mime_type(".jad") -> <<"text/vnd.sun.j2me.app-descriptor">>;
mime_type(".etx") -> <<"text/x-setext">>;
mime_type(".dl") -> <<"video/dl">>;
mime_type(".fli") -> <<"video/fli">>;
mime_type(".flv") -> <<"video/flv">>;
mime_type(".gl") -> <<"video/gl">>;
mime_type(".mp4") -> <<"video/mp4">>;
mime_type(".mpe") -> <<"video/mpeg">>;
mime_type(".mpeg") -> <<"video/mpeg">>;
mime_type(".mpg") -> <<"video/mpeg">>;
mime_type(".mov") -> <<"video/quicktime">>;
mime_type(".qt") -> <<"video/quicktime">>;
mime_type(".viv") -> <<"video/vnd.vivo">>;
mime_type(".vivo") -> <<"video/vnd.vivo">>;
mime_type(".asf") -> <<"video/x-ms-asf">>;
mime_type(".asx") -> <<"video/x-ms-asx">>;
mime_type(".wmv") -> <<"video/x-ms-wmv">>;
mime_type(".wmx") -> <<"video/x-ms-wmx">>;
mime_type(".wvx") -> <<"video/x-ms-wvx">>;
mime_type(".avi") -> <<"video/x-msvideo">>;
mime_type(".movie") -> <<"video/x-sgi-movie">>;
mime_type(".mime") -> <<"www/mime">>;
mime_type(".ice") -> <<"x-conference/x-cooltalk">>;
mime_type(".vrm") -> <<"x-world/x-vrml">>;
mime_type(".spx") -> <<"audio/ogg">>;
mime_type(".bz2") -> <<"application/x-bzip2">>;
mime_type(".doc") -> <<"application/msword">>;
mime_type(".z") -> <<"application/x-compress">>;
mime_type(".m4a") -> <<"audio/mpeg">>;
mime_type(_) -> <<"application/octet-stream">>.


-spec encode_request_cowboy(tuple(), pid(), map()) -> {ok, #request{}} | {error, atom()}.
encode_request_cowboy(CowboyReq, WorkerSend, HttpHeaderDefault) ->
	try
		Url = cowboy_req:path(CowboyReq),
		Url2 = remove_ult_backslash_url(binary_to_list(Url)),
		Uri = iolist_to_binary(cowboy_req:uri(CowboyReq)),
		RID = erlang:system_time(),
		Timestamp = calendar:local_time(),
		T1 = trunc(RID / 1.0e6), % optimized: same that get_milliseconds()
		Type = cowboy_req:method(CowboyReq),
		{Ip, _} = cowboy_req:peer(CowboyReq),
		IpBin = list_to_binary(inet_parse:ntoa(Ip)),
		Host = cowboy_req:host(CowboyReq),
		Version = cowboy_req:version(CowboyReq),
		case cowboy_req:header(<<"content-type">>, CowboyReq) of
			undefined -> ContentType = <<>>;
			MimeType -> ContentType = MimeType
		end,
		QuerystringBin = cowboy_req:qs(CowboyReq),
		ProtocolBin = cowboy_req:scheme(CowboyReq),
		Protocol = parse_protocol(ProtocolBin),
		Port = cowboy_req:port(CowboyReq),
		case QuerystringBin of
			<<>> -> QuerystringMap0 = #{};
			_ -> QuerystringMap0 = parse_querystring([binary_to_list(QuerystringBin)])
		end,
		case cowboy_req:header(<<"accept">>, CowboyReq) of
			undefined -> Accept = <<"*/*">>;
			AcceptValue -> Accept = AcceptValue
		end,
		case cowboy_req:header(<<"accept-encoding">>, CowboyReq) of
			undefined -> Accept_Encoding = <<"*">>;
			AcceptEncodingValue -> Accept_Encoding = AcceptEncodingValue
		end,
		{UserAgent, UserAgentVersion} = parse_user_agent(cowboy_req:header(<<"user-agent">>, CowboyReq)),
		case cowboy_req:header(<<"cache-control">>, CowboyReq) of
			undefined -> Cache_Control = <<>>;
			CacheControlValue -> Cache_Control = CacheControlValue
		end,
		Authorization = cowboy_req:header(<<"authorization">>, CowboyReq),
		IfModifiedSince = cowboy_req:header(<<"if-modified-since">>, CowboyReq),
		IfNoneMatch = cowboy_req:header(<<"if-none-match">>, CowboyReq),
		Referer = cowboy_req:header(<<"referer">>, CowboyReq),
		{Rowid, Params_url} = hashsym_and_params(Url2),
		TypeLookup = case Type of
					<<"OPTIONS">> -> 
						ems_db:inc_counter(ems_dispatcher_options),
						<<"GET">>;
					<<"HEAD">> -> 
						ems_db:inc_counter(ems_dispatcher_head),
						<<"GET">>;
					<<"GET">> -> 
						ems_db:inc_counter(ems_dispatcher_get),
						<<"GET">>;
					<<"POST">> -> 
						ems_db:inc_counter(ems_dispatcher_post),
						<<"POST">>;
					<<"PUT">> -> 
						ems_db:inc_counter(ems_dispatcher_put),
						<<"PUT">>;
					<<"DELETE">> -> 
						ems_db:inc_counter(ems_dispatcher_delete),
						<<"DELETE">>;
					_ ->
						ems_db:inc_counter(ehttp_verb_not_supported),
						erlang:error(ehttp_verb_not_supported)
			   end,
		Request = #request{
			rid = RID,
			rowid = Rowid,
			type = TypeLookup,
			uri = Uri,
			url = Url2,
			version = Version,
			querystring = QuerystringBin,
			querystring_map = QuerystringMap0,
			params_url = Params_url,
			accept = Accept,
			user_agent = UserAgent,
			user_agent_version = UserAgentVersion,
			accept_encoding = Accept_Encoding,
			cache_control = Cache_Control,
			ip = Ip,
			ip_bin = IpBin,
			host = Host,
			timestamp = Timestamp,
			authorization = Authorization,
			worker_send = WorkerSend,
			if_modified_since = IfModifiedSince,
			if_none_match = IfNoneMatch,
			protocol = Protocol,
			protocol_bin = ProtocolBin,
			port = Port,
			result_cache = false,
			t1 = T1,
			referer = Referer
		},	
		case ems_catalog_lookup:lookup(Request) of
			{Service = #service{http_max_content_length = HttpMaxContentLengthService}, ParamsMap, QuerystringMap} -> 
				case cowboy_req:body_length(CowboyReq) of
					undefined -> ContentLength = 0; %% The value returned will be undefined if the length couldn't be figured out from the request headers. 
					ContentLengthValue -> ContentLength = ContentLengthValue
				end,
				case ContentLength > 0 of
					true ->
						case ContentLength > HttpMaxContentLengthService of
							true ->	erlang:error(ehttp_max_content_length_error);
							false -> ok
						end,
						case ContentType of
							<<"application/json">> ->
								ems_db:inc_counter(http_content_type_in_application_json),
								ContentType2 = <<"application/json">>,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = decode_payload_as_json(Payload),
								QuerystringMap2 = QuerystringMap;
							<<"application/json; charset=utf-8">> ->
								ems_db:inc_counter(http_content_type_in_application_json),
								ContentType2 = <<"application/json">>,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = decode_payload_as_json(Payload),
								QuerystringMap2 = QuerystringMap;
							<<"application/json;charset=utf-8">> -> 
								ems_db:inc_counter(http_content_type_in_application_json),
								ContentType2 = <<"application/json">>,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = decode_payload_as_json(Payload),
								QuerystringMap2 = QuerystringMap;
							<<"application/x-www-form-urlencoded">> ->
								ems_db:inc_counter(http_content_type_in_form_urlencode),
								ContentType2 = <<"application/x-www-form-urlencoded; charset=UTF-8">>,
								{ok, Payload, CowboyReq2} = cowboy_req:read_urlencoded_body(CowboyReq),
								PayloadMap = maps:from_list(Payload),
								QuerystringMap2 = maps:merge(QuerystringMap, PayloadMap);
							<<"application/x-www-form-urlencoded; charset=UTF-8">> ->
								ems_db:inc_counter(http_content_type_in_form_urlencode),
								ContentType2 = <<"application/x-www-form-urlencoded; charset=UTF-8">>,
								{ok, Payload, CowboyReq2} = cowboy_req:read_urlencoded_body(CowboyReq),
								PayloadMap = maps:from_list(Payload),
								QuerystringMap2 = maps:merge(QuerystringMap, PayloadMap);
							<<"application/xml">> ->
								ems_db:inc_counter(http_content_type_in_application_xml),
								ContentType2 = <<"application/xml">>,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = decode_payload_as_xml(Payload),
								QuerystringMap2 = QuerystringMap;
							<<"text/plain">> ->
								ems_db:inc_counter(http_content_type_in_text_plain),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap;
							<<"text/csv">> ->
								ems_db:inc_counter(http_content_type_in_text_csv),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap;
							<<"application/octet-stream">> ->
								ems_db:inc_counter(http_content_type_in_octet_stream),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap;
							<<"application/gzip">> ->
								ems_db:inc_counter(http_content_type_in_application_gzip),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap;
							<<"application/pdf">> ->
								ems_db:inc_counter(http_content_type_in_application_pdf),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap;
							<<"application/msword">> ->
								ems_db:inc_counter(http_content_type_in_officedocument),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap;
							<<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">> ->
								ems_db:inc_counter(http_content_type_in_officedocument),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap;
							<<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">> ->
								ems_db:inc_counter(http_content_type_in_officedocument),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap;
							<<"image/png">> ->
								ems_db:inc_counter(http_content_type_in_image_png),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap;
							<<"image/jpeg">> ->
								ems_db:inc_counter(http_content_type_in_image_jpeg),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap;
							_ -> 
								ems_db:inc_counter(http_content_type_in_other),
								ContentType2 = ContentType,
								{ok, Payload, CowboyReq2} = cowboy_req:read_body(CowboyReq),
								PayloadMap = undefined,
								QuerystringMap2 = QuerystringMap
						end;
					false ->
						ContentType2 = ContentType,						
						Payload = <<>>,
						PayloadMap = undefined,
						QuerystringMap2 = QuerystringMap,
						CowboyReq2 = CowboyReq
				end,
				ReqHash = erlang:phash2([Url, QuerystringBin, ContentLength, ContentType2]),
				Request2 = Request#request{
					type = Type,
					querystring_map = QuerystringMap2,
					content_type_in = ContentType2,
					content_type = ContentType2,
					content_length = ContentLength,
					payload = Payload, 
					payload_map = PayloadMap,
					params_url = ParamsMap,
					req_hash = ReqHash
				},	
				{ok, Request2, Service, CowboyReq2};
			Error2 -> 
				if 
					Type =:= <<"OPTIONS">> orelse Type =:= "HEAD" ->
							{ok, request, Request#request{code = 200, 
														  reason = ok, 
														  response_header = HttpHeaderDefault,
														  latency = ems_util:get_milliseconds() - T1}
							};
					true ->
						ems_db:inc_counter(ems_dispatcher_lookup_enoent),								
						Error2
				end			
		end
	catch
		_Exception:Reason -> 
			ems_db:inc_counter(Reason),
			ems_logger:error("ems_util invalid http request ~p. Reason: ~p.", [CowboyReq, Reason]),
			{error, Reason}
	end.


parse_protocol(<<"http">>) -> http;
parse_protocol(<<"https">>) -> https;
parse_protocol(_) -> erlang:error(einvalid_protocol).


-spec parse_if_modified_since(binary() | undefined) -> calendar:datetime().
parse_if_modified_since(undefined) -> undefined;
parse_if_modified_since(IfModifiedSince) -> cow_date:parse_date(IfModifiedSince).


%% @doc Gera o response HTTP
encode_response(<<Codigo/binary>>, <<Payload/binary>>, <<MimeType/binary>>) ->
	encode_response(Codigo, Payload, MimeType, undefined).
	
encode_response(<<Codigo/binary>>, <<Payload/binary>>, <<MimeType/binary>>, Header) ->
	PayloadLength = list_to_binary(integer_to_list(size(Payload))),
	Response = [<<"HTTP/1.1 "/utf8>>, Codigo, <<" OK\n"/utf8>>,
				<<"Server: ErlangMS\n"/utf8>>,
				<<"Content-Type: "/utf8>>, MimeType, <<"\n"/utf8>>,
				<<"Content-Length: "/utf8>>, PayloadLength, <<"\n"/utf8>>,
				<<"Access-Control-Allow-Origin: *\n"/utf8>>,
				<<"Access-Control-Allow-Methods: GET, PUT, POST, DELETE, OPTIONS\n"/utf8>>,
				<<"Access-Control-Allow-Headers: Content-Type, Content-Range, Content-Disposition, Content-Description, X-Requested-With, X-CSRFToken, X-CSRF-Token, Authorization\n"/utf8>>,
				case Header of undefined -> header_cache_control(MimeType); _ -> Header end,
				<<"\n\n"/utf8>>, 
	            Payload],
	Response2 = iolist_to_binary(Response),
	Response2.


encode_response(Codigo, []) ->
	encode_response(Codigo, <<"[]">>, <<"application/json; charset=utf-8"/utf8>>);
encode_response(<<Codigo/binary>>, []) ->
	encode_response(Codigo, <<"[]">>, <<"application/json; charset=utf-8"/utf8>>);
encode_response(<<Codigo/binary>>, <<>>) ->
	encode_response(Codigo, <<"[]">>, <<"application/json; charset=utf-8"/utf8>>);
encode_response(<<Codigo/binary>>, <<Payload/binary>>) ->
	encode_response(Codigo, Payload, <<"application/json; charset=utf-8"/utf8>>);
encode_response(Codigo, Payload) when is_tuple(Payload) ->
    Payload2 = ems_schema:to_json(Payload),
    encode_response(Codigo, Payload2).
						
header_cache_control(<<"application/x-javascript">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"text/css">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/x-icon">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/png">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/gif">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/jpeg">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/bmp">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"application/font-woff">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<_MimeType/binary>>) ->
	<<"Cache-Control: no-cache"/utf8>>.


-spec parse_querystring(list()) -> list(tuple()).
parse_querystring(Q) ->
	Q1 = httpd:parse_query(Q),
	Q2 = [{iolist_to_binary(P), 
		   list_to_binary(case V of
										[34|_] -> remove_quoted_str(utf8_list_to_string(V));
										_  -> utf8_list_to_string(V)
						    end)}  || {P,V} <- Q1],
	maps:from_list(Q2).


-spec rid_to_string(integer()) -> list().
rid_to_string(RID) -> integer_to_list(RID).


method_to_string(Method) when is_atom(Method) -> atom_to_list(Method);
method_to_string(Method) -> Method.

decode_http_header(Headers, Params) ->
    case erlang:decode_packet(httph, Headers, []) of
        { ok, http_eoh, Rest } -> 
			{maps:from_list(Params), Rest};
        { ok, {http_header,_,P,_,V}, Rest } ->
            decode_http_header(Rest, [{P, V} | Params])
    end.

decode_http_request(RequestBin) ->
	case erlang:decode_packet(http_bin, RequestBin, []) of
		{ok, {http_error, _}, _} ->
			ems_logger:error("ems_util decode http error: ~p.", [RequestBin]),
			{error, http_error};
		{ok, Req, Rest} ->
			{http_request, Method, {abs_path, Uri}, {Http_Version_Major, Http_Version_Minor}} = Req,
			Http_Version = io_lib:format("HTTP/~p.~p", [Http_Version_Major, Http_Version_Minor]),
			case decode_http_header(Rest, []) of
				{error, ReasonDecodeHeader} -> {error, ReasonDecodeHeader};
				{Http_Params, Payload} -> {method_to_string(Method), 
										   binary_to_list(Uri), 
										   Http_Params, 
										   Http_Version,
										   Payload}
			end;
		{error, Reason} -> 
			ems_logger:error("ems_util decode http error: ~p.", [RequestBin]),
			{error, Reason}
	end.


%% @doc Decodifica o payload e transforma em um tipo Erlang
decode_payload_as_json(undefined) -> #{};
decode_payload_as_json(<<>>) -> #{};
decode_payload_as_json(PayloadBin) ->
	case json_decode_as_map(PayloadBin) of
		{ok, PayloadMap} -> PayloadMap;
		{error, _Reason} -> erlang:error(invalid_payload)
	end.

decode_payload_as_xml(undefined) -> #{};
decode_payload_as_xml(<<>>) -> #{};
decode_payload_as_xml(_) -> #{}.
	

-spec is_url_valido(binary() | string()) -> boolean().
is_url_valido(Url) when is_binary(Url) ->
	is_url_valido(binary_to_list(Url));
is_url_valido(Url) ->
	REPattern = ems_db:get_re_param(check_url_valid_re, "^((http:\/\/)|(\/))?([a-z_0-9\-]+\.)?[a-z_0-9\-.\/]+\.[a-z_0-9]{2,4}(\.[a-z0-9]{2,4})?(\/.*)?$"),
	case re:run(Url, REPattern) of
		nomatch -> false;
		_ -> true
	end.


-spec mask_ipaddress_to_tuple(binary()) -> tuple().
mask_ipaddress_to_tuple(<<IpAddress/binary>>) ->
	mask_ipaddress_to_tuple(binary_to_list(IpAddress));
mask_ipaddress_to_tuple(IpAddress) ->
	L = string:tokens(IpAddress, "."),
	L2 = lists:map(fun(X) -> 
								case X of
									"*" -> '_';
									_ -> list_to_integer(X)
								end
					end, L),
	list_to_tuple(L2).


%% @doc Retorna true se Ip2 combina com algum Ip da lista Ip1
-spec match_ip_address(list(), tuple()) -> boolean().
match_ip_address([Ip1|T], Ip2) ->
	case match_ip_address(Ip1, Ip2) of
		true -> true;
		false -> match_ip_address(T, Ip2)
	end;

%% @doc Retorna true se Ip2 combina com Ip1
match_ip_address([], _) -> false;
match_ip_address({O1, O2, O3, O4}, {X1, X2, X3, X4}) ->
   (O1 == '_' orelse O1 == X1) andalso
   (O2 == '_' orelse O2 == X2) andalso
   (O3 == '_' orelse O3 == X3) andalso
   (O4 == '_' orelse O4 == X4).
	
	
-spec parse_basic_authorization_header(Header :: binary()) -> {ok, string(), string()} | {error, access_denied}.
parse_basic_authorization_header(<<Basic:5/binary, _:1/binary, Secret/binary>>) ->
	case Basic =:= <<"Basic">> of
		true ->
			Secret2 = base64:decode_to_string(binary_to_list(Secret)),
			case string:tokens(Secret2, ":") of
				[Login|[Password|_]] -> {ok, Login, Password};
				_ -> {error, access_denied}
			end;
		false -> {error, access_denied}
	end;
parse_basic_authorization_header(_) -> {error, access_denied}.
	
-spec parse_bearer_authorization_header(Header :: binary()) -> {ok, binary()} | {error, access_denied}.
parse_bearer_authorization_header(Header) ->
	case Header of 
		<<Bearer:6/binary, _:1/binary, Secret/binary>> ->
			case Bearer =:= <<"Bearer">> of
				true ->	{ok, Secret};
				false -> {error, access_denied}
			end;
		_ -> {error, access_denied}
	end.

-spec parse_authorization_type(binary() | string() | oauth2 | basic | public | 0 | 1 | 2) -> atom().
parse_authorization_type(<<"Basic">>) -> basic;
parse_authorization_type(<<"basic">>) -> basic;
parse_authorization_type(<<"OAuth2">>) -> oauth2;
parse_authorization_type(<<"oauth2">>) -> oauth2;
parse_authorization_type(<<"Public">>) -> public;
parse_authorization_type(<<"public">>) -> public;
parse_authorization_type("Basic") -> basic;
parse_authorization_type("basic") -> basic;
parse_authorization_type("OAuth2") -> oauth2;
parse_authorization_type("oauth2") -> oauth2;
parse_authorization_type("Public") -> public;
parse_authorization_type("public") -> public;
parse_authorization_type(<<>>) -> public;
parse_authorization_type(oauth2) -> oauth2;
parse_authorization_type(basic) -> basic;
parse_authorization_type(public) -> public;
parse_authorization_type(0) -> public;
parse_authorization_type(1) -> basic;
parse_authorization_type(2) -> oauth2;
parse_authorization_type(_) -> erlang:error(einvalid_authorization_type).


-spec parse_result_cache(non_neg_integer()) -> non_neg_integer().
parse_result_cache(ResultCache) ->
	% Máximo permitido: 1 dia
	case is_integer(ResultCache) andalso ResultCache >= 0 andalso ResultCache =< 86400000 of
		true -> ResultCache;
		_ -> erlang:error(einvalid_result_cache)
	end.	


-spec parse_timeout(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
parse_timeout(Timeout, MaxTimeout) ->
	case is_integer(Timeout) andalso Timeout > 0 andalso Timeout =< MaxTimeout of
		true -> Timeout;
		_ -> erlang:error(einvalid_timeout)
	end.	

-spec parse_type_service(binary() | string() | non_neg_integer()) -> binary(). 
parse_type_service(<<"GET">>) -> <<"GET">>;
parse_type_service(<<"POST">>) -> <<"POST">>;
parse_type_service(<<"PUT">>) -> <<"PUT">>;
parse_type_service(<<"DELETE">>) -> <<"DELETE">>;
parse_type_service(<<"OPTIONS">>) -> <<"OPTIONS">>;
parse_type_service(<<"KERNEL">>) -> <<"KERNEL">>;
parse_type_service("GET") -> <<"GET">>;
parse_type_service("POST") -> <<"POST">>;
parse_type_service("PUT") -> <<"PUT">>;
parse_type_service("DELETE") -> <<"DELETE">>;
parse_type_service("OPTIONS") -> <<"OPTIONS">>;
parse_type_service("KERNEL") -> <<"KERNEL">>;
parse_type_service(0) -> <<"KERNEL">>;
parse_type_service(1) -> <<"GET">>;
parse_type_service(2) -> <<"POST">>;
parse_type_service(3) -> <<"PUT">>;
parse_type_service(4) -> <<"DELETE">>;
parse_type_service(5) -> <<"OPTIONS">>;
parse_type_service(_) -> erlang:error(einvalid_type_service).


-spec parse_type_querystring(binary() | string() | non_neg_integer()) -> binary(). 
parse_type_querystring(<<"int">>) -> <<"int">>;
parse_type_querystring(<<"string">>) -> <<"string">>;
parse_type_querystring(0) -> <<"int">>;
parse_type_querystring(1) -> <<"string">>;
parse_type_querystring(_) -> erlang:error(einvalid_type_querystring).


-spec parse_url_service(binary() | list()) -> binary().
parse_url_service(<<"/">>) -> <<"/">>;
parse_url_service(Url) when is_binary(Url) ->
	parse_url_service(binary_to_list(Url));
parse_url_service(Url) ->
	LenUrl = length(Url),
	case LenUrl > 0 andalso LenUrl =< 360 andalso is_url_valido(Url) of
		true -> list_to_binary(Url);
		false -> erlang:error(einvalid_url_service)
	end.

-spec parse_lang(binary() | string() | non_neg_integer()) -> binary().
parse_lang(<<"erlang">>) -> <<"erlang">>;
parse_lang("erlang") -> <<"erlang">>;
parse_lang(<<"java">>) -> <<"java">>;
parse_lang("java") -> <<"java">>;
parse_lang(0) -> <<"erlang">>;
parse_lang(1) -> <<"java">>;
parse_lang(_) -> erlang:error(einvalid_lang_service).

-spec parse_name_service(binary() | string()) -> binary().
parse_name_service(Name) when is_list(Name) ->
	parse_name_service(list_to_binary(Name));
parse_name_service(Name) ->
	REPattern = ems_db:get_re_param(check_name_service_valid_re, "^[/_a-zA-Z-.][.:/_a-zA-Z0-9-]{0,300}$"),
	case re:run(Name, REPattern) of
		nomatch -> erlang:error(einvalid_name_service);
		_ -> Name
	end.
	
parse_name_querystring(Name) when is_list(Name) ->
	parse_name_querystring(list_to_binary(Name));
parse_name_querystring(Name) ->
	REPattern = ems_db:get_re_param(check_name_querystring_valid_re, "^[_a-zA-Z][_a-zA-Z0-9]{0,29}$"),
	case re:run(Name, REPattern) of
		nomatch -> erlang:error(einvalid_name_querystring);
		_ -> Name
	end.
	
	
%% @doc Retorna uma mapa das querystrings e a quantidade de queries obrigatórias
-spec parse_querystring_def(list()) -> {list(map()), non_neg_integer()}.	
parse_querystring_def([]) -> {[], 0};
parse_querystring_def(Querystring) -> parse_querystring_def(Querystring, [], 0).
	
%% @doc Retorna uma mapa das querystrings e a quantidade de queries obrigatórias
-spec parse_querystring_def(list(), list(), non_neg_integer()) -> {list(map()), non_neg_integer()}.	
parse_querystring_def([], Querystring, QtdRequired) -> 	
	{Querystring, QtdRequired};
parse_querystring_def([H|T], Querystring, QtdRequired) -> 
	Name = parse_name_querystring(maps:get(<<"name">>, H)),
	Type = parse_type_querystring(maps:get(<<"type">>, H, <<"string">>)),
	Default = maps:get(<<"default">>, H, <<>>),
	Comment = maps:get(<<"comment">>, H, <<>>),
	Required = parse_bool(maps:get(<<"required">>, H, false)),
	case Required of
		true  -> QtdRequired2 = QtdRequired + 1;
		false -> QtdRequired2 = QtdRequired
	end,
	Q = #{<<"name">>     => Name,
		  <<"type">>     => Type,
		  <<"default">>  => Default,
		  <<"comment">>  => Comment,
		  <<"required">> => Required},
	parse_querystring_def(T, [Q | Querystring], QtdRequired2).

	
-spec parse_tcp_listen_address(list(string()) | list(binary()) |  list(tuple()) | string() | binary() | undefined | null) -> list(tuple()). 
parse_tcp_listen_address(undefined) -> [];
parse_tcp_listen_address(null) -> [];
parse_tcp_listen_address(<<>>) -> [];
parse_tcp_listen_address("") -> [];
parse_tcp_listen_address([{_,_,_,_}|_] = ListenAddress) -> ListenAddress;
parse_tcp_listen_address([H|_] = ListenAddress) when is_binary(H) -> 
	parse_tcp_listen_address_t(ListenAddress, []);
parse_tcp_listen_address([H|_] = ListenAddress) when is_list(H) -> 
	parse_tcp_listen_address_t(ListenAddress, []);
parse_tcp_listen_address(ListenAddress) when is_binary(ListenAddress) ->
	parse_tcp_listen_address(binary_to_list(ListenAddress));
parse_tcp_listen_address(ListenAddress) ->
	ListenAddress2 = string:trim(ListenAddress),
	case ListenAddress2 =/= "" of
		true ->	
			ListenAddress3 = [string:trim(IP) || IP <- string:split(ListenAddress2, ",")],
			parse_tcp_listen_address_t(ListenAddress3, []);
		false -> []
	end.

-spec parse_tcp_listen_address_t(list(string()) | list(binary()), list(tuple())) -> list(tuple()). 
parse_tcp_listen_address_t([], Result) -> Result;
parse_tcp_listen_address_t([H|T], Result) when is_binary(H) ->
	parse_tcp_listen_address_t([binary_to_list(H) | T], Result);
parse_tcp_listen_address_t([H|T], Result) ->
	case inet:parse_address(H) of
		{ok, {0, 0, 0, 0}} ->
			case ip_list() of
				{ok, IpList} -> IpList;
				_Error -> []
			end;
		{ok, L2} -> 
			case lists:member(L2, Result) of
				true -> parse_tcp_listen_address_t(T, Result);
				false -> parse_tcp_listen_address_t(T, [L2|Result])
			end;
		{error, einval} -> erlang:error(einvalid_tcp_listen_address)
	end.
	
-spec parse_allowed_address(all | undefined | null | binary() | string() | list()) -> list(tuple()).
parse_allowed_address(all) -> all;
parse_allowed_address(undefined) -> all;
parse_allowed_address(null) -> all;
parse_allowed_address(AllowedAddress) when is_binary(AllowedAddress) ->
	parse_allowed_address(binary_to_list(AllowedAddress));
parse_allowed_address(AllowedAddress) when is_list(AllowedAddress) ->
	AllowedAddress2 = string:trim(AllowedAddress),
	case AllowedAddress2 =/= "" of
		true ->	
			AllowedAddress3 = [string:trim(IP) || IP <- string:split(AllowedAddress2, ",")],
			parse_allowed_address_t(AllowedAddress3);
		false -> []
	end;
parse_allowed_address(AddrList) -> 
	ems_util:binlist_to_list(AddrList).

-spec parse_allowed_address_t(all | undefined | list()) -> all | undefined | list().
parse_allowed_address_t(all) -> all;
parse_allowed_address_t(undefined) -> undefined;
parse_allowed_address_t(null) -> undefined;
parse_allowed_address_t(AllowedAddress) -> 
	[mask_ipaddress_to_tuple(IP) || IP <- AllowedAddress].


-spec parse_tcp_port(undefined | binary() | string() | non_neg_integer()) -> non_neg_integer().
parse_tcp_port(undefined) -> undefined;
parse_tcp_port(<<Port/binary>>) -> 
	parse_tcp_port(binary_to_list(Port));		
parse_tcp_port(Port) when is_list(Port) -> 
	parse_tcp_port(list_to_integer(Port));
parse_tcp_port(Port) when is_integer(Port) -> 
	case is_range_valido(Port, ?TCP_PORT_MIN, ?TCP_PORT_MAX) of
		true -> Port;
		false -> erlang:error(einvalid_tcp_port)
	end.
	
	
-spec node_binary() -> binary().
node_binary() -> erlang:atom_to_binary(node(), utf8).

uptime_str() ->
	{UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds", [D,H,M,S])).
    

%% @doc Retorna a URL do request
get_property_request(<<"url">>, Request) ->
	Request#request.url;

%% @doc Retorna o tipo do request
get_property_request(<<"metodo">>, Request) ->
	Request#request.type;

get_property_request(<<"type">>, Request) ->
	Request#request.type;

%% @doc Retorna a URL do request
get_property_request(<<"http_version">>, Request) ->
	Request#request.version;

%% @doc Retorna o payload/body do request
get_property_request(<<"payload">>, Request) ->
	Request#request.payload_map;

%% @doc Retorna o payload/body do request
get_property_request(<<"body">>, Request) ->
	Request#request.payload_map.

%% @doc Retorna um parâmetro do request
get_param_url(NomeParam, Default, Request) ->
	ParamsUrl = Request#request.params_url,
	NomeParam2 = iolist_to_binary(NomeParam),
	maps:get(NomeParam2, ParamsUrl, Default).


get_querystring(<<QueryName/binary>>, Servico) ->	
	[Query] = [Q || Q <- maps:get(<<"querystring">>, Servico, <<>>), Q#service.comment == QueryName],
	Query.


%% @doc Retorna uma querystring do request
get_querystring(QueryName, Default, #request{querystring_map = QuerystringMap}) ->
	Value = maps:get(QueryName, QuerystringMap, Default),
	case erlang:is_list(Value) of
		true -> list_to_binary(Value);
		false -> Value
	end.

get_querystring(QueryName, OrQueryName2, Default, #request{querystring_map = QuerystringMap}) ->
	case maps:is_key(QueryName, QuerystringMap) of
		true ->	Value = maps:get(QueryName, QuerystringMap, Default);
		false -> Value = maps:get(OrQueryName2, QuerystringMap, Default)
	end,
	case erlang:is_list(Value) of
		true -> list_to_binary(Value);
		false -> Value
	end.


load_from_file_req(Request = #request{url = Url,
									  if_modified_since = IfModifiedSinceReq, 
									  if_none_match = IfNoneMatchReq,
									  timestamp = Timestamp,
									  service = #service{cache_control = CacheControl,
														 expires = ExpiresMinute,
														 path = Path}}) ->
	Filename = Path ++ string:substr(Url, string:len(hd(string:tokens(Url, "/")))+2),
	case file:read_file_info(Filename, [{time, universal}]) of
		{ok,{file_info, FSize, _Type, _Access, _ATime, MTime, _CTime, _Mode,_,_,_,_,_,_}} -> 
			?DEBUG("ems_static_file_service loading file ~p.", [Filename]),
			MimeType = mime_type(filename:extension(Filename)),
			ETag = integer_to_binary(erlang:phash2({FSize, MTime}, 16#ffffffff)),
			LastModified = cowboy_clock:rfc1123(MTime),
			ExpireDate = date_add_minute(Timestamp, ExpiresMinute + 120), % add +120min (2h) para ser horário GMT
			Expires = cowboy_clock:rfc1123(ExpireDate),
			HttpHeader =	#{
								<<"cache-control">> => CacheControl,
								<<"etag">> => ETag,
								<<"last-modified">> => LastModified,
								<<"expires">> => Expires
							},
			case ETag == IfNoneMatchReq orelse LastModified == IfModifiedSinceReq of
				true -> {ok, Request#request{code = 304, 
											 reason = enot_modified,
											 content_type = MimeType,
											 etag = ETag,
											 filename = Filename,
											 response_data = <<>>, 
											 response_header = HttpHeader}
						 };
				false ->
					case file:read_file(Filename) of
						{ok, FileData} -> 
							{ok, Request#request{code = 200, 
											     reason = ok,
												 content_type = MimeType,
											     etag = ETag,
											     filename = Filename,
											     response_data = FileData, 
											     response_header = HttpHeader}
							};
						{error, Reason} = Error -> 
							{error, Request#request{code = case Reason of enoent -> 404; _ -> 400 end, 
												    reason = Reason,
												    content_type = ?CONTENT_TYPE_JSON,
												    response_data = ems_schema:to_json(Error)}
							}
					end
			end;
		{error, Reason} = Error -> 
			ems_logger:warn("ems_static_file_service file ~p does not exist.", [Filename]),
			{error, Request#request{code = case Reason of enoent -> 404; _ -> 400 end, 
									reason = Reason,	
									response_data = ems_schema:to_json(Error)}
			 }
	end.


-spec tuple_to_maps_with_keys(list(tuple()), list(tuple())) -> map().
tuple_to_maps_with_keys(Tuple, Keys) ->
	Fields = erlang:tuple_to_list(Tuple),
	Record = tuple_to_maps_with_keys(Fields, Keys, []),
	maps:from_list(Record).

tuple_to_maps_with_keys(_, [], Result) -> Result;
tuple_to_maps_with_keys([null|FldT], Keys, Result) ->
	tuple_to_maps_with_keys([undefined|FldT], Keys, Result);
tuple_to_maps_with_keys([FldH|FldT], Keys, Result) when is_list(FldH) ->
	tuple_to_maps_with_keys([list_to_binary(FldH)|FldT], Keys, Result);
tuple_to_maps_with_keys([FldH|FldT], [KeyH|KeyT], Result) ->
	tuple_to_maps_with_keys(FldT, KeyT, [{KeyH, FldH} | Result]).


	
%% *********** Functions for data validation ************

-spec is_range_valido(non_neg_integer(), integer(), integer()) -> boolean().
is_range_valido(Number, RangeIni, RangeFim) when Number >= RangeIni andalso Number =< RangeFim -> true;
is_range_valido(_Number, _RangeIni, _RangeFim) -> false.


-spec parse_range(non_neg_integer(), integer(), integer()) -> non_neg_integer.
parse_range(Number, RangeIni, RangeFim) when Number >= RangeIni andalso Number =< RangeFim -> Number;
parse_range(_, _, _) -> erlang:error(erange_not_allowed).


-spec parse_email(string() | binary()) -> binary() | undefined.
parse_email(Value) when is_binary(Value) ->
	parse_email(binary_to_list(Value));
parse_email(Value) ->
	case length(Value) > 8 of
		true ->
			Value2 = string:to_lower(Value),
			REPattern = ems_db:get_re_param(check_email_valid_re, "\\b[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}\\b"),
			case re:run(Value2, REPattern) of
				nomatch -> erlang:error(einvalid_email);
				_ -> list_to_binary(Value2)
			end;
		false ->
			erlang:error(einvalid_email)
	end.
	

-spec is_email_valido(string()) -> boolean().
is_email_valido(Value) -> 
	REPattern = ems_db:get_re_param(check_email_valid_re, "\\b[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-z]{2,4}\\b"),
	case re:run(Value, REPattern) of
		nomatch -> false;
		_ -> true
	end.

%% @doc Retorna mensagem registro já existente
msg_registro_ja_existe(Pattern) ->
	case ems_db:existe(Pattern) of
		false -> [];
		_ -> <<"Registro já está cadastrado."/utf8>>
	end.

%% @doc Retorna mensagem registro já existente
msg_registro_ja_existe(Pattern, Message) ->
	case ems_db:existe(Pattern) of
		false -> [];
		_ -> Message
	end.
		
%% @doc Mensagens de campo obrigatório
msg_campo_obrigatorio(NomeCampo, []) -> 
	iolist_to_binary(io_lib:format(<<"Campo não preenchido: '~s'."/utf8>>, [NomeCampo]));
msg_campo_obrigatorio(NomeCampo, <<>>) -> 
	iolist_to_binary(io_lib:format(<<"Campo não preenchido: '~s'."/utf8>>, [NomeCampo]));
msg_campo_obrigatorio(_NomeCampo, _Value) -> [].

%% @doc Mensagem de e-mail inválido
msg_email_invalido(_NomeCampo, []) -> [];
msg_email_invalido(_NomeCampo, Value) -> 
	case is_email_valido(Value) of
		false -> iolist_to_binary(io_lib:format(<<"Email informado é inválido: '~s'."/utf8>>, [Value]));
		_ -> []
	end.

%% @doc Retorna somente mensagens não vazias
mensagens(L) -> lists:filter(fun(X) -> X /= [] end, L).

-spec parse_request_querystring(#service{}, #request{}) -> map().
parse_request_querystring(Service, Request) ->
	%% Querystrings do módulo ems_static_file_service e ems_options_service não são processados.
	QuerystringUser = Request#request.querystring_map,
	case Service#service.module of
		ems_static_file_service -> QuerystringUser;
		ems_options_service -> QuerystringUser;
		_ ->
			QuerystringServico = Service#service.querystring,
			case QuerystringUser =:= #{} of
				true -> 
					case QuerystringServico =:= [] of
						true -> QuerystringUser;
						false -> parse_request_querystring_defaults(QuerystringServico, QuerystringUser, [])
					end;
				false -> 
					case QuerystringServico =:= [] of
						true -> #{};
						false -> parse_request_querystring_defaults(QuerystringServico, QuerystringUser, [])
					end
			end
	end.

parse_request_querystring_defaults([], _QuerystringUser, QuerystringList) -> maps:from_list(QuerystringList);
parse_request_querystring_defaults([H|T], QuerystringUser, QuerystringList) ->
	%% Verifica se encontra a query na querystring do usuário
	NomeQuery = maps:get(<<"name">>, H),
	case maps:find(NomeQuery, QuerystringUser) of
		{ok, Value} -> 
			parse_request_querystring_defaults(T, QuerystringUser, [{NomeQuery, Value} | QuerystringList]);
		error ->
			%% se o usuário não informou a querystring, verifica se tem valor default na definição do serviço
			case maps:get(<<"default">>, H, enoent) of
				enoent -> [];
				Value -> parse_request_querystring_defaults(T, QuerystringUser, [{NomeQuery, Value} | QuerystringList])
			end
	end.

-spec compile_modulo_erlang(binary() | string() | undefined, binary() | string()) -> ok | {error, einvalidfilename} | {error, einvalid_dir}.
compile_modulo_erlang(undefined, _) -> ok;
compile_modulo_erlang(<<>>, _) -> ok;
compile_modulo_erlang(Path, ModuleNameCanonical) when is_binary(Path) ->
	compile_modulo_erlang(binary_to_list(Path), ModuleNameCanonical);
compile_modulo_erlang(Path, ModuleNameCanonical) when is_binary(ModuleNameCanonical) ->
	compile_modulo_erlang(Path, binary_to_list(ModuleNameCanonical));
compile_modulo_erlang(Path, ModuleNameCanonical) ->
	case filelib:is_dir(Path) of
		true ->
			Filename = Path ++ "/" ++ ModuleNameCanonical ++ ".erl",
			case filelib:is_regular(Filename) of
				true ->
					ems_logger:info("Compile file ~p ", [Filename]),
					code:add_path(Path), 
					case compile:file(Filename, [{outdir, Path ++ "/"}]) of
						error -> ems_logger:error("[ ERROR ]\n");
						{error, Errors, _Warnings} -> 
							ems_logger:error("[ ERROR ]\n"),
							ems_logger:error("~p\n", [Errors]);
						_ -> 
							ems_logger:error("[ OK ]\n"),
							ok
					end;
				_ -> {error, einvalid_filename}
			end;
		false -> {error, einvalid_dir}
	end.

-spec print_int_map(map()) -> binary().
print_int_map(Map) -> print_int_map(Map, maps:keys(Map), maps:values(Map), <<>>, []).

-spec print_int_map(map(), list(), list(), binary(), list()) -> binary().
print_int_map(_, [], _, _, Result) -> iolist_to_binary(lists:reverse(Result));
print_int_map(Map, [Key|TKey], [Value|TValue], Sep, Result) ->
	print_int_map(Map, TKey, TValue, <<", ">>, [[Sep, Key, <<"=">>, integer_to_binary(Value)] | Result]).
	

-spec print_str_map(map()) -> binary().
print_str_map(Map) -> print_str_map(Map, maps:keys(Map), maps:values(Map), <<>>, []).

-spec print_str_map(map(), list(), list(), binary(), list()) -> binary().
print_str_map(_, [], _, _, Result) -> iolist_to_binary(lists:reverse(Result));
print_str_map(Map, [Key|TKey], [Value|TValue], Sep, Result) ->
	print_str_map(Map, TKey, TValue, <<", ">>, [[Sep, Key, <<"=\"">>, Value, <<"\"">>] | Result]).


-spec binlist_to_atomlist(list(binary()) | binary()) -> list(atom()) | atom().
binlist_to_atomlist([])  -> undefined;
binlist_to_atomlist(undefined)  -> undefined;
binlist_to_atomlist(<<>>)  -> undefined;
binlist_to_atomlist(Value) when is_list(Value) ->
	binlist_to_atomlist_(Value, []);
binlist_to_atomlist(Value)  ->
	binary_to_atom(Value, utf8).

binlist_to_atomlist_([], Result) -> Result;
binlist_to_atomlist_([H|T], Result) ->
	binlist_to_atomlist_(T, [binary_to_atom(H, utf8)|Result]).

-spec json_field_strip_and_escape(string() | binary()) -> iolist().
json_field_strip_and_escape([]) ->	<<"null"/utf8>>;
json_field_strip_and_escape(<<>>) -> <<"null"/utf8>>;
json_field_strip_and_escape(undefined) -> <<"null"/utf8>>;
json_field_strip_and_escape(Value) when is_binary(Value) ->
	json_field_strip_and_escape(binary_to_list(Value));
json_field_strip_and_escape(Value) -> 
	case string:strip(Value) of
		[] -> <<"null"/utf8>>;
		ValueStrip -> 
			ValueEscaped = [case Ch of 
									34 -> "\\\""; 
									_ -> Ch 
							end || Ch <- ValueStrip],
			[<<"\""/utf8>>, ValueEscaped, <<"\""/utf8>>]
	end.

-spec parse_user_agent(binary() | string()) -> tuple().
parse_user_agent(<<>>) -> {browser_other, ""};
parse_user_agent(undefined) -> {browser_other, ""};
parse_user_agent(UserAgent) when is_binary(UserAgent) ->
	parse_user_agent(binary_to_list(UserAgent));
parse_user_agent(UserAgent) ->
	case string:rstr(UserAgent, "Chrome/") of
		PosChrome when PosChrome > 0 ->
			BrowserName = browser_chrome,
			BrowserVersion = parse_user_agent_version(string:substr(UserAgent, PosChrome+7, 4));
		0 ->
			case string:rstr(UserAgent, "Firefox/") of
				PosFirefox when PosFirefox > 0 ->
					BrowserName = browser_firefox,
					BrowserVersion = parse_user_agent_version(string:substr(UserAgent, PosFirefox+8, 4));
				0 ->
					case string:rstr(UserAgent, "Trident/") of
						PosTrident when PosTrident > 0 ->
							BrowserName = browser_ie,
							BrowserVersion = parse_user_agent_version(string:substr(UserAgent, PosTrident+8, 4));
						0 ->
							case string:rstr(UserAgent, "Edge/") of
								PosEdge when PosEdge > 0 ->
									BrowserName = browser_edge,
									BrowserVersion = parse_user_agent_version(string:substr(UserAgent, PosEdge+5, 4));
								0 ->
									case string:rstr(UserAgent, "OPR/") of
										PosOpera when PosOpera > 0 ->
											BrowserName = browser_opera,
											BrowserVersion = parse_user_agent_version(string:substr(UserAgent, PosOpera+4, 4));
										0 ->
											case string:rstr(UserAgent, "insomnia/") of
												PosInsomnia when PosInsomnia > 0 ->
													BrowserName = browser_insomnia,
													BrowserVersion = parse_user_agent_version_subversion(string:substr(UserAgent, PosInsomnia+9, 5));
												0 ->
													case string:rstr(UserAgent, "Safari/") of
														PosSafari when PosSafari > 0 ->
															BrowserName = browser_safari,
															BrowserVersion = parse_user_agent_version(string:substr(UserAgent, PosSafari+7, 4));
														0 ->
															BrowserName = browser_other,
															BrowserVersion = ""
													end
											end
									end
							end
					end
			end
	end,
	{BrowserName, list_to_binary(BrowserVersion)}.

parse_user_agent_version(Version) -> parse_user_agent_version(Version, []).
parse_user_agent_version([], Result) -> lists:reverse(Result);
parse_user_agent_version([$.|_], Result) -> lists:reverse(Result);
parse_user_agent_version([H|T], Result) -> 
  parse_user_agent_version(T, [H|Result]).

parse_user_agent_version_subversion(Version) ->
	parse_user_agent_version_subversion(Version, false, []).
parse_user_agent_version_subversion([], _, Result) -> lists:reverse(Result);
parse_user_agent_version_subversion([$.|_], true, Result) -> lists:reverse(Result);
parse_user_agent_version_subversion([$.|T], false, Result) -> 
	parse_user_agent_version_subversion(T, true, [$.|Result]);
parse_user_agent_version_subversion([H|T], Stop, Result) -> 
  parse_user_agent_version_subversion(T, Stop, [H|Result]).
		
-spec user_agent_atom_to_binary(atom()) -> binary().
user_agent_atom_to_binary(browser_chrome) -> <<"Chrome">>;
user_agent_atom_to_binary(browser_firefox) -> <<"Firefox">>;
user_agent_atom_to_binary(browser_ie) -> <<"IE">>;
user_agent_atom_to_binary(browser_insomnia) -> <<"Insomnia">>;
user_agent_atom_to_binary(browser_opera) -> <<"Opera">>;
user_agent_atom_to_binary(browser_safari) -> <<"Safari">>;
user_agent_atom_to_binary(_) -> <<"Other">>.


-spec to_lower_and_remove_backslash(string() | binary()) -> binary().
to_lower_and_remove_backslash(undefined) -> <<>>;
to_lower_and_remove_backslash(<<>>) -> <<>>;
to_lower_and_remove_backslash(<<"/">>) -> <<"/">>;
to_lower_and_remove_backslash("/") -> <<"/">>;
to_lower_and_remove_backslash(Uri) when is_binary(Uri) ->
	to_lower_and_remove_backslash(binary_to_list(Uri));
to_lower_and_remove_backslash(Uri) ->	
	list_to_binary(string:to_lower(remove_ult_backslash_url(Uri))).
	
	
-spec check_type_email(binary(), binary()) -> 1 | 2.
check_type_email("", _) -> 2;
check_type_email(SufixoEmailInstitucional, Email) ->
	case lists:suffix(SufixoEmailInstitucional, binary_to_list(Email)) of
		true -> 1;
		false -> 2
	end.

-spec is_email_institucional(binary(), binary()) -> boolean().
is_email_institucional("", _) -> false;
is_email_institucional(SufixoEmailInstitucional, Email) ->
	case lists:suffix(SufixoEmailInstitucional, binary_to_list(Email)) of
		true -> true;
		false -> false
	end.

