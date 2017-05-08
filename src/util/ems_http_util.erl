%%********************************************************************
%% @title Module ems_http_util
%% @version 1.0.0
%% @doc Module with useful functions for the HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_util).

-compile(export_all).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include("../../include/ems_http_messages.hrl").

encode_request_cowboy(CowboyReq, WorkerSend) ->
	try
		Url = cowboy_req:path(CowboyReq),
		Url2 = ems_util:remove_ult_backslash_url(binary_to_list(Url)),
		Uri = iolist_to_binary(cowboy_req:uri(CowboyReq)),
		RID = erlang:system_time(),
		Timestamp = calendar:local_time(),
		T1 = ems_util:get_milliseconds(),
		Method = binary_to_list(cowboy_req:method(CowboyReq)),
		{Ip, _} = cowboy_req:peer(CowboyReq),
		IpBin = list_to_binary(inet_parse:ntoa(Ip)),
		Host = cowboy_req:host(CowboyReq),
		Version = cowboy_req:version(CowboyReq),
		ContentType = cowboy_req:header(<<"content-type">>, CowboyReq),
		ContentLength = cowboy_req:body_length(CowboyReq),
		QuerystringBin = cowboy_req:qs(CowboyReq),
		ProtocolBin = cowboy_req:scheme(CowboyReq),
		Protocol = parse_protocol(ProtocolBin),
		Port = cowboy_req:port(CowboyReq),
		case QuerystringBin of
			<<>> -> QuerystringMap = #{};
			_ -> QuerystringMap = parse_querystring([binary_to_list(QuerystringBin)])
		end,
		case ContentLength > 0 of
			true ->
				case ContentType of
					<<"application/x-www-form-urlencoded; charset=UTF-8">> ->
						ContentType2 = <<"application/x-www-form-urlencoded; charset=UTF-8">>,
						{ok, Payload, _} = cowboy_req:read_urlencoded_body(CowboyReq),
						PayloadMap = maps:from_list(Payload),
						QuerystringMap2 = maps:merge(QuerystringMap, PayloadMap);
					<<"application/x-www-form-urlencoded">> ->
						ContentType2 = <<"application/x-www-form-urlencoded; charset=UTF-8">>,
						{ok, Payload, _} = cowboy_req:read_urlencoded_body(CowboyReq),
						PayloadMap = maps:from_list(Payload),
						QuerystringMap2 = maps:merge(QuerystringMap, PayloadMap);
					<<"application/json">> ->
						ContentType2 = <<"application/json">>,
						{ok, Payload, _} = cowboy_req:read_body(CowboyReq),
						PayloadMap = decode_payload_as_json(Payload),
						QuerystringMap2 = QuerystringMap;
					<<"application/xml">> ->
						ContentType2 = <<"application/xml">>,
						{ok, Payload, _} = cowboy_req:read_body(CowboyReq),
						PayloadMap = decode_payload_as_xml(Payload),
						QuerystringMap2 = QuerystringMap;
					_ -> 
						ContentType2 = ContentType,						
						{ok, Payload, _} = cowboy_req:read_body(CowboyReq),
						PayloadMap = #{},
						QuerystringMap2 = QuerystringMap
				end;
			false ->
				ContentType2 = ContentType,						
				Payload = <<>>,
				PayloadMap = #{},
				QuerystringMap2 = QuerystringMap
		end,
		Accept = cowboy_req:header(<<"accept">>, CowboyReq),
		Accept_Encoding = cowboy_req:header(<<"accept-encoding">>, CowboyReq),
		User_Agent = cowboy_req:header(<<"user-agent">>, CowboyReq),
		Cache_Control = cowboy_req:header(<<"cache-control">>, CowboyReq),
		Authorization = cowboy_req:header(<<"authorization">>, CowboyReq),
		IfModifiedSince = cowboy_req:header(<<"if-modified-since">>, CowboyReq),
		IfNoneMatch = cowboy_req:header(<<"if-none-match">>, CowboyReq),
		ReqHash = erlang:phash2([Url, QuerystringBin, ContentLength, ContentType2]),
		{Rowid, Params_url} = ems_util:hashsym_and_params(Url2),
		Request = #request{
			rid = RID,
			rowid = Rowid,
			type = Method,
			uri = Uri,
			url = Url2,
			version = Version,
			querystring = QuerystringBin,
			querystring_map = QuerystringMap2,
			params_url = Params_url,
			content_length = ContentLength,
			content_type = ContentType2,
			accept = Accept,
			user_agent = User_Agent,
			accept_encoding = Accept_Encoding,
			cache_control = Cache_Control,
			ip = Ip,
			ip_bin = IpBin,
			host = Host,
			payload = Payload, 
			payload_map = PayloadMap,
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
			req_hash = ReqHash
		},	
		{ok, Request}
	catch
		_Exception:Reason -> 
			ems_logger:error("ems_http_util invalid http request ~p. Reason: ~p.", [CowboyReq, Reason]),
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
										[34|_] -> ems_util:remove_quoted_str(ems_util:utf8_list_to_string(V));
										_  -> ems_util:utf8_list_to_string(V)
						    end)}  || {P,V} <- Q1],
	maps:from_list(Q2).


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
			ems_logger:error("ems_http_util decode http error: ~p.", [RequestBin]),
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
			ems_logger:error("ems_http_util decode http error: ~p.", [RequestBin]),
			{error, Reason}
	end.


%% @doc Decodifica o payload e transforma em um tipo Erlang
decode_payload_as_json(undefined) -> #{};
decode_payload_as_json(<<>>) -> #{};
decode_payload_as_json(PayloadBin) ->
	case ems_util:json_decode_as_map(PayloadBin) of
		{ok, PayloadMap} -> PayloadMap;
		{error, _Reason} -> erlang:error(invalid_payload)
	end.

decode_payload_as_xml(undefined) -> #{};
decode_payload_as_xml(<<>>) -> #{};
decode_payload_as_xml(_) -> #{}.
	
get_http_header_adicionais(Header) ->
	Header1 = lists:map(fun(H) -> get_param_header(H, []) end, Header),
	maps:from_list(Header1).

%% @doc Retorna uma tupla com o name do cabecalho e o seu valor
%% Ex.: get_param_header("Host: localhost:2301", [])  =>  {"host","localhost:2301"}
get_param_header([], Key) -> {string:to_lower(lists:reverse(Key)), []};
get_param_header([H|T], Key) ->
	case H of
		$: -> 
			P = string:to_lower(lists:reverse(Key)),
			V = format_header_value(P, T),
			{P, V};
		_ -> get_param_header(T, [H|Key])
	end.


%% @doc formata o valor do header (String, Integer)
format_header_value("content-length", Value) ->
	Value1 = string:strip(Value),
	Value2 = list_to_integer(Value1),
	case is_content_length_valido(Value2) of
		true -> Value2;
		false -> 0
	end;

format_header_value(_, Value) -> 
	string:strip(Value).

is_content_length_valido(N) when N < 0; N > ?HTTP_MAX_POST_SIZE -> false;
is_content_length_valido(_) -> true.

%% @doc Retorna booleano se o método é suportado pelo servidor
is_metodo_suportado(<<"GET">>) -> true;
is_metodo_suportado(<<"POST">>) -> true;
is_metodo_suportado(<<"PUT">>) -> true;
is_metodo_suportado(<<"DELETE">>) -> true;
is_metodo_suportado(<<"OPTIONS">>) -> true;
is_metodo_suportado("GET") -> true;
is_metodo_suportado("POST") -> true;
is_metodo_suportado("PUT") -> true;
is_metodo_suportado("DELETE") -> true;
is_metodo_suportado("OPTIONS") -> true;
is_metodo_suportado(_) -> false.

%% @doc Indica se a URL é valida
is_url_valido(Url) ->
	case re:run(Url, "^((http:\/\/)|(\/))?([a-z_0-9\-]+\.)?[a-z_0-9\-]+\.[a-z_0-9]{2,4}(\.[a-z0-9]{2,4})?(\/.*)?$") of
		nomatch -> false;
		_ -> true
	end.

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
match_ip_address([Ip1|T],	Ip2) ->
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
	
	
-spec parse_basic_authorization_header(Header :: binary()) -> {ok, string(), string()} | {error, einvalid_authorization}.
parse_basic_authorization_header(<<Basic:5/binary, _:1/binary, Secret/binary>>) ->
	case Basic =:= <<"Basic">> of
		true ->
			Secret2 = base64:decode_to_string(binary_to_list(Secret)),
			[Login|[Password|_]] = string:tokens(Secret2, ":"),
			{ok, Login, Password};
		false -> 
			{error, einvalid_authorization_header}
	end;
parse_basic_authorization_header(_) -> {error, einvalid_authorization_header}.
	
-spec parse_barer_authorization_header(Header :: binary()) -> {ok, string(), string()} | {error, einvalid_authorization}.
parse_barer_authorization_header(Header) ->
	<<Barer:5/binary, _:1/binary, Secret/binary>> = Header,
	case Barer =:= <<"Barer">> of
		true ->
			base64:decode(Secret);
		false -> 
			{error, einvalid_authorization_header}
	end.


parse_authorization_type(<<"Basic">>) -> http_basic;
parse_authorization_type(<<"basic">>) -> http_basic;
parse_authorization_type(<<"OAuth2">>) -> oauth2;
parse_authorization_type(<<"oauth2">>) -> oauth2;
parse_authorization_type(<<"Public">>) -> public;
parse_authorization_type(<<"public">>) -> public;
parse_authorization_type(<<>>) -> public;
parse_authorization_type(_) -> erlang:error(einvalid_authorization_type).

	
%% @doc Retorna o mime-type do arquivo
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
mime_type(".bmp") -> <<"image/bmp">>;
mime_type(".pdf") -> <<"application/pdf">>;
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
mime_type(".csv") -> <<"text/csv">>;
mime_type(_) -> <<"application/octet-stream">>.
