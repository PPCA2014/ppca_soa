%%********************************************************************
%% @title Module ems_socket
%% @version 1.0.0
%% @doc  Wrapper for http/https sockets based and adapted from MochiWeb socket.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_socket).

-include("../../include/ems_config.hrl").

-export([make_opts_listen/2, 
		 listen/3, accept/2, 
		 recv/3, send/2, close/1, 
		 port/1, peername/1,
         setopts/2, type/1,
         send_data/2, posix_error_description/1]).

make_opts_listen(IpAddress, #tcp_config{tcp_keepalive = KeepAlive, 
										 tcp_nodelay = NoDelay, 
										 tcp_send_timeout = SendTimeout,
										 tcp_backlog = Backlog,
										 tcp_delay_send = DelaySend,
										 tcp_buffer = Buffer,
										 tcp_is_ssl = IsSsl,
										 tcp_ssl = Ssl}) ->
	Opts = [binary, 
			{packet, 0}, 
			{active, true},
			{buffer, Buffer},
			{send_timeout_close, true},
			{send_timeout, SendTimeout}, 
			{keepalive, KeepAlive}, 
			{nodelay, NoDelay},
			{backlog, Backlog},
			{ip, IpAddress},
			{reuseaddr, true},
			{delay_send, DelaySend}],
	case IsSsl of
		true  -> 
			SslOpts1 = add_ssl_opts([], Ssl, cacertfile, fun() -> ?SSL_PATH ++  "/" ++ binary_to_list(maps:get(<<"cacertfile">>, Ssl)) end),
			SslOpts2 = add_ssl_opts(SslOpts1, Ssl, certfile, fun() -> ?SSL_PATH ++ "/" ++ binary_to_list(maps:get(<<"certfile">>, Ssl)) end),
			SslOpts3 = add_ssl_opts(SslOpts2, Ssl, keyfile, fun() -> ?SSL_PATH ++  "/" ++ binary_to_list(maps:get(<<"keyfile">>, Ssl)) end),
			SslOpts4 = add_ssl_opts(SslOpts3, Ssl, depth, fun() -> maps:get(<<"depth">>, Ssl) end),
			SslOpts5 = add_ssl_opts(SslOpts4, Ssl, password, fun() -> binary_to_list(maps:get(<<"keyfile">>, Ssl)) end),
			SslOpts6 = add_ssl_opts(SslOpts5, Ssl, key, fun() -> binary_to_list(maps:get(<<"key">>, Ssl)) end),
			Opts ++ SslOpts6;		
		false -> Opts
	end.
	
add_ssl_opts(Opts, Ssl, Param, FunValue) -> 	
	ParamBin = erlang:atom_to_binary(Param, latin1),
	case maps:is_key(ParamBin, Ssl) of
		true -> [{Param, FunValue()} | Opts];
		false -> Opts
	end.

listen(IsSsl, Port, Opts) ->
    case IsSsl of
        true ->
            case ssl:listen(Port, Opts) of
                {ok, ListenSocket} ->
                    {ok, {ssl, ListenSocket}};
                {error, _} = Err ->
                    Err
            end;
        false ->
            gen_tcp:listen(Port, Opts)
    end.

accept({ssl, ListenSocket}, Timeout) ->
    try ssl:transport_accept(ListenSocket) of
        {ok, Socket} ->
            case ssl:ssl_accept(Socket, Timeout) of
                ok -> {ok, {ssl, Socket}};
                {error, _} = Err ->
					Err
            end;
        {error, _} = Err ->
			Err
            
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason}
    end;
accept(ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout).

recv({ssl, Socket}, Length, Timeout) ->
    ssl:recv(Socket, Length, Timeout);
recv(Socket, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout).

send({ssl, Socket}, Data) ->
    ssl:send(Socket, Data);
send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

close({ssl, Socket}) ->
    ssl:close(Socket);
close(Socket) ->
    gen_tcp:close(Socket).

port({ssl, Socket}) ->
    case ssl:sockname(Socket) of
        {ok, {_, Port}} ->
            {ok, Port};
        {error, _} = Err ->
            Err
    end;
port(Socket) ->
    inet:port(Socket).

peername({ssl, Socket}) ->
    ssl:peername(Socket);
peername(Socket) ->
    inet:peername(Socket).

setopts({ssl, Socket}, Opts) ->
    ssl:setopts(Socket, Opts);
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).

type({ssl, _}) ->
    ssl;
type(_) ->
    plain.


%% @doc Sends the data to the client. Method timeout treatment
send_data(Socket, Data) ->
	case send(Socket, Data) of
		{error, timeout} ->
			close(Socket),
			ems_logger:error("Error sending response: timeout."),
			timeout;
        {error, closed} -> ok;
		{error, PosixError} ->
			close(Socket),
			PosixErrorDescription = posix_error_description(PosixError),
			ems_logger:error("Error sending response: ~p.", [PosixErrorDescription]),
			PosixError;
		ok -> ok
	end.

%% @doc Translates the code into a more useful description
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
posix_error_description(Code) when is_atom(Code) -> atom_to_list(Code);
posix_error_description(Code) -> Code.
