-module(socks_client).
-include_lib("kernel/include/logger.hrl").
-include_lib("socks_er/include/socks.hrl").
-export([connect_to_proxy/4, connect_to_target/4, encode_connect/3]).


connect_to_proxy(ProxyHost, ProxyPort, Uname, Passwd) ->
    case gen_tcp:connect(ProxyHost, ProxyPort, [binary, {active, false}], 10000) of
        {ok, Socket} -> 
            ?LOG_DEBUG("TCP connection established, socket: ~p", [Socket]),
            ok = negotiate(Socket),
            ok = authenticate(Socket, Uname, Passwd),
            {ok, Socket};
        E ->
            ?LOG_ERROR("TCP connection to the server not established, reason: ~p", [E]),
            E
    end.

negotiate(Socket) ->
    case gen_tcp:send(Socket, <<?SOCKS_VER:8, 1:8, ?UNAME_PASSWD_METHOD:8>>) of
        ok ->
            case gen_tcp:recv(Socket, 2, 5000) of
                {ok, <<?SOCKS_VER:8, ?UNAME_PASSWD_METHOD:8>>} ->
                    ?LOG_DEBUG("Negotiation succeeded"),
                    ok;
                {ok, Other} ->
                    gen_tcp:close(Socket),
                    ?LOG_ERROR("Negotiation failed, received response: ~p", [Other]);
                Error -> 
                    gen_tcp:close(Socket),
                    ?LOG_ERROR("Error occured during SOCKS5 Method negotiation: ~p", [Error]),
                    Error
            end;
        E -> 
            ?LOG_ERROR("Error sending negotiate request to the upstream"),
            gen_tcp:close(Socket),
            E
    end.

authenticate(Socket, Uname, Passwd) when is_binary(Uname) andalso is_binary(Passwd) -> 
    case gen_tcp:send(Socket, <<?UNAME_PASSWD_SUBNEG_VER:8, (byte_size(Uname)), Uname/binary, 
                              (byte_size(Passwd)), Passwd/binary>>) of
        ok ->
            case gen_tcp:recv(Socket, 2, 5000) of
                {ok, <<?UNAME_PASSWD_SUBNEG_VER, 0>>} ->
                    ?LOG_DEBUG("UNAME, PASSWD authenticated by the server successfuly"),
                    ok;
                {ok, Other} -> 
                    ?LOG_ERROR("UNAME, PASSWD authentication  failed: ~p", [Other]),
                    gen_tcp:close(Socket),
                    error;
                E ->
                    ?LOG_ERROR("Error authenticating uname/passwd ~p", [E]),
                    gen_tcp:close(Socket),
                    E
            end;
        E ->
            ?LOG_ERROR("Error sending auth request to the upstream proxy: ~p", [E]),
            E
    end.
    
connect_to_target(Socket, Atype, Host, Port) ->
    case gen_tcp:send(Socket, encode_connect(Atype, Host, Port)) of
        ok ->
            case gen_tcp:recv(Socket, 0, 5000) of
                {ok, <<?SOCKS_VER, ?SUCCEEDED, ?RSV, _Atyp:8, _BindAdrPort/binary>> = Repl} -> 
                    {ok, Repl};
                {ok, Repl} -> 
                    ?LOG_ERROR("Connecting to target failed: ~p", [Repl]),
                    gen_tcp:close(Socket),
                    {error, Repl};
                Error ->
                    ?LOG_ERROR("Error connecting to the target: ~p", [Error]),
                    gen_tcp:close(Socket),
                    Error
            end;
        E ->
            ?LOG_ERROR("Error sending connect request to the upstream proxy: ~p", [E]),
            gen_tcp:close(Socket)
    end.

encode_connect(?ATYP_DOMAIN_NAME, HostName, Port) ->    
    catch <<?SOCKS_VER, ?SOCKS_CONN, ?RSV, ?ATYP_DOMAIN_NAME, 
      (byte_size(HostName)), HostName/binary, Port:16>>;
encode_connect(?ATYP_IP4, {O1, O2, O3, O4}, Port) ->
    catch <<?SOCKS_VER, ?SOCKS_CONN, ?RSV, ?ATYP_IP4, 
      O1:8, O2:8, O3:8, O4:8, Port:16>>;
encode_connect(?ATYP_IP6, {H1, H2, H3, H4, H5, H6, H7, H8}, Port) ->
    catch <<?SOCKS_VER, ?SOCKS_CONN, ?RSV, ?ATYP_IP4, 
            H1:16, H2:16, H3:16, H4:16, H5:16, H6:16, H7:16, H8:16, Port:16>>;
encode_connect(Atype, Host, Port) ->
    ?LOG_ERROR("Incorrect address: ~p, ~p, ~p", [Atype, Host, Port]),
    {error, incorrect_address}.


