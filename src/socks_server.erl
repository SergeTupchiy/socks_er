-module(socks_server).
-description("Ranch protocol for SOCKS5 server that forwards all requests to upstream SOCKS5 proxy server").
-behaviour(ranch_protocol).
-include_lib("kernel/include/logger.hrl").
-include_lib("socks_er/include/socks.hrl").
-export([start_link/4]).
-export([init/3]).
-export([forward/4]).

start_link(Ref, _Socket, Transport, Opts) ->
	Pid = erlang:spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    socks_negotiate(Socket, Transport, Opts).

socks_negotiate(Socket, Transport, Opts) ->
    case Transport:recv(Socket, 3, 5000) of
        {ok, <<?SOCKS_VER:8, 1:8, ?NO_AUTH:8>>} ->
            ?LOG_DEBUG("Client negotiated no auth method, and it will be accepted"),
            Transport:send(Socket, <<?SOCKS_VER, ?NO_AUTH>>),
            socks_connect(Socket, Transport, Opts);
        {ok, Other} ->
            ?LOG_ERROR("Unsupported authentication method received during negotiation: ~p", [Other]),
            ok = Transport:close(Socket);
        E -> 
            ?LOG_ERROR("Error receiving authentication method negotiation  request: ~p", [E]),
            ok = Transport:close(Socket)
    end.

socks_connect(Socket, Transport, [ProxyHost, ProxyPort, ProxyUname, ProxyPasswd, ConnTimeout]) ->
    case Transport:recv(Socket, 0, 5000) of 
        {ok, <<?SOCKS_VER:8, ?SOCKS_CONN:8, ?RSV:8, Atype:8, HostPort/binary>>} ->
            {Host, Port} = host_port(Socket, Transport, Atype, HostPort),
            {ok, ProxySocket} = connect_to_upstream(Socket, Transport,
                                                    ProxyHost, ProxyPort, ProxyUname, ProxyPasswd),
            case socks_client:connect_to_target(ProxySocket, Atype, Host, Port) of
                {ok, Repl} ->
                    Transport:send(Socket, Repl),
                    BackForwarder = erlang:spawn_link(?MODULE, forward, 
                                                      [ProxySocket, Socket, Transport, self()]),
                    ?LOG_INFO("Backforwarder spawned, pid: ~p", [BackForwarder]),
                    loop(Socket, Transport, ProxySocket, erlang:monotonic_time(millisecond), ConnTimeout);
                {error, Repl} ->
                    Transport:send(Socket, Repl),
                    ok = Transport:shutdown(Socket, read_write);
                _ -> 
                    ok = Transport:close(Socket)
            end;
       Other -> 
            ?LOG_ERROR("Error receiving incomming CONNECT request: ~p", [Other]),
            Transport:close(Socket)
    end.

host_port(_, _, ?ATYP_IP4, <<O1:8, O2:8, O3:8, O4:8, Port:16>>) ->
    Host = {O1, O2, O3, O4},
    ?LOG_INFO("Host to connect: ~p, port: ~p, address type: IPv4", [Host, Port]),
    {Host, Port};
host_port(_, _, ?ATYP_IP6, <<H1:16, H2:16, H3:16, H4:16, H5:16, H6:16, H7:16, H8:16, Port:16>>) ->
    Host = {H1, H2, H3, H4, H5, H6, H7, H8},
    ?LOG_INFO("Host to connect: ~p, port: ~p, address type: IPv6", [Host, Port]),
    {Host, Port};
host_port(_, _, ?ATYP_DOMAIN_NAME, <<AddrSize:8, HostPort/binary>>) ->
    Host = binary:part(HostPort, 0, AddrSize),
    <<Port:16>> = binary:part(HostPort, AddrSize, 2),
    ?LOG_INFO("Host to connect: ~p, port: ~p, addres type: domain name", [Host, Port]),
    {Host, Port};
host_port(Socket, Transport, Atype, HostPort) ->
    ?LOG_ERROR("Wrong address format, address type: ~p, host/port: ~p", [Atype, HostPort]),
    ok = Transport:close(Socket).

connect_to_upstream(Socket, Transport, ProxyHost, ProxyPort, ProxyUname, ProxyPasswd) ->
    case socks_client:connect_to_proxy(ProxyHost, ProxyPort, ProxyUname, ProxyPasswd) of
        {ok, ProxySocket} ->
            {ok, ProxySocket};
        _ ->
            ok = Transport:close(Socket),
            {error, upstream_connection_error}
    end.

loop(Socket, Transport, ProxySocket, LastRecv, Timeout) ->
    Now = erlang:monotonic_time(millisecond),
    LastRecv1 = maybe_upstream_last_recv(LastRecv),
    case Now - LastRecv1 - Timeout of
         X when X > 0 ->
            ?LOG_INFO("Closing sockets as no data received from both ends for timeout of : ~p", [Timeout]),
            ok = Transport:close(Socket),
            ok = gen_tcp:close(ProxySocket);
        _ ->
            case Transport:recv(Socket, 0, 5000) of 
                {ok, Data} ->
                    ?LOG_DEBUG("Incoming request received"),
                    gen_tcp:send(ProxySocket, Data),
                    loop(Socket, Transport, ProxySocket, erlang:monotonic_time(millisecond), Timeout);
                {error, timeout} ->
                    loop(Socket, Transport, ProxySocket, erlang:monotonic_time(millisecond), Timeout);
                {error, closed} ->
                    ?LOG_DEBUG("incoming connection socket is closed"),
                    ok = Transport:close(Socket),
                    ok = gen_tcp:close(ProxySocket),
                    {error, closed};
                E ->
                    ?LOG_ERROR("Error receiving incoming requests: ~p", [E]),
                    ok = Transport:close(Socket),
                    ok = gen_tcp:close(ProxySocket),
                    E
            end
    end.

maybe_upstream_last_recv(LastRecv) ->
    receive 
        {lastrecv, Ts} ->
            case Ts - LastRecv of X when X > 0 -> Ts; _ -> LastRecv end
    after 0 ->
            LastRecv
    end.

forward(FromSock, ToSock, Transport, ParentPid) ->
    case gen_tcp:recv(FromSock, 0, 5000) of
        {ok, B} ->
            ?LOG_DEBUG("Response from proxy received"),
            ParentPid ! {lastrecv, erlang:monotonic_time(millisecond)},
            Transport:send(ToSock, B),  
            forward(FromSock, ToSock, Transport, ParentPid);
        {error, timeout} ->
            forward(FromSock, ToSock, Transport, ParentPid);
        {error, closed} ->
            ?LOG_DEBUG("Upstream Proxy socket is closed"), 
            ok = gen_tcp:close(FromSock),
            ok = Transport:close(ToSock), 
            {error, closed};
        E ->
            ?LOG_ERROR("Error receiving response from proxy: ~p", [E]),
            ok = gen_tcp:close(FromSock),
            ok =Transport:close(ToSock),
            E
    end.
