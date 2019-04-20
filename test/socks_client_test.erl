-module(socks_client_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("socks_er/include/socks.hrl").
%% Test requires access to the Internet and  socks5 server listening on localhost:1080, 
%% see test_env and README for instructions on how to run Dante server via Docker

connect_test_() ->
    {foreach,
     fun() -> {ok, Socket} = socks_client:connect_to_proxy("localhost", 1080, 
                                                           <<"test">>,
                                                           <<"test1">>),
              Socket end,
     fun gen_tcp:close/1,
     [fun(Socket) -> ?_assertMatch({ok, <<"HTTP/1.1 200 OK\r\n", _/binary>>}, domain_name(Socket)) end
     , fun(Socket) -> ?_assertMatch({ok, <<"HTTP/1.1 200 OK\r\n", _/binary>>}, ip4(Socket)) end
     %%, fun(Socket) -> ?_assertMatch({ok, <<"HTTP/1.1 200 OK\r\n", _/binary>>}, ip6(Socket)) end
     ]
    }.
       

domain_name(Socket) ->
    {ok, <<?SOCKS_VER, ?SUCCEEDED, ?RSV, _Atyp:8, _BindAdrPort/binary>>} = 
        socks_client:connect_to_target(Socket, ?ATYP_DOMAIN_NAME, <<"example.com">>, 80),
    ok = gen_tcp:send(Socket, helper:request()),
    gen_tcp:recv(Socket, 0, 10000).

ip4(Socket) ->
    {ok, Ip4} = inet:getaddr("example.com", inet),
    {ok, <<?SOCKS_VER, ?SUCCEEDED, ?RSV, _Atyp:8, _BindAdrPort/binary>>} = 
        socks_client:connect_to_target(Socket, ?ATYP_IP4, Ip4, 80),
    ok = gen_tcp:send(Socket, helper:request()),
    gen_tcp:recv(Socket, 0, 10000).

%%ip6(Socket) ->
%%    {ok, Ip6} = inet:getaddr("example.com", inet6),
%%    {ok, <<?SOCKS_VER, ?SUCCEEDED, ?RSV, _Atyp:8, _BindAdrPort/binary>>} = 
%%        socks_client:connect_to_target(Socket, ?ATYP_IP6, Ip6, 80),
%%    ok = gen_tcp:send(Socket, helper:request()),
%%    gen_tcp:recv(Socket, 0, 10000).
