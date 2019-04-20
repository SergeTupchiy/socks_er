-module(socks_server_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("socks_er/include/socks.hrl").
%% Test requires access to the Internet and socks5 server listening on localhost:1080, 
%% see test_env and README for instructions on how to run Dante server via Docker

server_test_() ->        
    {setup, 
     fun() -> {ok, _} = application:ensure_all_started(socks_er),
              {ok, Socket} = gen_tcp:connect("localhost", 1081, [binary, {active, false}], 5000),
              Socket 
     end,
     fun(Socket) -> ok = application:stop(socks_er), gen_tcp:close(Socket) end,
     fun(Socket) ->
             [{"negotiate", ?_assertMatch({ok, <<?SOCKS_VER, ?NO_AUTH>>}, negotiate(Socket))},
              {"connect to target", 
               ?_assertMatch({ok, <<?SOCKS_VER, ?SUCCEEDED, ?RSV, _Atyp:8, _BindAdrPort/binary>>}, target(Socket))},
              {"send request and verifiy response", 
               ?_assertMatch({ok, <<"HTTP/1.1 200 OK\r\n", _/binary>>}, response(Socket))}] 
     end}.


negotiate(Socket) ->
    ok = gen_tcp:send(Socket, <<?SOCKS_VER:8, 1:8, ?NO_AUTH:8>>),
    gen_tcp:recv(Socket, 2, 5000).

target(Socket) ->
    ok = gen_tcp:send(Socket, 
                      socks_client:encode_connect(?ATYP_DOMAIN_NAME, <<"example.com">>, 80)),
    gen_tcp:recv(Socket, 0, 5000).

response(Socket) ->
    ok = gen_tcp:send(Socket, helper:request()),
    gen_tcp:recv(Socket, 0, 5000).
