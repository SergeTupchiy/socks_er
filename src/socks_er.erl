-module(socks_er).
-behaviour(application).
-behaviour(supervisor).
-export([start/0]). %% api for starting with erl -s socks_er 
-export([start/2, stop/1]). %% application
-export([start_link/0, init/1]). %% supervisor

%% dev/testing defaults to be used with Dante socks5 upstream server run locally via Docker
-define(UNAME, <<"test">>).
-define(PASSWD, <<"test1">>).
-define(DEFAULT_UPSTREAM_HOST, "localhost").
-define(DEFAULT_UPSTREAM_PORT, "1080").
-define(DEFAULT_LISTEN_PORT, "1081").
-define(DEFAULT_NUM_ACCEPTORS, "100").
-define(DEFAULT_MAX_CONNECTIONS, "1024"). %% the same as ranch default value
-define(DEFAULT_CONN_TIMEOUT_MS, "300000"). %% 5 minutes


start() ->  
    {ok, _} = application:ensure_all_started(socks_er, permanent),
    ok.

%% application
start(_StartType, _StartArgs) ->
    ok = application:ensure_started(ranch),
    ProxyHost = os:getenv("U_HOST", ?DEFAULT_UPSTREAM_HOST),
    ProxyPort = erlang:list_to_integer(os:getenv("U_PORT", ?DEFAULT_UPSTREAM_PORT)),
    ProxyUname = erlang:iolist_to_binary(os:getenv("U_UNAME", ?UNAME)),
    ProxyPasswd = erlang:iolist_to_binary(os:getenv("U_PASSWD", ?PASSWD)),
    ListenPort = erlang:list_to_integer(os:getenv("LISTEN_PORT", ?DEFAULT_LISTEN_PORT)),
    NumAcceptors = erlang:list_to_integer(os:getenv("NUM_ACCEPTORS", ?DEFAULT_NUM_ACCEPTORS)),
    MaxConnections = erlang:list_to_integer(os:getenv("MAX_CONNECTIONS", ?DEFAULT_MAX_CONNECTIONS)),
    ConnTimeout = erlang:list_to_integer(os:getenv("CONN_TIMEOUT", ?DEFAULT_CONN_TIMEOUT_MS)),
    {ok, _} = ranch:start_listener(socks_server, ranch_tcp, 
                                   #{socket_opts => [{port, ListenPort}], 
                                     max_connections => MaxConnections,
                                     num_acceptors => NumAcceptors},
                                   socks_server, [ProxyHost, ProxyPort, ProxyUname, ProxyPasswd, ConnTimeout]),
    start_link().

stop(_State) ->
    ranch:stop_listener(socks_server),
    ok.

%% supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.
