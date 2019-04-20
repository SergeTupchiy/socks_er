socks_er
=====

socks_er is a simplistic Socks5 proxy forwarder created with a single aim to accept incoming SOCKS5 connections with no auth and forward them
to another (upstream) SOCKS5 proxy server that does require user name/password auth.  
It is writen in Erlang and uses Ranch for handling incoming connections.  
One of the possible use cases of socks_er is running headless chrome with a proxy that requires auth
(it is not possible to pass uname/passwd of proxy server as command line arguments when starting headless Chrome).


Usage
-----

Run socks_er with rebar3:

    $ rebar3 shell

Or simply using erl:

    $ rebar3 compile
    $ rebar3 compile && ERL_LIBS=_build/default/lib/ erl -config sys.config -s socks_er

you can also provide ENV variables as erl flags:
    
    $ rebar3 compile && ERL_LIBS=_build/default/lib/ erl -config sys.config -s socks_er -env LISTEN_PORT 1080 -env U_HOST some.host -env U_UNAME user -env U_PASSWD pass  

After that, you can run chrome:
    
    $ google-chrome --proxy-server=socks5://localhost:1080 --remote-debugging-port=9222 --headless


Config
-----

All configuration parameters must be provided via OS ENV variables, some of them have default values:

    +-----------------+------------------------------------------------------------------------------+----------+---------------------------------------+
    | env             | description                                                                  | required | default value                         |
    +-----------------+------------------------------------------------------------------------------+----------+---------------------------------------+
	| U_HOST          | upstream proxy server host                                                   | yes      |                                       |
	+-----------------+------------------------------------------------------------------------------+----------+---------------------------------------+
	| U_PORT          | upstream proxy server port                                                   | no       | 1080                                  |
	+-----------------+------------------------------------------------------------------------------+----------+---------------------------------------+
	| U_UNAME         | upstream proxy user name                                                     | yes      |                                       |
	+-----------------+------------------------------------------------------------------------------+----------+---------------------------------------+
	| U_PASSWD        | upstream proxy password                                                      | yes      |                                       |
	+-----------------+------------------------------------------------------------------------------+----------+---------------------------------------+
	| LISTEN_PORT     | listening port for incoming connections                                      | no       | 1081                                  |
    |                 |                                                                              |          |  (not 1080, as in dev/testing         |
    |                 |                                                                              |          |  environment, local upstream server   | 
    |                 |                                                                              |          |  is listening on localhost:1080)      |
	+-----------------+------------------------------------------------------------------------------+----------+---------------------------------------+
	| NUM_ACCEPTORS   | Ranch acceptors pool size                                                    | no       | 100                                   |
	+-----------------+------------------------------------------------------------------------------+----------+---------------------------------------+
	| MAX_CONNECTIONS | Ranch max concurrent connections value                                       | no       | 1024                                  |
	+-----------------+------------------------------------------------------------------------------+----------+---------------------------------------+
	|                 | Max time in milliseconds before closing  both connections                    | yes      | 300000 (5 minutes)                    |
	| CONN_TIMEOUT    | (incoming and upstream proxy) in case no data is received in both directions |          |                                       |
	|                 | (from incoming accepted connection as well as upstream proxy connection).    |          |                                       |
	|                 | Note: this a safety measure to make sure connection is closed                |          |                                       |
	|                 | and resources are released in case both parties keep connection open         |          |                                       |
	|                 | but do not send any data.                                                    |          |                                       |
	|                 | If at least one side has closed connection from its side,                    |          |                                       |
	|                 | the server with close both connection as well.                               |          |                                       |
	+-----------------+------------------------------------------------------------------------------+----------+---------------------------------------+

Docker
-----

Build docker image and run:

    $ sudo docker build . -t socks_er
    $ sudo docker run --net=host --env LISTEN_PORT=1080 --env U_HOST=upsteram.host \
        --env U_UNAME=username --env U_PASSWD=pass socks_er
    
Tests
----

Tests require access to the Internet(example.com) and local SOCKS5 upstream server. Dante docker-compose file is included to the repository,  
the tests can be run as follows:

    $ sudo docker-compose -f test_userver/dante.yaml up -d \
        && rebar3 eunit \
        && sudo docker-compose -f test_userver/dante.yaml down
    
Load Testing
----

Load testing can be done using tcpkali (https://github.com/satori-com/tcpkali).

    $ tcpkali --write-combine=off -c 100 -r 100 --delay-send=1ms \
    -e1 "\x05\x01\x00" -e1 "\x05\x01\x00\x03\x0B\example.com\x00\x50" \
    -em "GET / HTTP/1.1\r\nHost: example.com\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/72.0.3626.109 Safari/537.36\r\nAccept: */*\r\n\r\n" \
    localhost:1081
    
Note: test results also depend on upstream proxy performance. 


