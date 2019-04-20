-module(helper).
-compile(export_all).
     
request() ->
    <<"GET / HTTP/1.1", 
      "\r\n",
      "Host: example.com", "\r\n", 
      "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/72.0.3626.109 Safari/537.36", "\r\n",
      "Accept: */*",
      "\r\n",
      "\r\n">>.
