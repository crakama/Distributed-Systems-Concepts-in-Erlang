-module(aserver_file).
-export([hello/0]).

hello() ->
  receive
    X -> io:format("aaa! suprice, a message: ~s~n", [X])
  end.
