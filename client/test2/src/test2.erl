
-module(test2).

-export([start/0]).

start() ->
 ok = application:start(test2).
