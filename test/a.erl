-module(a).

-export([start/0]).

start() ->
    b:start().
