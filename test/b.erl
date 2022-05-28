-module(b).

-export([start/0]).

start() ->
    e:b_calls_me(),
    Module = get_module(),
    Module:test().

get_module() ->
    c.
