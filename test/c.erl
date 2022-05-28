-module(c).

-export([test/0]).

test() ->
    Literal = get_literal(),
    Module = maps:get(module, Literal),
    Module:c_calls_me().

get_literal() ->
    #{
        module => f
    }.
