%%
%% Copyright (c) 2023 dushin.net
%% All rights reserved.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

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
