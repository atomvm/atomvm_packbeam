%%
%% Copyright (c) 2023 dushin.net
%% All rights reserved.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(b).

-export([start/0]).

start() ->
    e:b_calls_me(),
    Module = get_module(),
    Module:test().

get_module() ->
    c.
