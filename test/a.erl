%%
%% Copyright (c) 2023 dushin.net
%% All rights reserved.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(a).

-export([start/0]).

start() ->
    b:start().
