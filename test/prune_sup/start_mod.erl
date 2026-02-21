%%
%% Copyright (c) 2026 Peter M <petermm@gmail.com>
%% All rights reserved.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% @doc A start module that starts a supervisor.
%% The supervisor module (my_sup) is referenced via atoms and imports.
-module(start_mod).

-export([start/0]).

start() ->
    Sup = my_sup,
    Sup:start_link().
