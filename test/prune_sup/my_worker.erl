%%
%% Copyright (c) 2026 Peter M <petermm@gmail.com>
%% All rights reserved.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% @doc A worker module only referenced as a callback in a supervisor
%% child spec. No module directly imports this module.
-module(my_worker).

-export([start_link/0]).

start_link() ->
    ok.
