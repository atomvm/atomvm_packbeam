%%
%% Copyright (c) 2026 Peter M <petermm@gmail.com>
%% All rights reserved.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% @doc A supervisor-like module whose init returns child specs.
%% The child module (my_worker) is only referenced as an atom.
-module(my_sup).

-export([start_link/0, init/1]).

start_link() ->
    init([]).

init(_Args) ->
    ChildSpecs = [#{id => my_worker, start => {my_worker, start_link, []}}],
    {ok, {#{}, ChildSpecs}}.
