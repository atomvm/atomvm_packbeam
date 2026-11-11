%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(packbeam_function_prune_tests).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    B = fixture(
        pf_basic,
        "-export([start/0,dead/0]). start()->live(3). live(X)->{ok,X}. dead()->[dead_literal,123456789]."
    ),
    {Out, Report} = packbeam_prune:run([B], [], [{pf_basic, start, 0}], #{}),
    [New] = Out,
    ?assertEqual([], maps:get(warnings, Report)),
    ?assertEqual([{start, 0}], exports(New)),
    ?assert(byte_size(New) < byte_size(B)),
    ?assertEqual({ok, 3}, execute(pf_basic, New, start, [])),
    ?assertEqual(nomatch, binary:match(literals(New), <<"dead_literal">>)).

reference_test() ->
    A = fixture(
        pf_app,
        "-export([start/0,callback/0,dead/0]). start()->pf_lib:run(). callback()->42. dead()->bad."
    ),
    L = fixture(pf_lib, "-export([run/0,dead/0]). run()->pf_app:callback(). dead()->unused."),
    {[New], _} = packbeam_prune:run([A], [L], [{pf_app, start, 0}], #{}),
    ?assertEqual([{callback, 0}, {start, 0}], exports(New)),
    {Bundled, _} = packbeam_prune:run([A, L], [], [{pf_app, start, 0}], #{}),
    ?assertEqual(2, length(Bundled)),
    ?assertEqual([{run, 0}], exports(lists:nth(2, Bundled))).

known_module_dynamic_function_test() ->
    A = fixture(
        pf_dynamic,
        "-export([start/0]). start()->F=list_to_atom(erlang:atom_to_list(erlang:get(key))),pf_target:F()."
    ),
    T = fixture(pf_target, "-export([one/0,two/0]). one()->1. two()->2."),
    U = fixture(pf_unused, "-export([dead/0]). dead()->dead."),
    {Out, R} = packbeam_prune:run([A, T, U], [], [{pf_dynamic, start, 0}], #{}),
    ?assertEqual(2, length(Out)),
    ?assert(lists:any(fun(#{scope := S}) -> S =:= {module, pf_target} end, maps:get(warnings, R))).

%% An unbounded dispatch retains the exports of the modules the analyzed code
%% names. A module whose name only ever comes from runtime data -- here a term
%% decoded at run time -- is not discovered, and is trimmed.
unknown_module_test() ->
    A = fixture(
        pf_unknown,
        "-export([start/0]). start()->M=binary_to_term(erlang:get(key)),F=erlang:get(function),M:F()."
    ),
    T = fixture(pf_target, "-export([one/0,two/0]). one()->1. two()->2."),
    {[Only], R} = packbeam_prune:run([A, T], [], [{pf_unknown, start, 0}], #{}),
    ?assertEqual([{start, 0}], exports(Only)),
    ?assert(lists:any(fun(#{scope := S}) -> S =:= named_modules end, maps:get(warnings, R))),
    %% Naming the module and the function keeps that entry point; the other
    %% export, whose name the code never produces, still goes.
    Named = fixture(
        pf_named,
        "-export([start/0]). start()->erlang:put(k,{pf_target,one}),M=binary_to_term(erlang:get(key)),F=erlang:get(function),M:F()."
    ),
    {[_, Kept], _} = packbeam_prune:run([Named, T], [], [{pf_named, start, 0}], #{}),
    ?assertEqual([{one, 0}], exports(Kept)).

%% A fun built from runtime data (erl_parse:normalise/1 builds `fun M:F/A'
%% from a parse tree) can only name a module the code names: like an apply
%% with an unknown module, it keeps those, not the whole package.
unknown_make_fun_test() ->
    A = fixture(
        pf_make_fun,
        "-export([start/0]). start()->{M,F,Ar}=binary_to_term(erlang:get(key)),erlang:make_fun(M,F,Ar)."
    ),
    T = fixture(pf_target, "-export([one/0,two/0]). one()->1. two()->2."),
    {Out, R} = packbeam_prune:run([A, T], [], [{pf_make_fun, start, 0}], #{}),
    ?assertEqual(1, length(Out)),
    ?assert(lists:any(fun(#{scope := S}) -> S =:= named_modules end, maps:get(warnings, R))).

%% A module retained whole for an unknown dispatch is kept without being
%% analyzed: what its functions call must be kept too, even when the callee's
%% name is not an atom the code can produce.
named_module_callees_test() ->
    A = fixture(
        pf_named_app,
        "-export([start/0]). start()->erlang:put(k,{pf_named_x,f}),M=binary_to_term(erlang:get(key)),F=erlang:get(function),M:F()."
    ),
    X = fixture(pf_named_x, "-export([f/0]). f()->pf_named_y:g()."),
    Y = fixture(pf_named_y, "-export([g/0,h/0]). g()->ok. h()->dead."),
    {Out, _} = packbeam_prune:run([A, X, Y], [], [{pf_named_app, start, 0}], #{}),
    ?assertEqual(3, length(Out)),
    ?assertEqual([{g, 0}], exports(lists:nth(3, Out))).

%% A retained module names the next one in its literals too, as a child spec
%% does: `{code_server, start_link, []}'.
named_module_literal_test() ->
    A = fixture(
        pf_named_app,
        "-export([start/0]). start()->erlang:put(k,{pf_named_x,f}),M=binary_to_term(erlang:get(key)),F=erlang:get(function),M:F()."
    ),
    X = fixture(
        pf_named_x,
        "-export([f/0]). f()->[M,F]=persistent_term:get(k,[pf_named_z,go]),M:F()."
    ),
    Z = fixture(pf_named_z, "-export([go/0]). go()->ok."),
    {Out, _} = packbeam_prune:run([A, X, Z], [], [{pf_named_app, start, 0}], #{}),
    ?assertEqual(3, length(Out)).

%% A function kept without being analyzed may call an analyzed one with
%% arguments the analysis never saw: the callee's branches for them, and what
%% they call, stay.
named_module_caller_arguments_test() ->
    A = fixture(
        pf_named_app,
        "-export([start/0]). start()->pf_named_y:g(ok),erlang:put(k,{pf_named_x,f}),M=binary_to_term(erlang:get(key)),F=erlang:get(function),M:F()."
    ),
    X = fixture(pf_named_x, "-export([f/0]). f()->pf_named_y:g(erlang:get(z))."),
    Y = fixture(
        pf_named_y,
        "-export([g/1]). g(ok)->one; g(_)->other(). other()->two."
    ),
    {Out, _} = packbeam_prune:run([A, X, Y], [], [{pf_named_app, start, 0}], #{}),
    ?assertEqual(3, length(Out)),
    ?assertEqual(two, execute(pf_named_y, lists:nth(3, Out), g, [other])).

closure_test() ->
    B = fixture(
        pf_fun,
        "-export([start/0,dead/0]). start()->F=make(7),F(5). make(X)->fun(Y)->X+Y end. dead()->fun()->unused end."
    ),
    {[New], _} = packbeam_prune:run([B], [], [{pf_fun, start, 0}], #{}),
    ?assertEqual(12, execute(pf_fun, New, start, [])),
    ?assertEqual([{start, 0}], exports(New)).

branches_test() ->
    B = fixture(
        pf_branches,
        "-export([start/0,choose/1,dead/0]). start()->choose(erlang:get(key)). choose(a)->one(); choose(b)->two(); choose(_)->other(). one()->{one,[1,2,3]}. two()->{two,<<1,2,3>>}. other()->other. dead()->dead."
    ),
    {[New], _} = packbeam_prune:run([B], [], [{pf_branches, start, 0}], #{}),
    ?assertEqual({one, [1, 2, 3]}, execute(pf_branches, New, choose, [a])),
    ?assertEqual({two, <<1, 2, 3>>}, execute(pf_branches, New, choose, [b])),
    ?assertEqual(other, execute(pf_branches, New, choose, [c])).

constant_return_test() ->
    A = fixture(pf_return, "-export([start/0]). start()->M=module(),M:run(). module()->pf_target."),
    T = fixture(pf_target, "-export([run/0,dead/0]). run()->ok. dead()->dead."),
    {Out, R} = packbeam_prune:run([A, T], [], [{pf_return, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{run, 0}], exports(lists:nth(2, Out))).

on_load_test() ->
    B = fixture(
        pf_load,
        "-on_load(setup/0). -export([start/0,dead/0]). setup()->persistent_term:put(pf_load,loaded),ok. start()->persistent_term:get(pf_load). dead()->dead."
    ),
    {[New], _} = packbeam_prune:run([B], [], [{pf_load, start, 0}], #{}),
    ?assertEqual(loaded, execute(pf_load, New, start, [])),
    persistent_term:erase(pf_load).

archive_test() ->
    Dir = "_build/prune_archive_test/",
    ok = filelib:ensure_dir(Dir ++ "pf_lib2/x"),
    A = fixture(pf_app, "-export([start/0,dead/0]). start()->pf_lib:run(). dead()->bad."),
    L = fixture(pf_lib, "-export([run/0,dead/0]). run()->ok. dead()->unused."),
    ok = file:write_file(Dir ++ "pf_app.beam", A),
    ok = file:write_file(Dir ++ "pf_lib.beam", L),
    Init = fixture(init, "-export([boot/1]). boot([_,M])->M:start()."),
    ok = file:write_file(Dir ++ "init.beam", Init),
    ok = packbeam_api:create(Dir ++ "lib.avm", [Dir ++ "pf_lib.beam", Dir ++ "init.beam"], #{
        lib => true
    }),
    {ok, Before} = file:read_file(Dir ++ "lib.avm"),
    ok = packbeam_api:create(Dir ++ "out.avm", [Dir ++ "pf_app.beam"], #{
        prune => functions, references => [Dir ++ "lib.avm"]
    }),
    [Only] = packbeam_api:list(Dir ++ "out.avm"),
    ?assertEqual(pf_app, packbeam_api:get_element_module(Only)),
    ?assertEqual([{start, 0}], exports(packbeam_api:get_element_data(Only))),
    ?assertEqual({ok, Before}, file:read_file(Dir ++ "lib.avm")),
    ok = packbeam_api:create(Dir ++ "out.avm", [Dir ++ "pf_app.beam", Dir ++ "lib.avm"], #{
        prune => functions
    }),
    [_, Lib, _Init] = packbeam_api:list(Dir ++ "out.avm"),
    ?assertEqual([{run, 0}], exports(packbeam_api:get_element_data(Lib))),
    %% A reference given twice is analyzed once.
    ok = packbeam_api:create(Dir ++ "out.avm", [Dir ++ "pf_app.beam"], #{
        prune => functions, references => [Dir ++ "lib.avm", Dir ++ "lib.avm"]
    }),
    [Again] = packbeam_api:list(Dir ++ "out.avm"),
    ?assertEqual(pf_app, packbeam_api:get_element_module(Again)),
    %% A bundled module shadows the reference's, as it does on the device.
    L2 = fixture(pf_lib, "-export([run/0,other/0]). run()->pf_lib:other(). other()->ok."),
    ok = file:write_file(Dir ++ "pf_lib2/pf_lib.beam", L2),
    ok = packbeam_api:create(
        Dir ++ "out.avm", [Dir ++ "pf_app.beam", Dir ++ "pf_lib2/pf_lib.beam"], #{
            prune => functions, references => [Dir ++ "lib.avm"]
        }
    ),
    [_, Shadowing] = packbeam_api:list(Dir ++ "out.avm"),
    ?assertEqual([{other, 0}, {run, 0}], exports(packbeam_api:get_element_data(Shadowing))),
    ?assertError(
        {duplicate_module, pf_lib},
        packbeam_prune:run([A, L], [L], [{pf_app, start, 0}], #{})
    ).

%% ExAtomVM marks the start module with the start flag alone. AtomVM loads it
%% as a module all the same, so pruning must analyze it: otherwise what it
%% calls is dropped.
start_flag_only_entry_test() ->
    Dir = "_build/prune_start_flag_test/",
    ok = filelib:ensure_dir(Dir ++ "x"),
    A = fixture(pf_app, "-export([start/0,dead/0]). start()->pf_lib:run(). dead()->bad."),
    L = fixture(pf_lib, "-export([run/0,dead/0]). run()->ok. dead()->unused."),
    Init = fixture(init, "-export([boot/1]). boot([_,M])->M:start()."),
    ok = file:write_file(Dir ++ "pf_app.beam", A),
    ok = file:write_file(Dir ++ "pf_lib.beam", L),
    ok = file:write_file(Dir ++ "init.beam", Init),
    ok = packbeam_api:create(Dir ++ "app.avm", [Dir ++ "pf_app.beam"]),
    %% The first entry follows the 24-byte header: size, then flags.
    {ok, <<Header:24/binary, Size:32, 3:32, Rest/binary>>} = file:read_file(Dir ++ "app.avm"),
    ok = file:write_file(Dir ++ "app.avm", <<Header/binary, Size:32, 1:32, Rest/binary>>),
    ok = packbeam_api:create(Dir ++ "lib.avm", [Dir ++ "pf_lib.beam", Dir ++ "init.beam"], #{
        lib => true
    }),
    ok = packbeam_api:create(Dir ++ "out.avm", [Dir ++ "app.avm", Dir ++ "lib.avm"], #{
        prune => functions
    }),
    Out = maps:from_list([
        {packbeam_api:get_element_module(E), packbeam_api:get_element_data(E)}
     || E <- packbeam_api:list(Dir ++ "out.avm")
    ]),
    ?assertEqual([{start, 0}], exports(maps:get(pf_app, Out))),
    ?assertEqual([{run, 0}], exports(maps:get(pf_lib, Out))).

%% An AVM appended to the AtomVM executable boots init with the escript
%% arguments, and init calls the start module's main/1.
escript_entry_test() ->
    Dir = "_build/prune_escript_test/",
    ok = filelib:ensure_dir(Dir ++ "x"),
    A = fixture(
        pf_esc, "-export([start/0,main/1,dead/0]). start()->ok. main(_)->pf_lib:run(). dead()->bad."
    ),
    L = fixture(pf_lib, "-export([run/0,dead/0]). run()->ok. dead()->unused."),
    Init = fixture(
        init,
        "-export([boot/1]).\n"
        "boot([<<\"-s\">>, escript, <<\"--\">>, _File | Args]) ->\n"
        "    case atomvm:get_start_beam(escript) of\n"
        "        {ok, B} ->\n"
        "            Size = byte_size(B),\n"
        "            Name = if Size > 5 -> case binary:part(B, Size - 5, 5) of\n"
        "                    <<\".beam\">> -> binary:part(B, 0, Size - 5); _ -> B end;\n"
        "                true -> B end,\n"
        "            M = binary_to_atom(Name, utf8),\n"
        "            case erlang:function_exported(M, main, 1) of\n"
        "                true -> M:main(Args);\n"
        "                false -> error\n"
        "            end;\n"
        "        _ -> error\n"
        "    end;\n"
        "boot([<<\"-s\">>, M]) -> M:start()."
    ),
    Atomvm = fixture(
        atomvm, "-export([get_start_beam/1]). get_start_beam(_) -> erlang:nif_error(undefined)."
    ),
    Binary = fixture(binary, "-export([part/3]). part(_, _, _) -> erlang:nif_error(undefined)."),
    ok = file:write_file(Dir ++ "pf_esc.beam", A),
    ok = file:write_file(Dir ++ "pf_lib.beam", L),
    ok = file:write_file(Dir ++ "init.beam", Init),
    ok = file:write_file(Dir ++ "atomvm.beam", Atomvm),
    ok = file:write_file(Dir ++ "binary.beam", Binary),
    Beams = [Dir ++ M ++ ".beam" || M <- ["pf_esc", "pf_lib", "init", "atomvm", "binary"]],
    ok = packbeam_api:create(Dir ++ "out.avm", Beams, #{
        prune => functions,
        diagnostics => fun(R) -> self() ! {warnings, maps:get(warnings, R)} end
    }),
    Out = maps:from_list([
        {packbeam_api:get_element_module(E), packbeam_api:get_element_data(E)}
     || E <- packbeam_api:list(Dir ++ "out.avm")
    ]),
    ?assertEqual([{main, 1}, {start, 0}], lists:sort(exports(maps:get(pf_esc, Out)))),
    ?assertEqual([{run, 0}], exports(maps:get(pf_lib, Out))),
    receive
        {warnings, Warnings} -> ?assertEqual([], Warnings)
    end.

%% AtomVM loads the first entry of a module's name: later copies are shadowed
%% and never run, so they are neither analyzed nor kept.
shadowed_module_test() ->
    Dir = "_build/prune_shadowed_test/",
    ok = filelib:ensure_dir(Dir ++ "x"),
    A = fixture(pf_app, "-export([start/0]). start()->pf_dup:run()."),
    First = fixture(pf_dup, "-export([run/0]). run()->pf_left:run()."),
    Second = fixture(pf_dup, "-export([run/0]). run()->pf_right:run()."),
    Left = fixture(pf_left, "-export([run/0]). run()->left."),
    Right = fixture(pf_right, "-export([run/0]). run()->right."),
    Init = fixture(init, "-export([boot/1]). boot([_,M])->M:start()."),
    ok = file:write_file(Dir ++ "pf_app.beam", A),
    ok = file:write_file(Dir ++ "pf_left.beam", Left),
    ok = file:write_file(Dir ++ "pf_right.beam", Right),
    ok = file:write_file(Dir ++ "init.beam", Init),
    ok = filelib:ensure_dir(Dir ++ "first/x"),
    ok = filelib:ensure_dir(Dir ++ "second/x"),
    ok = file:write_file(Dir ++ "first/pf_dup.beam", First),
    ok = file:write_file(Dir ++ "second/pf_dup.beam", Second),
    Beams = [
        Dir ++ P
     || P <- [
            "pf_app.beam",
            "first/pf_dup.beam",
            "second/pf_dup.beam",
            "pf_left.beam",
            "pf_right.beam",
            "init.beam"
        ]
    ],
    ok = packbeam_api:create(Dir ++ "out.avm", Beams, #{prune => functions}),
    Out = [
        {packbeam_api:get_element_module(E), packbeam_api:get_element_data(E)}
     || E <- packbeam_api:list(Dir ++ "out.avm")
    ],
    ?assertEqual([init, pf_app, pf_dup, pf_left], lists:sort([M || {M, _} <- Out])).

%% The start module's name reaches the analysis only through init's seed
%% arguments: analyzed as any value, it must still count as an atom the
%% program knows, or the call on it finds no target.
coarse_start_module_test() ->
    Dir = "_build/prune_coarse_test/",
    ok = filelib:ensure_dir(Dir ++ "x"),
    A = fixture(pf_app, "-export([start/0,dead/0]). start()->pf_lib:run(). dead()->bad."),
    L = fixture(pf_lib, "-export([run/0,dead/0]). run()->ok. dead()->unused."),
    Init = fixture(init, "-export([boot/1]). boot([_,M])->M:start()."),
    ok = file:write_file(Dir ++ "pf_app.beam", A),
    ok = file:write_file(Dir ++ "pf_lib.beam", L),
    ok = file:write_file(Dir ++ "init.beam", Init),
    ok = packbeam_api:create(
        Dir ++ "out.avm",
        [Dir ++ "pf_app.beam", Dir ++ "pf_lib.beam", Dir ++ "init.beam"],
        #{prune => functions, precision => coarse, diagnostics => fun(_) -> ok end}
    ),
    Out = maps:from_list([
        {packbeam_api:get_element_module(E), packbeam_api:get_element_data(E)}
     || E <- packbeam_api:list(Dir ++ "out.avm")
    ]),
    ?assertEqual([{start, 0}], exports(maps:get(pf_app, Out))),
    ?assertEqual([{run, 0}], exports(maps:get(pf_lib, Out))).

init_entry_test() ->
    Dir = "_build/prune_init_test/",
    ok = filelib:ensure_dir(Dir ++ "x"),
    B = fixture(init, "-export([boot/1,start/0]). boot(_)->ok. start()->unused."),
    ok = file:write_file(Dir ++ "init.beam", B),
    ok = packbeam_api:create(Dir ++ "out.avm", [Dir ++ "init.beam"], #{
        prune => functions
    }),
    [P] = packbeam_api:list(Dir ++ "out.avm"),
    ?assertEqual([{boot, 1}], exports(packbeam_api:get_element_data(P))).

driver_test() ->
    B = fixture(
        pf_driver,
        "-export([start/0,dead/0]). start()->open_port({spawn,\"gpio\"},[]). dead()->open_port({spawn,\"uart\"},[])."
    ),
    {_, R} = packbeam_prune:run([B], [], [{pf_driver, start, 0}], #{}),
    ?assertEqual(["gpio"], maps:get(drivers, R)),
    Suggestions = lists:flatten(packbeam_prune:driver_suggestions(R)),
    ?assertEqual(nomatch, string:find(Suggestions, "GPIO_PORT_DRIVER")),
    ?assertNotEqual(nomatch, string:find(Suggestions, "UART")),
    U = fixture(
        pf_driver_unknown, "-export([start/0]). start()->open_port(erlang:get(driver),[])."
    ),
    {_, UR} = packbeam_prune:run([U], [], [{pf_driver_unknown, start, 0}], #{}),
    ?assertEqual([], packbeam_prune:driver_suggestions(UR)).

finite_modules_test() ->
    A = fixture(
        pf_finite,
        "-export([start/0]). start()->M=case erlang:get(key) of a->pf_left;_->pf_right end,M:run()."
    ),
    L = fixture(pf_left, "-export([run/0,dead/0]). run()->left. dead()->dead."),
    R = fixture(pf_right, "-export([run/0,dead/0]). run()->right. dead()->dead."),
    {Out, Report} = packbeam_prune:run([A, L, R], [], [{pf_finite, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, Report)),
    ?assertEqual([[{start, 0}], [{run, 0}], [{run, 0}]], [exports(B) || B <- Out]).

%% A library that was not supplied cannot be called at all, so its absence is
%% reported instead of silently keeping every function of the output.
missing_library_test() ->
    A = fixture(
        pf_missing, "-export([start/0,callback/0]). start()->missing_lib:run(). callback()->ok."
    ),
    {[New], R} = packbeam_prune:run([A], [], [{pf_missing, start, 0}], #{}),
    ?assertEqual([{start, 0}], exports(New)),
    ?assertEqual(
        [{absent, missing_lib}], [Scope || #{scope := Scope} <- maps:get(warnings, R)]
    ),
    %% Nothing reachable opens a port, so the advice covers every driver.
    ?assertNotEqual([], packbeam_prune:driver_suggestions(R)).

%% Without init, the package runs next to an AtomVM library installed on the
%% device, which packbeam cannot see: a call into it returns, and may run the
%% closures and callback modules it is given, or send messages.
device_library_test() ->
    Dir = "_build/prune_device_library_test/",
    ok = filelib:ensure_dir(Dir ++ "x"),
    A = fixture(
        pf_app,
        "-export([start/0,dead/0]).\n"
        "start()->R=pf_device:run(fun(X)->{cb,X} end,pf_callback),{done,R,listen()}.\n"
        "listen()->pf_device:subscribe(self()),receive {'$gen_cast',X}->{event,X} end.\n"
        "dead()->bad."
    ),
    C = fixture(
        pf_callback, "-export([handle/1,other/0]). handle(X)->{handled,X}. other()->other."
    ),
    U = fixture(pf_unused, "-export([dead/0]). dead()->dead."),
    Device = fixture(
        pf_device,
        "-export([run/2,subscribe/1]). run(F,M)->{F(1),M:handle(2)}. subscribe(P)->P!{'$gen_cast',7}."
    ),
    ok = file:write_file(Dir ++ "pf_app.beam", A),
    ok = file:write_file(Dir ++ "pf_callback.beam", C),
    ok = file:write_file(Dir ++ "pf_unused.beam", U),
    Beams = [Dir ++ M ++ ".beam" || M <- ["pf_app", "pf_callback", "pf_unused"]],
    Self = self(),
    lists:foreach(
        fun(Options) ->
            ok = packbeam_api:create(
                Dir ++ "out.avm",
                Beams,
                Options#{diagnostics => fun(R) -> Self ! {warnings, maps:get(warnings, R)} end}
            ),
            Out = maps:from_list([
                {packbeam_api:get_element_module(E), packbeam_api:get_element_data(E)}
             || E <- packbeam_api:list(Dir ++ "out.avm")
            ]),
            ?assertEqual([pf_app, pf_callback], lists:sort(maps:keys(Out))),
            ?assertEqual([{start, 0}], exports(maps:get(pf_app, Out))),
            ?assertEqual(
                [{handle, 1}, {module_info, 0}, {module_info, 1}, {other, 0}],
                exports(maps:get(pf_callback, Out))
            ),
            receive
                {warnings, Warnings} ->
                    ?assertEqual(
                        [{module, pf_callback}, {unanalyzed, pf_device}],
                        lists:usort([S || #{scope := S} <- Warnings])
                    )
            end,
            Loaded = [{pf_device, Device} | maps:to_list(Out)],
            [{module, M} = code:load_binary(M, "pruned.beam", B) || {M, B} <- Loaded],
            try
                ?assertEqual({done, {{cb, 1}, {handled, 2}}, {event, 7}}, pf_app:start())
            after
                [code:delete(M) andalso code:purge(M) || {M, _} <- Loaded]
            end
        end,
        [
            %% What orbital passes for Gleam projects.
            #{prune => true, lib => false, start_module => pf_app},
            #{prune => functions, lib => true, keep => [{pf_app, start, 0}]}
        ]
    ).

spawn_test() ->
    B = fixture(
        pf_spawn,
        "-export([start/0,worker/0,dead/0]). start()->spawn(pf_spawn,worker,[]). worker()->ok. dead()->dead."
    ),
    {[New], _} = packbeam_prune:run([B], [], [{pf_spawn, start, 0}], #{}),
    ?assertEqual([{start, 0}, {worker, 0}], exports(New)).

literal_fun_test() ->
    A = fixture(pf_fun_ref, "-export([start/0]). start()->fun pf_target:run/0."),
    T = fixture(pf_target, "-export([run/0,dead/0]). run()->ok. dead()->dead."),
    {Out, _} = packbeam_prune:run([A, T], [], [{pf_fun_ref, start, 0}], #{}),
    ?assertEqual(2, length(Out)),
    ?assertEqual([{run, 0}], exports(lists:nth(2, Out))).

large_labels_test() ->
    Dead = [io_lib:format("f~p()->{dead,~p}. ", [I, I]) || I <- lists:seq(1, 1050)],
    B = fixture(
        pf_large,
        lists:flatten([
            "-compile(export_all). ",
            Dead,
            "start()->f1051(). f1051()->-123456789012345678901234567890."
        ])
    ),
    {[New], _} = packbeam_prune:run([B], [], [{pf_large, start, 0}], #{}),
    ?assertEqual(-123456789012345678901234567890, execute(pf_large, New, start, [])),
    ?assertEqual([{f1051, 0}, {start, 0}], exports(New)).

reference_erlang_spawn_test() ->
    B = fixture(
        pf_spawn_ref,
        "-export([start/0,worker/0,dead/0]). start()->spawn(pf_spawn_ref,worker,[]). worker()->ok. dead()->dead."
    ),
    E = fixture(erlang, "-export([spawn/3]). spawn(_,_,_)->erlang:nif_error(undefined)."),
    {[New], _} = packbeam_prune:run([B], [E], [{pf_spawn_ref, start, 0}], #{}),
    ?assertEqual([{start, 0}, {worker, 0}], exports(New)).

remote_spawn_test() ->
    B = fixture(
        pf_remote,
        "-export([start/0,worker/0,dead/0]). start()->spawn('remote@host',pf_remote,worker,[]). worker()->ok. dead()->dead."
    ),
    {[New], _} = packbeam_prune:run([B], [], [{pf_remote, start, 0}], #{}),
    ?assertEqual([{start, 0}, {worker, 0}], exports(New)).

%% OTP 29 native records name their atoms and default values by index, so
%% the definitions follow the tables when trimming renumbers them.
native_records_test_() ->
    case list_to_integer(erlang:system_info(otp_release)) >= 29 of
        true -> [fun native_records/0];
        false -> []
    end.
native_records() ->
    B = fixture(
        pf_records,
        "-export([dead/0,start/0]). -record #point{x, y = {origin, [1, 2]}}. "
        "dead()->{unused_a, unused_b, [3, 4, 5]}. "
        "start()->P = #point{x = 1}, {P#point.x, P#point.y}."
    ),
    {[New], _} = packbeam_prune:run([B], [], [{pf_records, start, 0}], #{}),
    ?assertEqual([{start, 0}], exports(New)),
    ?assertEqual({1, {origin, [1, 2]}}, execute(pf_records, New, start, [])).

compressed_and_litu_test() ->
    B = fixture(
        pf_literals,
        "-export([start/0,dead/0]). start()->{keep,[1,2,3]}. dead()->{discard,<<0:800>>}."
    ),
    {ok, _, Cs} = beam_lib:all_chunks(B),
    D = literals(B),
    Variants = [{"LitT", <<(byte_size(D)):32, (zlib:compress(D))/binary>>}, {"LitU", D}],
    lists:foreach(
        fun(Chunk) ->
            {ok, Input} = beam_lib:build_module([Chunk | lists:keydelete("LitT", 1, Cs)]),
            {[New], _} = packbeam_prune:run([Input], [], [{pf_literals, start, 0}], #{}),
            ?assertEqual({keep, [1, 2, 3]}, execute(pf_literals, New, start, [])),
            ?assertEqual(nomatch, binary:match(literals(New), <<"discard">>))
        end,
        Variants
    ).

keep_and_resources_test() ->
    Dir = "_build/prune_keep_test/",
    ok = filelib:ensure_dir(Dir ++ "x"),
    B = fixture(pf_keep, "-export([callback/0,dead/0]). callback()->ok. dead()->dead."),
    ok = file:write_file(Dir ++ "pf_keep.beam", B),
    ok = file:write_file(Dir ++ "asset.txt", <<"asset">>),
    ?assertError(
        no_pruning_roots,
        packbeam_api:create(Dir ++ "out.avm", [Dir ++ "pf_keep.beam"], #{
            lib => true, prune => functions
        })
    ),
    Self = self(),
    ok = packbeam_api:create(
        Dir ++ "out.avm",
        [Dir ++ "pf_keep.beam", Dir ++ "asset.txt"],
        #{
            lib => true,
            prune => functions,
            keep => [{pf_keep, callback, 0}],
            diagnostics => fun(R) -> Self ! {report, R} end
        }
    ),
    [P, Asset] = packbeam_api:list(Dir ++ "out.avm"),
    ?assertEqual([{callback, 0}], exports(packbeam_api:get_element_data(P))),
    ?assertEqual(false, packbeam_api:is_entrypoint(P)),
    ?assertEqual(false, packbeam_api:is_beam(Asset)),
    receive
        {report, Report} -> ?assertEqual([{pf_keep, callback, 0}], maps:get(reachable, Report))
    after 1000 -> error(no_report)
    end,
    ?assertError(
        {missing_root, {pf_keep, missing, 0}},
        packbeam_prune:run([B], [], [{pf_keep, missing, 0}], #{})
    ).

structured_arguments_test() ->
    A = fixture(
        pf_structure,
        "-export([start/0]). start()->dispatch({erlang:get(key),pf_target}),spawn(pf_receiver,run,[self(),pf_target]). dispatch(T)->M=element(2,T),M:run()."
    ),
    R = fixture(pf_receiver, "-export([run/2,dead/0]). run(P,M)->P!M:run(). dead()->dead."),
    T = fixture(pf_target, "-export([run/0,dead/0]). run()->ok. dead()->dead."),
    {Out, Report} = packbeam_prune:run([A, R, T], [], [{pf_structure, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, Report)),
    ?assertEqual([[{start, 0}], [{run, 2}], [{run, 0}]], [exports(B) || B <- Out]).

structured_map_test() ->
    A = fixture(
        pf_map,
        "-export([start/0]). start()->dispatch(#{module=>pf_target,value=>erlang:get(key)}). dispatch(T)->M=maps:get(module,T),M:run()."
    ),
    T = fixture(pf_target, "-export([run/0,dead/0]). run()->ok. dead()->dead."),
    {Out, Report} = packbeam_prune:run([A, T], [], [{pf_map, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, Report)),
    ?assertEqual([{run, 0}], exports(lists:nth(2, Out))).

closure_register_lifetime_test() ->
    A = fixture(
        pf_lifetime,
        "-export([start/0,dead/0]). start()->F=make(7),case F(pf_sum:sum([1,2,3])) of 13->0;_->1 end. make(X)->fun(Y)->X+Y end. dead()->dead."
    ),
    L = fixture(pf_sum, "-export([sum/1,dead/0]). sum([A,B,C])->A+B+C. dead()->dead."),
    {Out, R} = packbeam_prune:run([A, L], [], [{pf_lifetime, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{start, 0}], exports(hd(Out))).

fun_table_relocation_test() ->
    B = fixture(
        pf_fun_table,
        "-export([dead/1,start/1]). dead(X)->fun()->X end. start(X)->F=make(X),F(5). make(X)->fun(Y)->X+Y end."
    ),
    {[New], _} = packbeam_prune:run([B], [], [{pf_fun_table, start, 1}], #{}),
    ?assertEqual([{start, 1}], exports(New)),
    ?assertMatch({beam_file, pf_fun_table, _, _, _, _}, beam_disasm:file(New)),
    ?assertEqual(12, execute(pf_fun_table, New, start, [7])).

boot_dispatch_test() ->
    Dir = "_build/prune_boot_dispatch_test/",
    ok = filelib:ensure_dir(Dir ++ "x"),
    Init = fixture(
        init,
        "-export([boot/1]). boot([<<\"-s\">>, escript|_])->{ok,B}=atomvm:get_start_beam(escript),M=binary_to_atom(binary:part(B,0,byte_size(B)-5),utf8),M:main([]); boot([<<\"-s\">>,M])->M:start()."
    ),
    App = fixture(pr_boot_app, "-export([start/0,dead/0]). start()->42. dead()->unused."),
    Unused = fixture(pr_unused, "-export([start/0,main/1]). start()->unused. main(_)->unused."),
    Atomvm = fixture(
        atomvm, "-export([get_start_beam/1]). get_start_beam(_)->erlang:nif_error(undefined)."
    ),
    Binary = fixture(binary, "-export([part/3]). part(_,_,_)->erlang:nif_error(undefined)."),
    Lib = ["init", "pr_unused", "atomvm", "binary"],
    lists:foreach(fun({N, B}) -> ok = file:write_file(Dir ++ N ++ ".beam", B) end, [
        {"init", Init},
        {"pr_boot_app", App},
        {"pr_unused", Unused},
        {"atomvm", Atomvm},
        {"binary", Binary}
    ]),
    ok = packbeam_api:create(Dir ++ "lib.avm", [Dir ++ N ++ ".beam" || N <- Lib], #{
        lib => true
    }),
    Self = self(),
    ok = packbeam_api:create(Dir ++ "app.avm", [Dir ++ "pr_boot_app.beam", Dir ++ "lib.avm"], #{
        prune => true, diagnostics => fun(R) -> Self ! R end
    }),
    Ps = packbeam_api:list(Dir ++ "app.avm"),
    %% pr_boot_app has no main/1: the escript path reaches only the stubs.
    ?assertEqual(
        [pr_boot_app, init, atomvm, binary], [packbeam_api:get_element_module(P) || P <- Ps]
    ),
    ?assertEqual([{start, 0}], exports(packbeam_api:get_element_data(hd(Ps)))),
    receive
        Report ->
            ?assertEqual([], maps:get(warnings, Report)),
            ?assert(lists:member({init, boot, 1}, maps:get(reachable, Report)))
    after 1000 -> error(no_report)
    end,
    ok = packbeam_api:create(Dir ++ "reference.avm", [Dir ++ "pr_boot_app.beam"], #{
        prune => true, references => [Dir ++ "lib.avm"], diagnostics => fun(_) -> ok end
    }),
    [Only] = packbeam_api:list(Dir ++ "reference.avm"),
    ?assertEqual([{start, 0}], exports(packbeam_api:get_element_data(Only))).

nif_driver_test() ->
    B = fixture(
        pr_nif,
        "-export([start/0,dead/0]). start()->gpio:digital_write(2,high),esp:nvs_fetch_binary(atomvm,key). dead()->esp:rtc_slow_get_binary()."
    ),
    {_, R} = packbeam_prune:run([B], [], [{pr_nif, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assert(lists:member({gpio, digital_write, 2}, maps:get(nifs, R))),
    Advice = lists:flatten(packbeam_prune:driver_suggestions(R)),
    ?assertEqual(nomatch, string:find(Advice, "CONFIG_AVM_ENABLE_GPIO_NIFS=n")),
    ?assertEqual(nomatch, string:find(Advice, "CONFIG_AVM_ENABLE_NVS_NIFS=n")),
    ?assertNotEqual(nomatch, string:find(Advice, "CONFIG_AVM_ENABLE_RTC_SLOW_NIFS=n")),
    ?assertNotEqual(nomatch, string:find(Advice, "CONFIG_AVM_ENABLE_GPIO_PORT_DRIVER=n")).

try_dispatch_test() ->
    B = fixture(
        pr_try,
        "-export([start/0, live/0, dead/0, invoke/3]). start()->invoke(pr_try,live,[]). invoke(M,F,A)->put(initial,{M,F}),try apply(M,F,A) catch _:_ -> failed end. live()->42. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_try, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertNot(lists:member({dead, 0}, exports(Out))),
    {module, pr_try} = code:load_binary(pr_try, "pr_try.beam", Out),
    try
        ?assertEqual(42, pr_try:start())
    after
        code:delete(pr_try),
        code:purge(pr_try)
    end.

returned_dispatch_test() ->
    B = fixture(
        pr_returned,
        "-export([start/0, target/0, live/0, dead/0]). start()->{M,F}=pr_return_helper:target(),M:F(). target()->{pr_returned,live}. live()->42. dead()->unused."
    ),
    Helper = fixture(pr_return_helper, "-export([target/0]). target()->{pr_returned,live}."),
    {[Out, _], R} = packbeam_prune:run([B, Helper], [], [{pr_returned, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertNot(lists:member({dead, 0}, exports(Out))).

boot_branch_execution_test() ->
    Entry = fixture(
        pr_boot_entry,
        "-export([start/0,boot/1]). start()->boot([<<\"-s\">>,pr_boot_target]). boot([<<\"-s\">>,escript|_])->M=binary_to_term(get(module)),M:main([]); boot([<<\"-s\">>,M]) when is_atom(M)->case atomvm:get_boot() of undefined -> M:start(); {ok,B}->apply(binary_to_term(B),start,[]) end."
    ),
    Target = fixture(pr_boot_target, "-export([start/0,dead/0]). start()->42. dead()->unused."),
    {[E, T], R} = packbeam_prune:run([Entry, Target], [], [{pr_boot_entry, start, 0}], #{
        boot_data => undefined
    }),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{start, 0}], exports(T)),
    %% Supply the native boot query's result on the host OTP VM.
    Native = fixture(atomvm, "-export([get_boot/0]). get_boot()->undefined."),
    lists:foreach(fun({M, B}) -> {module, M} = code:load_binary(M, "fixture.beam", B) end, [
        {pr_boot_entry, E}, {pr_boot_target, T}, {atomvm, Native}
    ]),
    try
        ?assertEqual(42, pr_boot_entry:start())
    after
        lists:foreach(
            fun(M) ->
                code:delete(M),
                code:purge(M)
            end,
            [pr_boot_entry, pr_boot_target, atomvm]
        )
    end.

deep_dispatch_test() ->
    App = fixture(
        pr_deep,
        "-export([start/0,live/5,dead/0]). start()->pr_deep_helper:wrap(pr_deep,live,[self(),foo,bar,[],[{name,foo}]]). live(_,_,_,_,_)->42. dead()->unused."
    ),
    Helper = fixture(
        pr_deep_helper,
        "-export([wrap/3,dispatch/5]). wrap(M,F,A)->spawn(pr_deep_helper,dispatch,[self(),[],M,F,A]). dispatch(_,_,M,F,A)->apply(M,F,A)."
    ),
    {[Out, _], R} = packbeam_prune:run([App, Helper], [], [{pr_deep, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{live, 5}, {start, 0}], exports(Out)).

recursive_continuation_test() ->
    B = fixture(
        pr_recursive,
        "-export([start/0,loop/0,live/0]). start()->loop(),live(). loop()->loop(). live()->42."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_recursive, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    {module, pr_recursive} = code:load_binary(pr_recursive, "fixture.beam", Out),
    code:delete(pr_recursive),
    code:purge(pr_recursive).

native_chunk_test() ->
    B = fixture(pr_native, "-export([start/0]). start()->ok."),
    {ok, _, Cs} = beam_lib:all_chunks(B),
    {ok, Native} = beam_lib:build_module(Cs ++ [{"avmN", <<1, 2, 3>>}]),
    ?assertError(
        {cannot_prune_native_code, pr_native},
        packbeam_prune:run([Native], [], [{pr_native, start, 0}], #{})
    ),
    {ok, Legacy} = beam_lib:build_module(Cs ++ [{"HA64", <<1>>}, {"HX86", <<2>>}]),
    ?assertMatch({[_], _}, packbeam_prune:run([Legacy], [], [{pr_native, start, 0}], #{})).

aot_after_pruning_test() ->
    Dir = "_build/prune_aot_test/",
    ok = filelib:ensure_dir(Dir ++ "x"),
    Init = fixture(init, "-export([boot/1]). boot([_,M])->M:start()."),
    App = fixture(
        pr_aot, "-export([start/0,dead/0]). start()->{ok,[1,2]}. dead()->{unused,[3,4]}."
    ),
    ok = file:write_file(Dir ++ "init.beam", Init),
    ok = file:write_file(Dir ++ "pr_aot.beam", App),
    Self = self(),
    Compile = fun(M, B) ->
        ?assertEqual(pr_aot, M),
        ?assertEqual([{start, 0}], exports(B)),
        {ok, M, Cs} = beam_lib:all_chunks(B),
        ?assertNot(lists:keymember("avmN", 1, Cs)),
        {ok, Native} = beam_lib:build_module(Cs ++ [{"avmN", <<1, 2, 3, 4>>}]),
        Self ! compiled,
        Native
    end,
    ok = packbeam_api:create(Dir ++ "app.avm", [Dir ++ "pr_aot.beam"], #{
        prune => true, references => [Dir ++ "init.beam"], precompile => Compile
    }),
    receive
        compiled -> ok
    after 1000 -> error(no_precompile_pass)
    end,
    [P] = packbeam_api:list(Dir ++ "app.avm"),
    {ok, _, Cs} = beam_lib:all_chunks(packbeam_api:get_element_data(P)),
    ?assertEqual(<<1, 2, 3, 4>>, proplists:get_value("avmN", Cs)).

formatter_list_test() ->
    B = fixture(
        pr_formatters,
        "-export([start/1,dead/0]). start(S)->run(build(S,[])). build([],A)->lists:reverse(A); build([H|T],A)->build(T,[fun(X)->{H,X} end|A]). run([])->[]; run([F|T])->[F(42)|run(T)]. dead()->unused."
    ),
    Stub = fixture(
        lists,
        "-export([reverse/1,reverse/2]). reverse(_)->erlang:nif_error(undefined). reverse(_,_)->erlang:nif_error(undefined)."
    ),
    {[Out], R} = packbeam_prune:run([B], [Stub], [{pr_formatters, start, 1}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertNot(lists:member({dead, 0}, exports(Out))),
    {module, pr_formatters} = code:load_binary(pr_formatters, "fixture.beam", Out),
    try
        ?assertEqual([{X, 42} || X <- lists:seq(1, 40)], pr_formatters:start(lists:seq(1, 40)))
    after
        code:delete(pr_formatters),
        code:purge(pr_formatters)
    end.

formatter_tuple_test() ->
    B = fixture(
        pr_format_tuple,
        "-export([start/1,dead/0]). start(S)->{Tokens,Fs}=split(S,[],[]),run(Fs,Tokens). split([],T,F)->{lists:reverse(T,[sentinel]),lists:reverse(F,[fun(X)->{sentinel,X} end])}; split([H|R],T,F)->split(R,[H|T],[fun(X)->{H,X} end|F]). run([],[])->[]; run([F|Fs],[T|Ts])->[F(T)|run(Fs,Ts)]. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_format_tuple, start, 1}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{start, 1}], exports(Out)),
    {module, pr_format_tuple} = code:load_binary(pr_format_tuple, "fixture.beam", Out),
    try
        ?assertEqual(
            [{X, X} || X <- lists:seq(1, 40)] ++ [{sentinel, sentinel}],
            pr_format_tuple:start(lists:seq(1, 40))
        )
    after
        code:delete(pr_format_tuple),
        code:purge(pr_format_tuple)
    end.

unknown_list_fun_test() ->
    B = fixture(
        pr_unknown_list,
        "-export([start/1,dead/0]). start(L)->[F|_]=lists:reverse(L),F(). dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_unknown_list, start, 1}], #{}),
    %% The fun is unbounded, but no closure exists in this program, so the
    %% call reaches nothing and the module is still trimmed.
    ?assert(
        lists:any(
            fun(#{reason := Reason}) -> Reason =:= dynamic_fun_known_arity end,
            maps:get(warnings, R)
        )
    ),
    ?assertNot(lists:any(fun(#{scope := Scope}) -> Scope =:= package end, maps:get(warnings, R))),
    ?assertEqual([{start, 1}], exports(Out)).

unknown_arity_test() ->
    B = fixture(
        pr_arity,
        "-export([start/1,live/0,live/1,dead/0]). start(A)->apply(pr_arity,live,A). live()->ok. live(X)->X. dead()->M=erlang:get(module),M:run()."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_arity, start, 1}], #{}),
    ?assertEqual([{live, 0}, {live, 1}, {start, 1}], exports(Out)),
    [W] = maps:get(warnings, R),
    ?assertEqual({function, pr_arity, live}, maps:get(scope, W)),
    ?assertEqual(dynamic_arity, maps:get(reason, W)),
    Text = lists:flatten(packbeam_prune:format_warning(W)),
    ?assertNotEqual(nomatch, string:find(Text, "argument-list length")),
    ?assertEqual(nomatch, string:find(Text, "list_to_atom")).

warning_path_test() ->
    App = fixture(pr_path, "-export([start/0]). start()->pr_path_ref:run()."),
    Ref = fixture(
        pr_path_ref,
        "-export([run/0]). run()->invoke(erlang:get(callback)). invoke(F)->F(),missing_after_fallback:run()."
    ),
    {[_Out], R} = packbeam_prune:run([App], [Ref], [{pr_path, start, 0}], #{}),
    [W] = [X || #{reason := dynamic_fun_known_arity} = X <- maps:get(warnings, R)],
    ?assertEqual(reference, maps:get(origin, W)),
    ?assertEqual(
        [{pr_path, start, 0}, {pr_path_ref, run, 0}, {pr_path_ref, invoke, 1}], maps:get(path, W)
    ),
    Text = lists:flatten(packbeam_prune:format_warning(W)),
    ?assertNotEqual(
        nomatch,
        string:find(Text, "pr_path:start/0\n    -> pr_path_ref:run/0\n    -> pr_path_ref:invoke/1")
    ),
    ?assertNotEqual(nomatch, string:find(Text, "closure")),
    ?assertEqual(nomatch, string:find(Text, "list_to_atom")).

callback_argument_shapes_test() ->
    B = fixture(
        pr_shapes,
        "-export([start/0,dispatch/3,init_it/4,init_it/5,init/1,dead/0]). start()->dispatch(pr_shapes,init_it,[self(),pr_shapes,[],[]]),dispatch(pr_shapes,init_it,[self(),name,pr_shapes,[],[]]). dispatch(M,F,A)->apply(M,F,A). init_it(P,_Name,M,A,O)->init_it(P,M,A,O). init_it(_P,M,A,_O)->M:init(A). init([])->ok. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_shapes, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assert(lists:member({pr_shapes, init, 1}, maps:get(reachable, R))),
    ?assertNot(lists:member({dead, 0}, exports(Out))),
    {module, pr_shapes} = code:load_binary(pr_shapes, "fixture.beam", Out),
    try
        ?assertEqual(ok, pr_shapes:start())
    after
        code:delete(pr_shapes),
        code:purge(pr_shapes)
    end.

record_callback_test() ->
    B = fixture(
        pr_record,
        "-record(state,{mod,children=[],pid}). -export([start/0,update/1,run/1,init/0,dead/0]). start()->run(update(#state{mod=pr_record})). update(S)->S#state{children=[child],pid=self()}. run(#state{mod=M})->M:init(). init()->ok. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_record, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertNot(lists:member({dead, 0}, exports(Out))),
    {module, pr_record} = code:load_binary(pr_record, "fixture.beam", Out),
    try
        ?assertEqual(ok, pr_record:start())
    after
        code:delete(pr_record),
        code:purge(pr_record)
    end.

receive_callback_test() ->
    B = fixture(
        pr_receive,
        "-export([start/0,loop/1,init/1,dead/0]). start()->self()!ping,loop(pr_receive). loop(M)->receive X->M:init(X) after 0->M:init(timeout) end. init(X)->X. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_receive, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertNot(lists:member({dead, 0}, exports(Out))),
    {module, pr_receive} = code:load_binary(pr_receive, "fixture.beam", Out),
    try
        ?assertEqual(ping, pr_receive:start())
    after
        code:delete(pr_receive),
        code:purge(pr_receive)
    end.

callback_context_test() ->
    App = fixture(
        pr_context,
        "-export([start/0,dispatch/2,choose/1]). start()->{M,A}=choose(left),dispatch(M,A),{N,B}=choose(right),dispatch(N,B). choose(left)->{pr_context_left,{pr_context_left,[]}}; choose(right)->{pr_context_right,[]}. dispatch(M,A)->M:init(A)."
    ),
    Left = fixture(
        pr_context_left,
        "-export([init/1,finish/1,dead/0]). init({M,A})->M:finish(A). finish([])->ok. dead()->unused."
    ),
    Right = fixture(pr_context_right, "-export([init/1,dead/0]). init([])->ok. dead()->unused."),
    {Out, R} = packbeam_prune:run([App, Left, Right], [], [{pr_context, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual(3, length(Out)),
    lists:foreach(
        fun(B) ->
            ?assertNot(lists:member({dead, 0}, exports(B))),
            {ok, M, _} = beam_lib:all_chunks(B),
            {module, M} = code:load_binary(M, "fixture.beam", B)
        end,
        Out
    ),
    try
        ?assertEqual(ok, pr_context:start())
    after
        lists:foreach(
            fun(M) ->
                code:delete(M),
                code:purge(M)
            end,
            [pr_context, pr_context_left, pr_context_right]
        )
    end.

supervisor_literal_child_test() ->
    App = fixture(
        pr_child_app,
        "-export([start/0]). start()->pr_child_sup:boot(#{id=>worker,start=>{pr_child_worker,start_link,[hello]}})."
    ),
    Sup = fixture(
        pr_child_sup,
        "-export([boot/1,terminate/2,start_child/1]). -record(child,{id,start,pid}). boot(#{id:=Id,start:=MFA})->C=#child{id=Id,start=MFA},case erlang:function_exported(pr_child_sup,terminate,2) of true->ok;false->ok end,try_start(C). try_start(#child{start={M,F,A}})->apply(M,F,A). terminate(_,_)->ok. start_child(Spec)->boot(Spec)."
    ),
    Worker = fixture(
        pr_child_worker, "-export([start_link/1,dead/0]). start_link(X)->{ok,X}. dead()->dead."
    ),
    {[_, S, W], R} = packbeam_prune:run([App, Sup, Worker], [], [{pr_child_app, start, 0}], #{}),
    ?assertEqual([{start_link, 1}], exports(W)),
    ?assertNot(lists:member({start_child, 1}, exports(S))),
    ?assertNot(lists:any(fun(#{scope := Scope}) -> Scope =:= package end, maps:get(warnings, R))).

literal_child_alternatives_test() ->
    App = fixture(
        pr_child_choices,
        "-export([start/1,dead/0]). start(X)->Spec=case X of a->#{id=>a,start=>{pr_child_a,run,[first]}};_->#{id=>b,start=>{pr_child_b,run,[second,extra]}} end,pr_child_dispatch:start(Spec). dead()->#{id=>dead,start=>{pr_child_unused,run,[]}}."
    ),
    Dispatch = fixture(
        pr_child_dispatch,
        "-export([start/1]). -record(child,{id,start}). start(#{id:=Id,start:=MFA})->invoke(#child{id=Id,start=MFA}). invoke(#child{start={M,F,A}})->apply(M,F,A)."
    ),
    A = fixture(pr_child_a, "-export([run/1,dead/0]). run(X)->X. dead()->dead."),
    B = fixture(pr_child_b, "-export([run/2,dead/0]). run(X,Y)->{X,Y}. dead()->dead."),
    U = fixture(pr_child_unused, "-export([run/0]). run()->unused."),
    {Out, R} = packbeam_prune:run(
        [App, Dispatch, A, B, U], [], [{pr_child_choices, start, 1}], #{}
    ),
    ?assertEqual(4, length(Out)),
    ?assertEqual([], maps:get(warnings, R)),
    Graph = maps:get(graph, R),
    Literals = maps:get(literals, Graph),
    ?assert(
        lists:any(
            fun(#{value := V}) -> V =:= #{id => a, start => {pr_child_a, run, [first]}} end,
            maps:values(Literals)
        )
    ),
    ?assertNot(
        lists:any(
            fun(#{value := V}) -> V =:= #{id => dead, start => {pr_child_unused, run, []}} end,
            maps:values(Literals)
        )
    ),
    lists:foreach(
        fun({M, Bin}) -> {module, M} = code:load_binary(M, "fixture.beam", Bin) end,
        lists:zip([pr_child_choices, pr_child_dispatch, pr_child_a, pr_child_b], Out)
    ),
    try
        ?assertEqual(first, pr_child_choices:start(a)),
        ?assertEqual({second, extra}, pr_child_choices:start(b))
    after
        lists:foreach(
            fun(M) ->
                code:delete(M),
                code:purge(M)
            end,
            [pr_child_choices, pr_child_dispatch, pr_child_a, pr_child_b]
        )
    end.

literal_children_record_loop_test() ->
    B = fixture(
        pr_child_loop,
        "-export([start/0,one/0,two/1,dead/0]). -record(child,{id,start}). -record(state,{children=[]}). start()->S=build([#{id=>a,start=>{pr_child_loop,one,[]}},#{id=>b,start=>{pr_child_loop,two,[hello]}}],#state{}),run(S#state.children). build([],S)->S;build([#{id:=Id,start:=MFA}|T],S)->C=#child{id=Id,start=MFA},build(T,S#state{children=[C|S#state.children]}). run([])->[];run([#child{start={M,F,A}}|T])->[apply(M,F,A)|run(T)]. one()->first. two(X)->X. dead()->dead."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_child_loop, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{one, 0}, {start, 0}, {two, 1}], exports(Out)),
    {module, pr_child_loop} = code:load_binary(pr_child_loop, "fixture.beam", Out),
    try
        ?assertEqual([hello, first], pr_child_loop:start())
    after
        code:delete(pr_child_loop),
        code:purge(pr_child_loop)
    end.

mailbox_literal_child_test() ->
    App = fixture(
        pr_mail_child,
        "-export([start/0,live/0,dead/0]). start()->self()!{'$gen_call',{self(),make_ref()},#{start=>{pr_mail_child,live,[]}}},receive Msg->dispatch(Msg) end. dispatch({'$gen_call',_,#{start:={M,F,A}}})->apply(M,F,A);dispatch(_)->other. live()->42. dead()->dead."
    ),
    {[Out], R} = packbeam_prune:run([App], [], [{pr_mail_child, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{live, 0}, {start, 0}], exports(Out)),
    {module, pr_mail_child} = code:load_binary(pr_mail_child, "fixture.beam", Out),
    try
        ?assertEqual(42, pr_mail_child:start())
    after
        code:delete(pr_mail_child),
        code:purge(pr_mail_child)
    end.

mailbox_unknown_child_test() ->
    App = fixture(
        pr_mail_unknown,
        "-export([start/0,dead/0]). start()->self()!binary_to_term(erlang:get(input)),receive {'$gen_call',_,#{start:={M,F,A}}}->apply(M,F,A) end. dead()->dead."
    ),
    {[Out], R} = packbeam_prune:run([App], [], [{pr_mail_unknown, start, 0}], #{}),
    %% The child specification is decoded at run time, so no module is named
    %% for it and nothing is retained on its behalf.
    ?assert(
        lists:any(fun(#{scope := Scope}) -> Scope =:= named_modules end, maps:get(warnings, R))
    ),
    ?assertEqual([{start, 0}], exports(Out)).

mailbox_timer_child_test() ->
    B = fixture(
        pr_mail_timer,
        "-export([start/0,live/0,dead/0]). start()->erlang:send_after(0,self(),{'$gen_cast',{pr_mail_timer,live,[]}}),receive {'$gen_cast',{M,F,A}}->apply(M,F,A) end. live()->42. dead()->dead."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_mail_timer, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{live, 0}, {start, 0}], exports(Out)).

mailbox_separate_receives_test() ->
    B = fixture(
        pr_mail_separate,
        "-export([start/0,one/0,two/0,dead/0]). start()->self()!{'$gen_call',none,{pr_mail_separate,one,[]}},A=receive X->X end,self()!{'$gen_cast',{pr_mail_separate,two,[]}},receive {'$gen_cast',_}->case A of {'$gen_call',_,{M,F,Args}}->apply(M,F,Args) end end. one()->1. two()->2. dead()->dead."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_mail_separate, start, 0}], #{}),
    %% Forgetting the earlier message is conservative; it must never acquire
    %% the later message's fields and trim its actual target.
    ?assert(lists:member({pr_mail_separate, one, 0}, maps:get(reachable, R))),
    {module, pr_mail_separate} = code:load_binary(pr_mail_separate, "fixture.beam", Out),
    try
        ?assertEqual(1, pr_mail_separate:start())
    after
        code:delete(pr_mail_separate),
        code:purge(pr_mail_separate)
    end.

mailbox_echo_child_test() ->
    B = fixture(
        pr_mail_echo,
        "-export([start/0,live/0,dead/0]). start()->P=open_port({spawn,\"echo\"},[]),P!{self(),{'$gen_call',none,{pr_mail_echo,live,[]}}},receive {'$gen_call',_,{M,F,A}}->apply(M,F,A) end. live()->42. dead()->dead."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_mail_echo, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{live, 0}, {start, 0}], exports(Out)).

mailbox_external_port_test() ->
    B = fixture(
        pr_mail_port,
        "-export([start/0,dead/0]). start()->open_port({spawn,\"custom\"},[]),receive {'$gen_call',_,{M,F,A}}->apply(M,F,A) end. dead()->dead."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_mail_port, start, 0}], #{}),
    %% An unknown port can send anything, so the control message is unbounded.
    %% The dispatch is then bounded by the modules the code names: none here.
    ?assert(
        lists:any(fun(#{scope := Scope}) -> Scope =:= named_modules end, maps:get(warnings, R))
    ),
    ?assertEqual([{start, 0}], exports(Out)).

known_nonfun_dispatch_test() ->
    B = fixture(
        pr_nonfun,
        "-export([start/1,invoke/1,dead/0]). start(X)->F=case X of a->ordered;_->undefined end,try invoke(F) catch error:{badfun,_}->caught end. invoke(F)->F(). dead()->dead."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_nonfun, start, 1}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{invoke, 1}, {start, 1}], exports(Out)),
    {module, pr_nonfun} = code:load_binary(pr_nonfun, "fixture.beam", Out),
    try
        ?assertEqual(caught, pr_nonfun:start(a))
    after
        code:delete(pr_nonfun),
        code:purge(pr_nonfun)
    end.

unknown_module_export_query_test() ->
    App = fixture(
        pr_export_query,
        "-export([start/1,dead/0]). start(M)->erlang:put(known,pr_export_handler),erlang:function_exported(M,terminate,2). dead()->dead."
    ),
    Handler = fixture(
        pr_export_handler, "-export([terminate/2,dead/0]). terminate(_,_)->ok. dead()->dead."
    ),
    {[A, H], R} = packbeam_prune:run([App, Handler], [], [{pr_export_query, start, 1}], #{}),
    ?assertEqual([{start, 1}], exports(A)),
    ?assertEqual([{terminate, 2}], exports(H)),
    ?assertNot(lists:any(fun(#{scope := Scope}) -> Scope =:= package end, maps:get(warnings, R))).

record_context_widening_test() ->
    Calls = lists:join(",", [
        io_lib:format("dispatch({state,pr_wide_record,~p})", [N])
     || N <- lists:seq(1, 20)
    ]),
    Body = lists:flatten([
        "-export([start/0,dispatch/1,live/1,dead/0]). start()->",
        Calls,
        ". dispatch({state,M,N})->M:live(N). live(N)->N. dead()->dead."
    ]),
    B = fixture(pr_wide_record, Body),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_wide_record, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{dispatch, 1}, {live, 1}, {start, 0}], exports(Out)),
    {module, pr_wide_record} = code:load_binary(pr_wide_record, "fixture.beam", Out),
    try
        ?assertEqual(20, pr_wide_record:start())
    after
        code:delete(pr_wide_record),
        code:purge(pr_wide_record)
    end.

startup_outcome_values_test() ->
    B = fixture(
        pr_outcome,
        "-export([start/1,dispatch/1,live/0,dead/0]). start(X)->case pr_outcome_source:outcome(X) of {ok,S}->dispatch(S);{fail,Error,_}->Error end. dispatch(S)->M=element(2,S),M:live(). live()->42. dead()->dead."
    ),
    Source = fixture(
        pr_outcome_source,
        "-export([outcome/1]). outcome(ok)->{ok,{state,pr_outcome}};outcome(_)->{fail,{error,bad_start},exit}."
    ),
    {[Out, H], R} = packbeam_prune:run([B, Source], [], [{pr_outcome, start, 1}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertNot(lists:member({dead, 0}, exports(Out))),
    Sites = maps:get(dispatches, maps:get(graph, R)),
    [Site] = [V || {{{pr_outcome, dispatch, 1}, _}, V} <- maps:to_list(Sites)],
    ?assertEqual({const, pr_outcome}, maps:get(modules, Site)),
    ?assertEqual([{pr_outcome, live, 0}], maps:get(targets, Site)),
    {module, pr_outcome} = code:load_binary(pr_outcome, "fixture.beam", Out),
    {module, pr_outcome_source} = code:load_binary(pr_outcome_source, "fixture.beam", H),
    try
        ?assertEqual(42, pr_outcome:start(ok)),
        ?assertEqual({error, bad_start}, pr_outcome:start(fail))
    after
        lists:foreach(
            fun(M) ->
                code:delete(M),
                code:purge(M)
            end,
            [pr_outcome, pr_outcome_source]
        )
    end.

tagged_callback_values_test() ->
    App = fixture(
        pr_tagged,
        "-export([start/1]). start(X)->R=pr_tag_source:outcome(X),case element(1,R) of ok->{M,F,A}=element(2,R),apply(M,F,A);error->element(2,R) end."
    ),
    Source = fixture(
        pr_tag_source,
        "-export([outcome/1]). outcome(ok)->{ok,{pr_tag_target,live,[self()]}};outcome(_)->{error,{pr_tag_unused,dead,[self()]}}."
    ),
    Target = fixture(pr_tag_target, "-export([live/1,dead/0]). live(_)->42. dead()->dead."),
    Unused = fixture(pr_tag_unused, "-export([dead/1]). dead(_)->dead."),
    {Out, R} = packbeam_prune:run([App, Source, Target, Unused], [], [{pr_tagged, start, 1}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual(3, length(Out)),
    ?assertEqual([{live, 1}], exports(lists:nth(3, Out))),
    Ms = [pr_tagged, pr_tag_source, pr_tag_target],
    lists:foreach(
        fun({M, Bin}) -> {module, M} = code:load_binary(M, "fixture.beam", Bin) end,
        lists:zip(Ms, Out)
    ),
    try
        ?assertEqual(42, pr_tagged:start(ok)),
        ?assertMatch({pr_tag_unused, dead, [_]}, pr_tagged:start(error))
    after
        lists:foreach(
            fun(M) ->
                code:delete(M),
                code:purge(M)
            end,
            Ms
        )
    end.

structural_map_callback_test() ->
    B = fixture(
        pr_shape,
        "-export([start/0,dispatch/1,live/1,dead/0]). start()->dispatch(#{id=>self(),start=>{pr_shape,live,[hello]}}). dispatch({_,{M,F,A}})->apply(M,F,A);dispatch(#{start:={M,F,A}})->apply(M,F,A). live(X)->X. dead()->dead."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_shape, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertNot(lists:member({dead, 0}, exports(Out))).

native_keyfind_callback_values_test() ->
    B = fixture(
        pr_keyfind,
        "-export([start/1,live/1,other/1,dead/0]). start(Key)->case lists:keyfind(Key,2,[{child,a,{pr_keyfind,live,[hello]}},{child,b,{pr_keyfind,other,[world]}}]) of false->not_found;{child,_,{M,F,A}}->apply(M,F,A) end. live(X)->X. other(X)->X. dead()->dead."
    ),
    Stub = fixture(lists, "-export([keyfind/3]). keyfind(_,_,_)->erlang:nif_error(undefined)."),
    {[Out], R} = packbeam_prune:run([B], [Stub], [{pr_keyfind, start, 1}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{live, 1}, {other, 1}, {start, 1}], exports(Out)),
    {module, pr_keyfind} = code:load_binary(pr_keyfind, "fixture.beam", Out),
    try
        ?assertEqual(hello, pr_keyfind:start(a)),
        ?assertEqual(world, pr_keyfind:start(b)),
        ?assertEqual(not_found, pr_keyfind:start(c))
    after
        code:delete(pr_keyfind),
        code:purge(pr_keyfind)
    end.

mailbox_reply_shape_values_test() ->
    B = fixture(
        pr_reply_shape,
        "-export([start/0,live/0,dead/0]). start()->self()!{get(tag),get(reply)},self()!{'$gen_call',none,{pr_reply_shape,live,[]}},receive {'$gen_call',_,{M,F,A}}->apply(M,F,A) end. live()->42. dead()->dead."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_reply_shape, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{live, 0}, {start, 0}], exports(Out)).

independent_projection_values_test() ->
    App = fixture(
        pr_independent,
        "-export([start/2]). start(X,Y)->A=pr_independent_source:outcome(X),B=pr_independent_source:outcome(Y),case element(1,A) of ok->M=element(2,B),M:run();error->blocked end."
    ),
    Source = fixture(
        pr_independent_source,
        "-export([outcome/1]). outcome(ok)->{ok,pr_independent_one};outcome(_)->{error,pr_independent_two}."
    ),
    One = fixture(pr_independent_one, "-export([run/0]). run()->one."),
    Two = fixture(pr_independent_two, "-export([run/0]). run()->two."),
    {Out, R} = packbeam_prune:run([App, Source, One, Two], [], [{pr_independent, start, 2}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual(4, length(Out)),
    Ms = [pr_independent, pr_independent_source, pr_independent_one, pr_independent_two],
    lists:foreach(
        fun({M, B}) -> {module, M} = code:load_binary(M, "fixture.beam", B) end, lists:zip(Ms, Out)
    ),
    try
        ?assertEqual(two, pr_independent:start(ok, error)),
        ?assertEqual(one, pr_independent:start(ok, ok)),
        ?assertEqual(blocked, pr_independent:start(error, ok))
    after
        lists:foreach(
            fun(M) ->
                code:delete(M),
                code:purge(M)
            end,
            Ms
        )
    end.

unknown_module_signature_test() ->
    App = fixture(
        pr_log_app,
        "-export([start/1,callback/0,dead/0]). start(M)->erlang:put(known,[pr_log_one,pr_log_two,pr_log_ref]),M:log(event,#{}). callback()->callback. dead()->unused."
    ),
    H1 = fixture(
        pr_log_one,
        "-export([log/1,log/2,log/3,dead/0]). log(_)->unused. log(E,C)->helper(E,C). log(_,_,_)->unused. helper(E,C)->{one,E,C}. dead()->unused."
    ),
    H2 = fixture(pr_log_two, "-export([log/2,dead/0]). log(E,C)->{two,E,C}. dead()->unused."),
    Ref = fixture(
        pr_log_ref, "-export([log/2,dead/0]). log(_,_)->pr_log_app:callback(). dead()->unused."
    ),
    Private = fixture(pr_log_private, "-export([other/0]). other()->log(a,b). log(A,B)->{A,B}."),
    {Out, R} = packbeam_prune:run([App, H1, H2, Private], [Ref], [{pr_log_app, start, 1}], #{}),
    ?assertEqual(3, length(Out)),
    ?assertEqual([[{callback, 0}, {start, 1}], [{log, 2}], [{log, 2}]], [exports(B) || B <- Out]),
    %% The function and arity bound the targets, so no warning is needed.
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual(
        [{pr_log_one, log, 2}, {pr_log_ref, log, 2}, {pr_log_two, log, 2}],
        dispatch_targets(R, {pr_log_app, start, 1})
    ),
    ?assert(lists:member({pr_log_one, helper, 2}, maps:get(reachable, R))),
    lists:foreach(
        fun(B) ->
            {ok, M, _} = beam_lib:all_chunks(B),
            {module, M} = code:load_binary(M, "fixture.beam", B)
        end,
        Out ++ [Ref]
    ),
    try
        ?assertEqual({one, event, #{}}, pr_log_app:start(pr_log_one)),
        ?assertEqual({two, event, #{}}, pr_log_app:start(pr_log_two)),
        ?assertEqual(callback, pr_log_app:start(pr_log_ref))
    after
        lists:foreach(
            fun(M) ->
                code:delete(M),
                code:purge(M)
            end,
            [pr_log_app, pr_log_one, pr_log_two, pr_log_ref]
        )
    end.

%% A callback module can only be selected by a name that appears in the
%% analyzed code. A dispatch bounded that way needs no warning.
%% A module with neither a BEAM in the closed world nor a registered native
%% implementation cannot be called: the call raises undef, so the code behind
%% it is not reachable. The absence is reported.
%% A configuration closure stays bound when it travels inside nested proper
%% lists: list length must not consume the structural depth budget the way
%% nesting does.
nested_list_callback_test() ->
    App = fixture(
        pr_deep_app,
        "-export([start/0,cb/0,dead/0]). start()->C=[{sta,[{a,1},{b,2},{c,fun() -> pr_deep_app:cb() end}]}],pr_deep_lib:run([w,x,y,z,[q,r,C]]). cb()->ok. dead()->unused."
    ),
    Lib = fixture(
        pr_deep_lib,
        "-export([run/1]). run([_,_,_,_,[_,_,C]])->pick(C). pick([{sta,S}])->pick2(S). pick2([{c,F}|_])->F(); pick2([_|T])->pick2(T)."
    ),
    {Out, R} = packbeam_prune:run([App, Lib], [], [{pr_deep_app, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assert(lists:member({pr_deep_app, cb, 0}, maps:get(reachable, R))),
    ?assertEqual([{cb, 0}, {start, 0}], exports(hd(Out))).

%% A native function's Erlang body is only a stub: its head need not accept
%% the arguments the native implementation handles, so the call still returns.
nif_stub_return_test() ->
    App = fixture(
        pr_stub_app,
        "-export([start/0,live/0,dead/0]). start()->pr_stub_lib:member(x,[a]),live(). live()->42. dead()->unused."
    ),
    Lib = fixture(
        pr_stub_lib, "-export([member/2]). member(_,[])->erlang:nif_error(undefined)."
    ),
    {[Out, _], R} = packbeam_prune:run([App, Lib], [], [{pr_stub_app, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{live, 0}, {start, 0}], exports(Out)),
    ?assert(lists:member({pr_stub_app, live, 0}, maps:get(reachable, R))).

%% A loop that never returns leaves its caller's continuation unreachable.
%% Widening the summary of such a loop to an arbitrary value instead would
%% poison every state it carries.
non_returning_loop_test() ->
    B = fixture(
        pr_loop,
        "-export([start/0,loop/1,dead/0]). start()->loop(pr_loop),dead(). loop(M)->receive stop -> loop(M) end. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_loop, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertNot(lists:member({pr_loop, dead, 0}, maps:get(reachable, R))),
    ?assertEqual([{loop, 1}, {start, 0}], exports(Out)).

%% A fun value can only be a closure that analyzed code created. An unbounded
%% fun call is bounded by those closures, taking the call arity into account,
%% instead of retaining every output module.
unknown_fun_closure_bound_test() ->
    B = fixture(
        pr_fun_bound,
        "-export([start/0,live/0,other/0,dead/0]). start()->erlang:put(f,fun() -> live() end),erlang:put(g,fun(X) -> other(),X end),F=erlang:get(f),F(),ok. live()->1. other()->2. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_fun_bound, start, 0}], #{}),
    ?assertNot(lists:any(fun(#{scope := Scope}) -> Scope =:= package end, maps:get(warnings, R))),
    ?assertNot(lists:member({pr_fun_bound, dead, 0}, maps:get(reachable, R))),
    ?assertEqual([{live, 0}, {other, 0}, {start, 0}], exports(Out)),
    [W] = maps:get(warnings, R),
    ?assertEqual({closures, 0}, maps:get(scope, W)),
    Text = lists:flatten(packbeam_prune:format_warning(W)),
    ?assertEqual(nomatch, string:find(Text, "ALL output")).

%% `trim' shifts the stack frame down; the slots that remain keep their
%% values. Dropping every stack fact there loses the callback module that a
%% gen_server carries across its own calls.
trim_keeps_stack_facts_test() ->
    App = fixture(
        pr_trim,
        "-export([start/0,mk/0,sink/1,dead/0]). start()->M=mk(),A=erlang:get(a),B=erlang:get(b),sink(A),sink(B),M:live(). mk()->case erlang:get(x) of undefined -> pr_trim_a; _ -> pr_trim_b end. sink(X)->X. dead()->unused."
    ),
    A = fixture(pr_trim_a, "-export([live/0,dead/0]). live()->1. dead()->unused."),
    B = fixture(pr_trim_b, "-export([live/0,dead/0]). live()->2. dead()->unused."),
    {Out, R} = packbeam_prune:run([App, A, B], [], [{pr_trim, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual(
        [[{mk, 0}, {sink, 1}, {start, 0}], [{live, 0}], [{live, 0}]], [exports(X) || X <- Out]
    ),
    %% The module of the call survived the trim, so the site is not a
    %% dispatch on an unknown module.
    ?assertEqual(
        [{choices, [{const, pr_trim_a}, {const, pr_trim_b}]}],
        [
            maps:get(modules, Site)
         || {{{pr_trim, start, 0}, _}, Site} <- maps:to_list(
                maps:get(dispatches, maps:get(graph, R))
            )
        ]
    ).

%% The candidates of a known-signature dispatch bound its result too: the
%% call returns what they return, not an arbitrary value.
signature_return_bound_test() ->
    App = fixture(
        pr_sigret,
        "-export([start/0,live/0,dead/0]). start()->erlang:put(m,pr_sigret_cb),M=erlang:get(m),case M:run() of {ok,_} -> live(); other -> dead() end. live()->1. dead()->2."
    ),
    Cb = fixture(pr_sigret_cb, "-export([run/0]). run()->{ok,42}."),
    {[Out, _], R} = packbeam_prune:run([App, Cb], [], [{pr_sigret, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertNot(lists:member({pr_sigret, dead, 0}, maps:get(reachable, R))),
    ?assertEqual([{live, 0}, {start, 0}], exports(Out)).

%% `++' builds a list out of its two arguments: the elements of both sides
%% survive, so a record carried in a list keeps its callback fields.
append_keeps_elements_test() ->
    B = fixture(
        pr_append,
        "-export([start/0,mk/1,run/1,live/0,other/0,dead/0]). start()->A=mk(a),C=mk(b),run(A++C). mk(K)->case erlang:get(K) of undefined -> [{spec,pr_append,live}]; _ -> [{spec,pr_append,other}] end. run(L)->{spec,M,F}=erlang:hd(L),M:F(). live()->ok. other()->ok. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_append, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assert(lists:member({pr_append, live, 0}, maps:get(reachable, R))),
    ?assertNot(lists:member({pr_append, dead, 0}, maps:get(reachable, R))),
    ?assertEqual([{live, 0}, {mk, 1}, {other, 0}, {run, 1}, {start, 0}], exports(Out)).

%% Neither the module nor the function is known. A module can still only be
%% selected by a name the analyzed code produces, so the exports of the named
%% modules bound the call and an unnamed module is still trimmed.
fully_unknown_dispatch_test() ->
    App = fixture(
        pr_unk_app,
        "-export([start/0,dead/0]). start()->erlang:put(k,{pr_unk_named,b}),M=erlang:get(m),F=erlang:get(f),M:F(). dead()->unused."
    ),
    Named = fixture(pr_unk_named, "-export([a/0,b/0]). a()->1. b()->helper(). helper()->2."),
    Other = fixture(pr_unk_other, "-export([b/0]). b()->3."),
    {Out, R} = packbeam_prune:run([App, Named, Other], [], [{pr_unk_app, start, 0}], #{}),
    ?assertNot(lists:any(fun(#{scope := Scope}) -> Scope =:= package end, maps:get(warnings, R))),
    %% pr_unk_other exports b/0 too, but nothing names that module.
    ?assertEqual([pr_unk_app, pr_unk_named], [module(X) || X <- Out]),
    ?assertNot(lists:member({pr_unk_app, dead, 0}, maps:get(reachable, R))),
    %% Only the exported name the code produces can be dispatched to, and
    %% `module_info/0,1\' is not one: a/0 and the generated exports go.
    ?assertEqual([{b, 0}], exports(lists:last(Out))),
    %% What the retained code calls locally comes along, so it still runs.
    {module, pr_unk_named} = code:load_binary(
        pr_unk_named, "fixture.beam", lists:last(Out)
    ),
    try
        ?assertEqual(2, pr_unk_named:b())
    after
        code:delete(pr_unk_named),
        code:purge(pr_unk_named)
    end.

%% Rewriting a module must not grow it: an AVM keeps literals uncompressed in
%% a `LitU\' chunk, and the rewritten module has to go back into the same form
%% rather than the four-bytes-longer uncompressed `LitT\'.
literal_chunk_round_trip_test() ->
    Dir = "_build/prune_literal_test/",
    ok = filelib:ensure_dir(Dir ++ "x"),
    ok = file:write_file(
        Dir ++ "pr_lit.beam",
        fixture(
            pr_lit,
            "-export([start/0]). start()->{ok,[<<\"a literal payload\">>,{tagged,tuple,fields},#{k=>v}]}."
        )
    ),
    ok = packbeam_api:create(Dir ++ "lit.avm", [Dir ++ "pr_lit.beam"], #{lib => true}),
    [Before] = [P || P <- packbeam_api:list(Dir ++ "lit.avm"), packbeam_api:is_beam(P)],
    ok = packbeam_api:create(Dir ++ "pruned.avm", [Dir ++ "lit.avm"], #{
        prune => functions,
        lib => true,
        keep => [{pr_lit, start, 0}],
        diagnostics => fun(_) -> ok end
    }),
    [After] = [P || P <- packbeam_api:list(Dir ++ "pruned.avm"), packbeam_api:is_beam(P)],
    {ok, _, Chunks} = beam_lib:all_chunks(packbeam_api:get_element_data(After)),
    ?assert(lists:keymember("LitU", 1, Chunks)),
    ?assertNot(lists:keymember("LitT", 1, Chunks)),
    ?assert(
        byte_size(packbeam_api:get_element_data(After)) =<
            byte_size(packbeam_api:get_element_data(Before))
    ).

%% `lists:keyreplace/4\' and its siblings return a list of the same elements,
%% with one replaced or removed. Following the loop inside them loses that,
%% and with it the callback fields a supervisor keeps in its child list.
key_list_functions_test() ->
    App = fixture(
        pr_keylist,
        "-export([start/0,live/0,other/0,dead/0]). start()->L=[{a,pr_keylist,live}],L2=lists:keyreplace(a,1,L,{a,pr_keylist,other}),{_,M,F}=erlang:hd(L2),M:F(). live()->1. other()->2. dead()->3."
    ),
    Lists = fixture(lists, "-export([keyreplace/4]). keyreplace(_,_,_,_)->erlang:get(opaque)."),
    {_, R} = packbeam_prune:run([App, Lists], [], [{pr_keylist, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assert(lists:member({pr_keylist, live, 0}, maps:get(reachable, R))),
    ?assert(lists:member({pr_keylist, other, 0}, maps:get(reachable, R))),
    ?assertNot(lists:member({pr_keylist, dead, 0}, maps:get(reachable, R))).

%% The atom, import and type tables are indexed like the literal table, and
%% keeping entries the retained code no longer names is dead weight.
table_trimming_test() ->
    B = fixture(
        pr_tables,
        "-export([start/0,dead/0]). start()->erlang:display(kept_atom). dead()->erlang:garbage_collect(),erlang:system_time(),erlang:make_ref(),{dropped_atom,another_dropped}."
    ),
    {[Out], _} = packbeam_prune:run([B], [], [{pr_tables, start, 0}], #{}),
    {ok, {_, [{atoms, Before}, {imports, ImpBefore}]}} = beam_lib:chunks(B, [atoms, imports]),
    {ok, {_, [{atoms, After}, {imports, ImpAfter}]}} = beam_lib:chunks(Out, [atoms, imports]),
    ?assert(length(After) < length(Before)),
    ?assert(length(ImpAfter) < length(ImpBefore)),
    ?assertNot(lists:member(dropped_atom, [A || {_, A} <- After])),
    ?assert(lists:member(kept_atom, [A || {_, A} <- After])),
    %% The module name stays at index 1, and the rewritten code still resolves.
    ?assertEqual(pr_tables, proplists:get_value(1, After)),
    {module, pr_tables} = code:load_binary(pr_tables, "fixture.beam", Out),
    try
        ?assertEqual(true, pr_tables:start())
    after
        code:delete(pr_tables),
        code:purge(pr_tables)
    end.

%% A generic server loop runs every server of the program. Once it has more
%% argument contexts than the budget, contexts may be merged, but only with
%% contexts of the same server: merging two servers' states hands one
%% server's callback data to the other's callbacks.
server_loop_widening_test() ->
    App = fixture(
        pr_gs_app,
        "-export([start/0,loop/2]). start()->loop(m1,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m1,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m2,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m2,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m3,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m3,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m4,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m4,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m5,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m5,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m6,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m6,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m7,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m7,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m8,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m8,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m9,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m9,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m10,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m10,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m11,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m11,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m12,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m12,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m13,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m13,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m14,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m14,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m15,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m15,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m16,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m16,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m17,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m17,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m18,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m18,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m19,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m19,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m20,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m20,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m21,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m21,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m22,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m22,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m23,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m23,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m24,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m24,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m25,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m25,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m26,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m26,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m27,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m27,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m28,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m28,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m29,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m29,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m30,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m30,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m31,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m31,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m32,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m32,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m33,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m33,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m34,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m34,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m35,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m35,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m36,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m36,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m37,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m37,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m38,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m38,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m39,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m39,{state,pr_gs_b,{spec,pr_gs_t,dead}}),loop(m40,{state,pr_gs_a,{spec,pr_gs_t,run}}),loop(m40,{state,pr_gs_b,{spec,pr_gs_t,dead}}). loop(Msg,{state,Mod,ModState})->Mod:handle(Msg,ModState)."
    ),
    A = fixture(pr_gs_a, "-export([handle/2]). handle(_,{spec,M,F})->M:F()."),
    B = fixture(pr_gs_b, "-export([handle/2]). handle(_,S)->S."),
    T = fixture(pr_gs_t, "-export([run/0,dead/0]). run()->ok. dead()->unused."),
    {_, R} = packbeam_prune:run([App, A, B, T], [], [{pr_gs_app, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assert(maps:is_key({pr_gs_app, loop, 2}, maps:get(widened_contexts, maps:get(graph, R)))),
    ?assert(lists:member({pr_gs_t, run, 0}, maps:get(reachable, R))),
    ?assertNot(lists:member({pr_gs_t, dead, 0}, maps:get(reachable, R))).

%% `erlang:is_builtin/3\' answers for the OTP running packbeam, not for
%% AtomVM, which implements some of those functions in erlang.beam. When the
%% supplied erlang module has a real body for the function, that body runs
%% and must be kept, along with what it calls.
erlang_module_implementation_test() ->
    App = fixture(
        pr_bif_app, "-export([start/0]). start()->erlang:send_after(10,erlang:self(),tick)."
    ),
    Erlang = fixture(
        erlang,
        "-export([send_after/3,unused/0]). send_after(T,D,M)->pr_bif_timer:schedule(T,D,M). unused()->ok."
    ),
    Timer = fixture(pr_bif_timer, "-export([schedule/3]). schedule(_,_,_)->erlang:make_ref()."),
    {Out, R} = packbeam_prune:run([App, Erlang, Timer], [], [{pr_bif_app, start, 0}], #{}),
    ?assert(lists:member({erlang, send_after, 3}, maps:get(reachable, R))),
    ?assert(lists:member({pr_bif_timer, schedule, 3}, maps:get(reachable, R))),
    ?assertEqual([pr_bif_app, erlang, pr_bif_timer], [module(X) || X <- Out]).

%% Merging contexts of one identity can produce a value whose own identity is
%% different: two constant request tuples join into a choice. The partition a
%% context belongs to is decided once, when it arrives; otherwise a merged
%% context leaves its partition, its call site re-creates it, and the fixed
%% point never settles.
widening_partition_stability_test_() ->
    {timeout, 60, fun widening_partition_stability/0}.

widening_partition_stability() ->
    App = fixture(
        pr_stable,
        "-export([start/0,call/2]). start()->call(m1,{req,1,x}),call(m2,{req,2,x}),call(m3,{req,3,x}),call(m4,{req,4,x}),call(m5,{req,5,x}),call(m6,{req,6,x}),call(m7,{req,7,x}),call(m8,{req,8,x}),call(m9,{req,9,x}),call(m10,{req,10,x}),call(m11,{req,11,x}),call(m12,{req,12,x}),call(m13,{req,13,x}),call(m14,{req,14,x}),call(m15,{req,15,x}),call(m16,{req,16,x}),call(m17,{req,17,x}),call(m18,{req,18,x}),call(m19,{req,19,x}),call(m20,{req,20,x}),call(m21,{req,21,x}),call(m22,{req,22,x}),call(m23,{req,23,x}),call(m24,{req,24,x}),call(m25,{req,25,x}),call(m26,{req,26,x}),call(m27,{req,27,x}),call(m28,{req,28,x}),call(m29,{req,29,x}),call(m30,{req,30,x}),call(m31,{req,31,x}),call(m32,{req,32,x}),call(m33,{req,33,x}),call(m34,{req,34,x}),call(m35,{req,35,x}),call(m36,{req,36,x}),call(m37,{req,37,x}),call(m38,{req,38,x}),call(m39,{req,39,x}),call(m40,{req,40,x}),call(m41,{req,41,x}),call(m42,{req,42,x}),call(m43,{req,43,x}),call(m44,{req,44,x}),call(m45,{req,45,x}),call(m46,{req,46,x}),call(m47,{req,47,x}),call(m48,{req,48,x}),call(m49,{req,49,x}),call(m50,{req,50,x}),call(m51,{req,51,x}),call(m52,{req,52,x}),call(m53,{req,53,x}),call(m54,{req,54,x}),call(m55,{req,55,x}),call(m56,{req,56,x}),call(m57,{req,57,x}),call(m58,{req,58,x}),call(m59,{req,59,x}),call(m60,{req,60,x}),call(m61,{req,61,x}),call(m62,{req,62,x}),call(m63,{req,63,x}),call(m64,{req,64,x}),call(m65,{req,65,x}),call(m66,{req,66,x}),call(m67,{req,67,x}),call(m68,{req,68,x}),call(m69,{req,69,x}),call(m70,{req,70,x}). call(Msg,Req)->{Msg,Req}."
    ),
    Self = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        Self ! {done, packbeam_prune:run([App], [], [{pr_stable, start, 0}], #{})}
    end),
    receive
        {done, {_, R}} ->
            erlang:demonitor(Ref, [flush]),
            ?assertEqual([], maps:get(warnings, R)),
            ?assert(
                maps:is_key({pr_stable, call, 2}, maps:get(widened_contexts, maps:get(graph, R)))
            );
        {'DOWN', Ref, process, Pid, Reason} ->
            error({prune_crashed, Reason})
    after 30000 ->
        exit(Pid, kill),
        error(fixed_point_does_not_settle)
    end.

%% A send of an unknown term does not make every control message possible.
%% A protocol-tagged tuple only exists if reachable code builds it, so a
%% request branch nobody constructs stays impossible even next to an opaque
%% send.
opaque_send_protocol_bound_test() ->
    B = fixture(
        pr_proto,
        "-export([start/0,loop/0,dead/0]). start()->erlang:self() ! erlang:get(anything),erlang:self() ! {'$gen_call',erlang:self(),status},loop(). loop()->receive {'$gen_call',_,{run,M,F}} -> M:F(); {'$gen_call',_,_} -> ok end. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_proto, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{loop, 0}, {start, 0}], exports(Out)).

%% Tuple-building BIFs can produce a protocol tag the analysis never saw
%% constructed, once that tag has been used as a plain value: here it is
%% stored, so the list the tuple is built from may hold it.
unknown_tuple_construction_test() ->
    B = fixture(
        pr_proto_bif,
        "-export([start/0,loop/0]). start()->erlang:put(tag,'$gen_call'),erlang:self() ! erlang:list_to_tuple(erlang:get(parts)),loop(). loop()->receive {'$gen_call',_,{run,M,F}} -> M:F() end."
    ),
    {_, R} = packbeam_prune:run([B], [], [{pr_proto_bif, start, 0}], #{}),
    ?assert(
        lists:any(fun(#{reason := Reason}) -> Reason =:= dynamic_module end, maps:get(warnings, R))
    ).

%% A tuple built with a tag the analysis cannot see only hides a protocol
%% message if that protocol's tag was ever used as a plain value, which is how
%% `gen:call\' receives `\'$gen_call\'\' or `system\'. Here `run\' is passed
%% around as data and `\'$stop\'\' never is.
protocol_tag_escape_test() ->
    B = fixture(
        pr_escape,
        "-export([start/0,loop/0,dead/0,live/0]). start()->T=erlang:get(tag),erlang:put(k,'$run'),erlang:self() ! {T,pr_escape,live},loop(). loop()->receive {'$run',M,F} -> M:F(); {'$stop',M,F} -> M:F(),dead() end. dead()->unused. live()->ok."
    ),
    {_, R} = packbeam_prune:run([B], [], [{pr_escape, start, 0}], #{}),
    ?assertNot(lists:member({pr_escape, dead, 0}, maps:get(reachable, R))).

%% A catch handler receives its class in x0, and it is one of three atoms.
catch_class_test() ->
    B = fixture(
        pr_class,
        "-export([start/0,loop/0,dead/0]). start()->try erlang:get(x) catch C:R:St -> erlang:self() ! {C,R,St} end,loop(). loop()->receive {'$gen_call',_,{run,M,F}} -> M:F(); _ -> ok end. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_class, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{loop, 0}, {start, 0}], exports(Out)).

%% A tuple built with a tag the analysis cannot see matters only if it can be
%% sent: as a request payload inside `{\'$gen_call\', From, Req}\' it is never a
%% message itself, so a protocol whose tag escapes stays bounded.
unknown_tag_payload_test() ->
    B = fixture(
        pr_payload,
        "-export([start/0,loop/0,dead/0]). start()->erlang:put(k,'$gen_call'),T=erlang:get(time),erlang:self() ! {'$gen_call',erlang:self(),{T,x}},loop(). loop()->receive {'$gen_call',_,{run,M,F}} -> M:F(); _ -> ok end. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_payload, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{loop, 0}, {start, 0}], exports(Out)).

%% A closure reached through an unknown fun value runs with the variables it
%% captured when it was built, not with arbitrary ones.
closure_fallback_captures_test() ->
    App = fixture(
        pr_cap_app,
        "-export([start/0,mkfun/1]). start()->erlang:put(g,mkfun(pr_cap_t)),erlang:put(k,pr_cap_o),H=erlang:get(h),H(). mkfun(M)->fun() -> M:live() end."
    ),
    T = fixture(pr_cap_t, "-export([live/0]). live()->t."),
    O = fixture(pr_cap_o, "-export([live/0]). live()->o."),
    {_, R} = packbeam_prune:run([App, T, O], [], [{pr_cap_app, start, 0}], #{}),
    ?assert(lists:member({pr_cap_t, live, 0}, maps:get(reachable, R))),
    ?assertNot(lists:member({pr_cap_o, live, 0}, maps:get(reachable, R))).

%% Calling a function a supplied module does not define raises undef: the
%% call does not return, so what follows it is unreachable, as with a module
%% that is not supplied at all.
undefined_function_call_test() ->
    App = fixture(
        pr_undef_app,
        "-export([start/0,dead/0]). start()->pr_undef_lib:missing(),M=erlang:get(m),F=erlang:get(f),M:F(). dead()->unused."
    ),
    Lib = fixture(pr_undef_lib, "-export([present/0]). present()->ok."),
    {_, R} = packbeam_prune:run([App, Lib], [], [{pr_undef_app, start, 0}], #{}),
    ?assertNot(
        lists:any(fun(#{reason := Reason}) -> Reason =:= dynamic_module end, maps:get(warnings, R))
    ).

%% A record whose fields were already joined covers the constant records with
%% its tag and arity, as a direct join would merge them. Keeping them apart in
%% a choice lets every round add a member, widen, and split again: the
%% summaries of a recursive parser then never settle.
join_absorbs_same_record_test() ->
    Wide = {tuple, [{const, format}, {choices, [{const, a}, {const, b}]}]},
    Choice = {choices, [{const, {other, 1}}, Wide]},
    ?assertEqual(Choice, packbeam_prune:join(Choice, {const, {format, a}})),
    ?assertEqual(Choice, packbeam_prune:join({const, {format, b}}, Choice)),
    ?assertEqual(
        {choices, [{const, {other, 1}}, {tuple, [{const, format}, unknown]}]},
        packbeam_prune:join(Choice, {tuple, [{const, format}, unknown]})
    ).

%% The compiler reads every field of a callback's return before testing its
%% tag, and may write the last field over the register holding the tuple. The
%% tag test must still narrow the fields read earlier: a `{stop, Reason, State}'
%% reason is not a `{noreply, State, Timeout}' state.
overwritten_tuple_projection_test() ->
    App = fixture(
        pr_proj_app,
        "-export([start/0]). -record(st,{mod,timeout})."
        " start()->handle(erlang:get(p),erlang:get(d),#st{mod=pr_proj_next})."
        " handle(P,D,St)->case pr_proj_cb:handle(erlang:get(x)) of"
        " {noreply,S}->continue(P,D,St#st{mod=S,timeout=infinity});"
        " {noreply,S,T}->continue(P,D,St#st{mod=S,timeout=T});"
        " {stop,R,S}->terminate(R,P,D,St#st{mod=S});"
        " _->terminate(error,P,D,St) end."
        " continue(_,_,#st{mod=S,timeout=T})->S:run(T)."
        " terminate(R,_,_,#st{mod=S})->S:stop(R)."
    ),
    Cb = fixture(
        pr_proj_cb,
        "-export([handle/1]). handle(a)->{noreply,pr_proj_next,0};"
        " handle(b)->{stop,shutdown,pr_proj_next}; handle(_)->{noreply,pr_proj_next}."
    ),
    Next = fixture(pr_proj_next, "-export([run/1,stop/1]). run(T)->T. stop(R)->R."),
    {_, R} = packbeam_prune:run([App, Cb, Next], [], [{pr_proj_app, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)).

%% A choice never keeps two alternatives that a direct join would merge: a
%% tuple whose tag is not a constant atom covers every tuple of its arity, and
%% an abstract map covers the constant maps. Otherwise joining a covered value
%% again changes the result, and a widened context never settles.
join_is_idempotent_test() ->
    Wild = {tuple, [unknown, unknown]},
    Consts = {choices, [{const, {a, 1}}, {const, {b, 2}}]},
    ?assertEqual(Wild, packbeam_prune:join(Consts, Wild)),
    ?assertEqual(Wild, packbeam_prune:join(Wild, Consts)),
    Mixed = {choices, [{const, x}, {const, {a, 1}}]},
    ?assertEqual({choices, [{const, x}, Wild]}, packbeam_prune:join(Mixed, Wild)),
    Map = {map, #{k => unknown}},
    Maps = {choices, [{const, #{k => 1}}, {const, #{k => 2}}]},
    ?assertEqual(Map, packbeam_prune:join(Maps, Map)),
    J = packbeam_prune:join(Mixed, Wild),
    ?assertEqual(J, packbeam_prune:join(J, {const, {c, 3}})).

%% A list whose tail is not known to be a list, as a scanner's accumulator
%% `[C | Acc]' is, joins cell by cell: kept as alternatives, each nested in
%% the next tail, it grows exponentially with the loop's iterations.
open_list_join_test() ->
    Open = {cons, unknown, unknown},
    Longer = {cons, {const, $a}, {cons, {const, $b}, unknown}},
    ?assertEqual(Open, packbeam_prune:join(Open, Longer)),
    ?assertEqual(Open, packbeam_prune:join(Longer, Open)),
    ?assertMatch({choices, [_, _]}, packbeam_prune:join(Open, {cons, {const, $a}, {const, []}})),
    Grown = lists:foldl(
        fun(C, Acc) -> packbeam_prune:join(Acc, {cons, {const, C}, Acc}) end,
        Open,
        lists:seq($a, $z)
    ),
    ?assertEqual(Open, Grown),
    Choices = {choices, [{const, x}, Longer]},
    ?assertEqual({choices, [{const, x}, Open]}, packbeam_prune:join(Choices, Open)).

%% Only values that can decide a dynamic call are relevant: a discarded result
%% reads nothing of the callee's return, and a parameter that never reaches a
%% call target does not need its own contexts.
relevance_test() ->
    B = fixture(
        pr_rel,
        "-export([start/0]). start()->_=work(a),_=work(b),_=work(c),M=pick(erlang:get(k)),dispatch(M,x). work(X)->{X,X}. pick(1)->pr_left; pick(_)->pr_right. dispatch(M,Tag)->_=Tag,M:run()."
    ),
    L = fixture(pr_left, "-export([run/0,dead/0]). run()->left. dead()->dead."),
    R = fixture(pr_right, "-export([run/0,dead/0]). run()->right. dead()->dead."),
    U = fixture(pr_unused, "-export([run/0]). run()->unused."),
    Modules = maps:from_list([
        {packbeam_beam_module(X), packbeam_prune:index_module(packbeam_beam:read(X))}
     || X <- [B, L, R, U]
    ]),
    #{params := Params, returns := Returns} = packbeam_relevance:compute(Modules),
    ?assertEqual([], maps:get({pr_rel, work, 1}, Params)),
    ?assertEqual([], maps:get({pr_rel, pick, 1}, Params)),
    ?assertEqual([0], maps:get({pr_rel, dispatch, 2}, Params)),
    ?assert(maps:is_key({pr_rel, pick, 1}, Returns)),
    ?assertNot(maps:is_key({pr_rel, work, 1}, Returns)),
    %% Adaptive precision gives contexts only where a call target depends on
    %% them.
    {Out, Report} = packbeam_prune:run([B, L, R, U], [], [{pr_rel, start, 0}], #{
        precision => adaptive
    }),
    Arguments = maps:get(arguments, maps:get(graph, Report)),
    ?assertEqual(1, length(maps:get({pr_rel, work, 1}, Arguments))),
    ?assertEqual(3, length(Out)),
    ?assertEqual([], maps:get(warnings, Report)),
    {_, Precise} = packbeam_prune:run([B, L, R, U], [], [{pr_rel, start, 0}], #{}),
    ?assertEqual(
        3, length(maps:get({pr_rel, work, 1}, maps:get(arguments, maps:get(graph, Precise))))
    ).

%% Once a received message decides a call, the message sent is relevant. It
%% is not always the last argument: the options of send/3, send_after/4 and
%% start_timer/4 come after it.
send_message_relevance_test_() ->
    [
        {Send, fun() ->
            B = fixture(
                pr_send_rel,
                "-export([start/0]). start()->cmd(pr_send_rel,live),cmd(pr_send_rel,other),loop(). cmd(M,F)->_=" ++
                    Send ++
                    ",ok. loop()->receive {'$cmd',M,F} -> M:F(); {timeout,_,{'$cmd',M,F}} -> M:F() end."
            ),
            Modules = #{pr_send_rel => packbeam_prune:index_module(packbeam_beam:read(B))},
            #{params := Params} = packbeam_relevance:compute(Modules),
            ?assertEqual([1], maps:get({pr_send_rel, cmd, 2}, Params))
        end}
     || Send <- [
            "erlang:send(erlang:self(),{'$cmd',M,F})",
            "erlang:send(erlang:self(),{'$cmd',M,F},[])",
            "erlang:send_after(0,erlang:self(),{'$cmd',M,F})",
            "erlang:send_after(0,erlang:self(),{'$cmd',M,F},[])",
            "erlang:start_timer(0,erlang:self(),{'$cmd',M,F})",
            "erlang:start_timer(0,erlang:self(),{'$cmd',M,F},[])"
        ]
    ].

packbeam_beam_module(B) ->
    {ok, {M, _}} = beam_lib:chunks(B, []),
    M.

%% A tree built in a loop nests its alternatives in every branch: bounding
%% its depth alone leaves a value exponential in that depth (the compiler's
%% `#cg_cons{}' chains).
recursive_record_size_test_() ->
    {timeout, 60, fun() ->
        B = fixture(
            pr_tree,
            "-export([start/0]). start()->walk(erlang:get(k),leaf). walk([],Acc)->Acc; walk([{a,X}|T],Acc)->walk(T,{node,{left,X},Acc}); walk([{b,X}|T],Acc)->walk(T,{node,Acc,{right,X}}); walk([_|T],Acc)->walk(T,{pair,Acc,Acc})."
        ),
        {_, R} = packbeam_prune:run([B], [], [{pr_tree, start, 0}], #{}),
        Contexts = maps:get({pr_tree, walk, 2}, maps:get(arguments, maps:get(graph, R))),
        ?assert(lists:all(fun(C) -> erts_debug:flat_size(C) < 20000 end, Contexts))
    end}.

%% Past the choice limit, alternatives merge per record (tag and arity) before
%% they merge per arity: a server's `{reply, Reply, State}' and
%% `{noreply, State, Timeout}' returns keep each field tied to its tag.
widening_keeps_record_tags_test() ->
    Replies = [{const, {reply, I, s}} || I <- lists:seq(1, 20)],
    NoReplies = [{const, {noreply, s, I}} || I <- lists:seq(1, 20)],
    V = lists:foldl(fun(A, Acc) -> packbeam_prune:join(Acc, A) end, none, Replies ++ NoReplies),
    Ints = {choices, [{const, I} || I <- lists:seq(1, 20)]},
    ?assertEqual(
        {choices, [
            {tuple, [{const, noreply}, {const, s}, Ints]},
            {tuple, [{const, reply}, Ints, {const, s}]}
        ]},
        V
    ).

%% A branch that no alternative of the tested value can take is not
%% analyzed: its fields would otherwise mix the other alternatives' fields.
impossible_branch_test() ->
    App = fixture(
        pr_imp_app,
        "-export([start/0]). start()->case pr_imp_cb:handle(erlang:get(x)) of"
        " {reply,R,S}->S:run(R); {noreply,S,T}->S:run(T); {stop,R,S}->S:stop(R) end."
    ),
    Cb = fixture(
        pr_imp_cb,
        "-export([handle/1]). handle(a)->{reply,x,pr_imp_next}; handle(_)->{stop,normal,pr_imp_next}."
    ),
    Next = fixture(pr_imp_next, "-export([run/1,stop/1]). run(T)->T. stop(R)->R."),
    {_, R} = packbeam_prune:run([App, Cb, Next], [], [{pr_imp_app, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)).

%% Past the context budget, a keyed lookup keeps one partition per constant
%% key: merging every key's list would lose the module stored under one of them.
keyed_context_partition_test() ->
    Calls = lists:flatten([
        io_lib:format("pr_key_lib:get(k~b,[{k~b,~b}]),pr_key_lib:get(k~b,[{x,0},{k~b,~b}]),", [
            I, I, I, I, I, I
        ])
     || I <- lists:seq(1, 40)
    ]),
    App = fixture(
        pr_key_app,
        "-export([start/0]). start()->" ++ Calls ++
            "pr_key_lib:get(other,[{other,pr_key_other}]),"
            "M=pr_key_lib:get(mod,[{other,1},{mod,pr_key_target}]),M:run()."
    ),
    Lib = fixture(
        pr_key_lib,
        "-export([get/2]). get(_,[])->undefined; get(K,[{K,V}|_])->V; get(K,[_|T])->get(K,T)."
    ),
    Target = fixture(pr_key_target, "-export([run/0]). run()->ok."),
    Other = fixture(pr_key_other, "-export([run/0]). run()->ok."),
    %% One context per function merges the lookups: only the full or adaptive
    %% budget keeps them apart.
    lists:foreach(
        fun(Precision) ->
            {_, R} = packbeam_prune:run([App, Lib, Target, Other], [], [{pr_key_app, start, 0}], #{
                precision => Precision
            }),
            Reachable = maps:get(reachable, R),
            ?assert(lists:member({pr_key_target, run, 0}, Reachable)),
            ?assertEqual(
                Precision =:= insensitive, lists:member({pr_key_other, run, 0}, Reachable)
            )
        end,
        [full, insensitive, adaptive]
    ).

%% Merged contexts can still resolve a call, to more targets than any single
%% call reaches: that call is imprecise, and adaptive precision refines it too.
adaptive_merged_targets_test() ->
    Calls = lists:flatten([
        io_lib:format("pr_mt_lib:get(k~b,[{k~b,pr_mt_target}]),", [I, I])
     || I <- lists:seq(1, 40)
    ]),
    App = fixture(
        pr_mt_app,
        "-export([start/0]). start()->" ++ Calls ++
            "pr_mt_lib:get(other,[{other,pr_mt_other}]),"
            "M=pr_mt_lib:get(mod,[{mod,pr_mt_target}]),M:run()."
    ),
    Lib = fixture(
        pr_mt_lib,
        "-export([get/2]). get(K,[{K,V}|_])->V; get(K,[_|T])->get(K,T)."
    ),
    Target = fixture(pr_mt_target, "-export([run/0]). run()->ok."),
    Other = fixture(pr_mt_other, "-export([run/0]). run()->ok."),
    lists:foreach(
        fun(Precision) ->
            {_, R} = packbeam_prune:run([App, Lib, Target, Other], [], [{pr_mt_app, start, 0}], #{
                precision => Precision
            }),
            ?assertEqual(
                Precision =:= insensitive,
                lists:member({pr_mt_other, run, 0}, maps:get(reachable, R))
            )
        end,
        [full, insensitive, adaptive]
    ).

%% `code:ensure_loaded/1' answers `{module, M}' for the module it is given,
%% which is how Elixir protocols find their implementation. The module must
%% stay in the output, even when nothing else calls it, or the answer changes.
ensure_loaded_test() ->
    App = fixture(
        pr_el_app,
        "-export([start/0]). start()->erlang:put(x,pr_el_other),"
        " {module,_}=code:ensure_loaded(pr_el_probe),"
        " case code:ensure_loaded(pr_el_impl) of {module,M}->M:run(); {error,_}->none end."
    ),
    Code = fixture(
        code, "-export([ensure_loaded/1]). ensure_loaded(_)->erlang:nif_error(undefined)."
    ),
    Impl = fixture(pr_el_impl, "-export([run/0]). run()->ok."),
    Other = fixture(pr_el_other, "-export([run/0]). run()->ok."),
    Probe = fixture(pr_el_probe, "-export([dead/0]). dead()->unused."),
    {Out, R} = packbeam_prune:run(
        [App, Code, Impl, Other, Probe], [], [{pr_el_app, start, 0}], #{}
    ),
    ?assertEqual([], maps:get(warnings, R)),
    Reachable = maps:get(reachable, R),
    ?assert(lists:member({pr_el_impl, run, 0}, Reachable)),
    ?assertNot(lists:member({pr_el_other, run, 0}, Reachable)),
    [ProbeOut] = [B || B <- Out, {ok, {pr_el_probe, _}} <- [beam_lib:chunks(B, [])]],
    {module, pr_el_probe} = code:load_binary(pr_el_probe, "pr_el_probe.beam", ProbeOut),
    try
        ?assertEqual(pr_el_probe, proplists:get_value(module, pr_el_probe:module_info()))
    after
        code:delete(pr_el_probe),
        code:purge(pr_el_probe)
    end.

%% A path that raises does not return. Recording its tail call as an
%% arbitrary return value hides the callback module the other paths return.
raising_tail_call_test() ->
    App = fixture(
        pr_raise_app,
        "-export([start/0]). start()->{ok,M}=pr_raise_lib:init(erlang:get(x)),M:run()."
    ),
    Lib = fixture(
        pr_raise_lib,
        "-export([init/1]). init(a)->{ok,pr_raise_target}; init(b)->exit(stop); init(c)->throw(stop);"
        " init(d)->erlang:raise(error,stop,[]); init(X)->erlang:error({bad,X})."
    ),
    Target = fixture(pr_raise_target, "-export([run/0]). run()->ok."),
    {_, R} = packbeam_prune:run([App, Lib, Target], [], [{pr_raise_app, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assert(lists:member({pr_raise_target, run, 0}, maps:get(reachable, R))).

absent_module_call_test() ->
    App = fixture(
        pr_absent_app,
        "-export([start/0,live/0,dead/0]). start()->case erlang:get(flavor) of jit -> pr_absent_jit:compile(),dead(); _ -> live() end. live()->42. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([App], [], [{pr_absent_app, start, 0}], #{}),
    ?assertEqual([{live, 0}, {start, 0}], exports(Out)),
    [W] = maps:get(warnings, R),
    ?assertEqual({absent, pr_absent_jit}, maps:get(scope, W)),
    ?assertEqual({missing_module, pr_absent_jit}, maps:get(reason, W)),
    Text = lists:flatten(packbeam_prune:format_warning(W)),
    ?assertEqual(nomatch, string:find(Text, "ALL output")),
    ?assertNotEqual(nomatch, string:find(Text, "pr_absent_jit")),
    {module, pr_absent_app} = code:load_binary(pr_absent_app, "pr_absent_app.beam", Out),
    try
        ?assertEqual(42, pr_absent_app:start())
    after
        code:delete(pr_absent_app),
        code:purge(pr_absent_app)
    end.

unnamed_callback_module_test() ->
    App = fixture(
        pr_cb_app,
        "-export([start/0]). start()->erlang:put(handler,pr_cb_named),M=erlang:get(handler),M:log(event,#{})."
    ),
    Named = fixture(pr_cb_named, "-export([log/2,dead/0]). log(E,C)->{named,E,C}. dead()->unused."),
    Unnamed = fixture(pr_cb_other, "-export([log/2]). log(E,C)->{other,E,C}."),
    {Out, R} = packbeam_prune:run([App, Named, Unnamed], [], [{pr_cb_app, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([pr_cb_app, pr_cb_named], [module(B) || B <- Out]),
    ?assertEqual([{log, 2}], exports(lists:last(Out))).

unknown_module_signature_unbounded_dependency_test() ->
    App = fixture(
        pr_log_unbounded,
        "-export([start/1,dead/0]). start(M)->erlang:put(known,pr_log_unsafe),M:log(event,#{}). dead()->dead."
    ),
    Handler = fixture(
        pr_log_unsafe,
        "-export([log/2]). log(_,_) -> M=erlang:get(module),F=erlang:get(function),M:F()."
    ),
    {Out, Report} = packbeam_prune:run([App, Handler], [], [{pr_log_unbounded, start, 1}], #{}),
    %% The retained handler makes an unbounded call, which falls back to the
    %% exports of every module the analyzed code names.
    ?assert(
        lists:any(
            fun(#{scope := Scope}) -> Scope =:= named_modules end, maps:get(warnings, Report)
        )
    ),
    ?assertEqual([[{start, 1}], [{log, 2}]], [exports(X) || X <- Out]).

unknown_module_native_signature_test() ->
    App = fixture(
        pr_native_signature,
        "-export([start/1,dead/0]). start(M)->M:digital_write(2,high). dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([App], [], [{pr_native_signature, start, 1}], #{}),
    ?assertEqual([{start, 1}], exports(Out)),
    ?assert(lists:member({gpio, digital_write, 2}, maps:get(nifs, R))).

unknown_module_apply_signature_test() ->
    App = fixture(
        pr_apply_signature,
        "-export([start/1,callback/0,dead/0]). start(M)->M:apply(fun pr_apply_signature:callback/0,[]). callback()->ok. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([App], [], [{pr_apply_signature, start, 1}], #{}),
    ?assertEqual([{callback, 0}, {start, 1}], exports(Out)),
    ?assertEqual([], maps:get(warnings, R)),
    ?assert(
        lists:member({erlang, apply, 2}, dispatch_targets(R, {pr_apply_signature, start, 1}))
    ).

known_paths_after_unbounded_call_test() ->
    App = fixture(
        pr_late_app,
        "-export([start/2,boot/1]). start(M,F)->M:F(),boot(pr_late_target). boot(M)->M:start()."
    ),
    Target = fixture(pr_late_target, "-export([start/0]). start()->ok."),
    {Out, Report} = packbeam_prune:run([App, Target], [], [{pr_late_app, start, 2}], #{}),
    ?assertEqual([[{boot, 1}, {start, 2}], [{start, 0}]], [exports(X) || X <- Out]),
    Graph = maps:get(graph, Report),
    ?assertEqual(false, maps:get(complete, Graph)),
    Sites = maps:get(dispatches, Graph),
    ?assert(
        lists:any(
            fun
                (
                    {{{pr_late_app, boot, 1}, _}, #{
                        modules := {const, pr_late_target}, targets := Ts
                    }}
                ) ->
                    lists:member({pr_late_target, start, 0}, Ts);
                (_) ->
                    false
            end,
            maps:to_list(Sites)
        )
    ).

discovery_budget_retains_output_test() ->
    App = fixture(
        pr_discovery_budget,
        "-export([start/0,dead/0]). start()->erlang:load_nif(\"lib\",0),ok. dead()->unused."
    ),
    {[App], Report} = packbeam_prune:run([App], [], [{pr_discovery_budget, start, 0}], #{
        discovery_budget => 0
    }),
    ?assertMatch(#{complete := false, discovery_limited := true}, maps:get(graph, Report)).

%% A guard that matched proves a shape: forwarding a value whose tagged-tuple
%% shape was just tested is not an opaque send, and an equality test binds the
%% constant it compared against.
matched_shape_send_test() ->
    B = fixture(
        pr_shape,
        "-export([start/0,fwd/1,eq/1]). start()->fwd(erlang:get(msg)),eq(erlang:get(mode)),receive {tag,X}->X end. fwd({tag,_}=M)->erlang:self() ! M. eq(M)->case M of ready -> erlang:self() ! {M,now}; _ -> ok end."
    ),
    {_, R} = packbeam_prune:run([B], [], [{pr_shape, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    G = maps:get(graph, R),
    ?assertEqual(#{}, maps:get(opaque_senders, G)),
    Ms = maps:get(messages, G),
    ?assert(maps:is_key({tag, 2}, Ms)),
    ?assert(maps:is_key({ready, 2}, Ms)).

%% Forwarding a message taken from the mailbox re-sends a message that some
%% reachable sender already produced. It introduces no new message shape, so
%% it must not disable control-message bounds.
forwarded_message_test() ->
    B = fixture(
        pr_fwd,
        "-export([start/0,loop/1]). start()->erlang:spawn(pr_fwd,loop,[erlang:self()]),erlang:self() ! {ping,1},ok. loop(P)->receive M -> fwd(P,M) end. fwd(P,M)->P ! M."
    ),
    {_, R} = packbeam_prune:run([B], [], [{pr_fwd, start, 0}], #{}),
    G = maps:get(graph, R),
    ?assertEqual(#{}, maps:get(opaque_senders, G)),
    ?assertEqual([{ping, 2}], maps:keys(maps:get(messages, G))).

%% Forwarding a part of a received message can only produce a term that some
%% reachable sender already built. Bound such a send by the matching subterms
%% of the recorded messages instead of losing every control-message bound.
forwarded_part_test() ->
    B = fixture(
        pr_part,
        "-export([start/0,loop/0,live/0,dead/0]). start()->erlang:self() ! {wrap,{'$cmd',pr_part,live}},loop(). loop()->receive {wrap,Inner} -> erlang:self() ! Inner, loop(); {'$cmd',M,F} -> M:F() end. live()->42. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_part, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{live, 0}, {loop, 0}, {start, 0}], exports(Out)),
    ?assert(lists:member({pr_part, live, 0}, maps:get(reachable, R))).

%% A receive whose message could not be bounded still holds a term that came
%% from the mailbox. Keep that provenance instead of widening it to an
%% arbitrary value, so forwarding it stays bounded.
unbounded_receive_keeps_provenance_test() ->
    B = fixture(
        pr_keep,
        "-export([start/0,loop/0]). start()->erlang:self() ! {erlang:get(tag),1},erlang:self() ! {'$ping',erlang:get(x)},loop(). loop()->receive {'$ping',_} = M -> erlang:self() ! M, ok end."
    ),
    {_, R} = packbeam_prune:run([B], [], [{pr_keep, start, 0}], #{}),
    ?assertEqual(
        [{pr_keep, start, 0}],
        maps:keys(maps:get(opaque_senders, maps:get(graph, R)))
    ).

%% A control message is bounded by the requests reachable code sends, kept
%% apart rather than joined: a server branch for a request nobody sends is
%% still impossible once there are more shapes than the choice bound.
message_alternatives_test() ->
    B = fixture(
        pr_alt,
        "-export([start/0,loop/0,dead/0]). start()->Self=erlang:self(),erlang:self() ! {'$gen_call',Self,a1},erlang:self() ! {'$gen_call',Self,a2},erlang:self() ! {'$gen_call',Self,a3},erlang:self() ! {'$gen_call',Self,a4},erlang:self() ! {'$gen_call',Self,a5},erlang:self() ! {'$gen_call',Self,a6},erlang:self() ! {'$gen_call',Self,a7},erlang:self() ! {'$gen_call',Self,a8},erlang:self() ! {'$gen_call',Self,a9},erlang:self() ! {'$gen_call',Self,{b1,1}},erlang:self() ! {'$gen_call',Self,{b2,1}},erlang:self() ! {'$gen_call',Self,{b3,1}},erlang:self() ! {'$gen_call',Self,{b4,1}},erlang:self() ! {'$gen_call',Self,{b5,1}},erlang:self() ! {'$gen_call',Self,{b6,1}},erlang:self() ! {'$gen_call',Self,{b7,1}},erlang:self() ! {'$gen_call',Self,{b8,1}},loop(). loop()->receive {'$gen_call',_,{run,M,F}} -> M:F(); {'$gen_call',_,_} -> loop() end. dead()->unused."
    ),
    {[Out], R} = packbeam_prune:run([B], [], [{pr_alt, start, 0}], #{}),
    ?assertEqual([], maps:get(warnings, R)),
    ?assertEqual([{loop, 0}, {start, 0}], exports(Out)).

opaque_mailbox_warning_test() ->
    B = fixture(
        pr_opaque_warning,
        "-export([start/0]). start()->self()!{get(tag),get(payload)},receive {restart_many_children,[{M,F,A}|_]}->apply(M,F,A) end."
    ),
    {_, R} = packbeam_prune:run([B], [], [{pr_opaque_warning, start, 0}], #{}),
    [W] = [X || #{scope := named_modules} = X <- maps:get(warnings, R)],
    ?assertEqual([2], maps:get(opaque_message_arities, W)),
    ?assert(maps:is_key({pr_opaque_warning, start, 0}, maps:get(opaque_senders, W))),
    Text = lists:flatten(packbeam_prune:format_warning(W)),
    ?assertNotEqual(nomatch, string:find(Text, "Opaque mailbox sends")),
    ?assertNotEqual(nomatch, string:find(Text, "pr_opaque_warning:start/0")).

%% A fun whose target the compiler knows is called with `call_fun2' naming
%% its FunT entry, even in a function that does not create it. Keeping that
%% function must keep the fun's code too, or its FunT row points nowhere.
call_fun2_target_test() ->
    B = fixture(
        pr_call_fun2, "-export([start/1]). start(X)->call(fun(Y)->{X,Y} end). call(F)->F(1)."
    ),
    M = packbeam_beam:read(B),
    {_, _, CallOps} = lists:keyfind(call, 1, maps:get(functions, M)),
    ?assert(lists:keymember(call_fun2, 2, CallOps)),
    ?assertNot(lists:keymember(make_fun3, 2, CallOps)),
    Keep = packbeam_beam:close_keep(M, [{call, 1}]),
    ?assert(lists:member({'-start/1-fun-0-', 2}, Keep)),
    %% Without the function creating the fun, the loader of some OTP
    %% releases rejects the module, so check the fun entry names the fun's
    %% code.
    New = packbeam_beam:trim(M, [{call, 1}]),
    {ok, {pr_call_fun2, [{"FunT", <<1:32, _Name:32, 2:32, Target:32, _/binary>>}]}} =
        beam_lib:chunks(New, ["FunT"]),
    {beam_file, pr_call_fun2, _, _, _, Fs} = beam_disasm:file(New),
    ?assertMatch({function, '-start/1-fun-0-', 2, Target, _}, lists:keyfind(Target, 4, Fs)).

%% Past 32767, an operand's first byte has its high bit set, and a reader
%% taking it as signed sees a negative number unless a zero byte comes first,
%% as the compiler writes it. Here labels go past it: keeping every function
%% must give back the compiler's code byte for byte.
large_operand_test_() ->
    {timeout, 120, fun() ->
        N = 16400,
        Exports = lists:join(",", [io_lib:format("f~b/0", [I]) || I <- lists:seq(1, N)]),
        Body = [io_lib:format("f~b()->~b. ", [I, I]) || I <- lists:seq(1, N)],
        B = fixture(pr_large_operand, lists:flatten(["-export([", Exports, "]). ", Body])),
        M = packbeam_beam:read(B),
        New = packbeam_beam:trim(M, [{F, A} || {F, A, _} <- maps:get(functions, M)]),
        {ok, {_, [{"Code", Code}]}} = beam_lib:chunks(B, ["Code"]),
        ?assertEqual({ok, {pr_large_operand, [{"Code", Code}]}}, beam_lib:chunks(New, ["Code"])),
        ?assertMatch(
            {module, pr_large_operand},
            code:load_binary(pr_large_operand, "pr_large_operand.beam", New)
        ),
        ?assertEqual(N, pr_large_operand:f16400()),
        code:purge(pr_large_operand),
        code:delete(pr_large_operand),
        code:purge(pr_large_operand)
    end}.

fixture(M, Body) ->
    Source = lists:flatten(io_lib:format("-module(~p). ~s", [M, Body])),
    {ok, Tokens, _} = erl_scan:string(Source),
    Forms = forms(Tokens, [], []),
    case compile:forms(Forms, [binary, return_errors, no_line_info]) of
        {ok, M, B} -> B;
        Other -> error(Other)
    end.
forms([], [], Acc) ->
    lists:reverse(Acc);
forms([{dot, _} = D | T], Cur, Acc) ->
    {ok, F} = erl_parse:parse_form(lists:reverse([D | Cur])),
    forms(T, [], [F | Acc]);
forms([H | T], Cur, Acc) ->
    forms(T, [H | Cur], Acc).
exports(B) ->
    {ok, {_, [{exports, E}]}} = beam_lib:chunks(B, [exports]),
    lists:sort(E).
literals(B) ->
    {ok, _, Cs} = beam_lib:all_chunks(B),
    case proplists:get_value("LitT", Cs) of
        undefined -> proplists:get_value("LitU", Cs, <<>>);
        <<0:32, D/binary>> -> D;
        <<_:32, D/binary>> -> zlib:uncompress(D)
    end.
execute(M, B, F, A) ->
    code:purge(M),
    code:delete(M),
    {module, M} = code:load_binary(M, "pruned.beam", B),
    try
        apply(M, F, A)
    after
        code:delete(M),
        code:purge(M)
    end.
module(B) ->
    {ok, {M, _}} = beam_lib:chunks(B, []),
    M.
dispatch_targets(Report, Caller) ->
    lists:usort(
        lists:append([
            maps:get(targets, Site)
         || {{C, _}, Site} <- maps:to_list(maps:get(dispatches, maps:get(graph, Report))),
            C =:= Caller
        ])
    ).
