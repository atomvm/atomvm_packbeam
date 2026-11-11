%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(packbeam_jit_types_tests).
-include_lib("eunit/include/eunit.hrl").

-define(XREG, 3).

encoding_test() ->
    Types = [
        atom,
        nil,
        cons,
        list,
        tuple,
        map,
        function,
        float,
        number,
        pid,
        port,
        reference,
        {integer, 1, 5},
        {integer, -(1 bsl 59), (1 bsl 59) - 1},
        {bitstring, 8}
    ],
    [
        ?assertEqual(T, packbeam_types:decode(V, packbeam_types:encode(V, T)))
     || V <- [3, 4], T <- Types
    ],
    %% As the compiler writes them: version 4 moved the flags up a bit.
    ?assertEqual(<<16#6020:16, 1:64, 5:64>>, packbeam_types:encode(4, {integer, 1, 5})),
    ?assertEqual(<<16#3020:16, 1:64, 5:64>>, packbeam_types:encode(3, {integer, 1, 5})),
    ?assertEqual(<<16#8002:16, 7:8>>, packbeam_types:encode(4, {bitstring, 8})),
    %% A bound the entry leaves out is open, and stays open.
    ?assertEqual({integer, '-inf', 9}, packbeam_types:decode(4, <<16#4020:16, 9:64>>)),
    ?assertEqual(
        {integer, 0, 9}, packbeam_types:refine({integer, '-inf', 9}, {integer, 0, 100})
    ).

of_value_test() ->
    ?assertEqual(atom, packbeam_types:of_value({choices, [{const, ok}, {const, error}]})),
    ?assertEqual({integer, 1, 5}, packbeam_types:of_value({choices, [{const, 5}, {const, 1}]})),
    ?assertEqual(list, packbeam_types:of_value({choices, [{const, []}, {cons, unknown, unknown}]})),
    ?assertEqual(tuple, packbeam_types:of_value({choices, [{const, {a}}, {tuple, [unknown]}]})),
    ?assertEqual(none, packbeam_types:of_value({choices, [{const, a}, {const, 1}]})),
    ?assertEqual(none, packbeam_types:of_value({const, 1 bsl 70})),
    ?assertEqual(none, packbeam_types:of_value(unknown)).

%% An exported function's parameters are unknown to the compiler; every call
%% the program makes passes an atom and a small integer.
argument_types_test() ->
    App = fixture(
        pr_jt_app, "-export([start/0]). start()->{pr_jt_lib:f(ok,1),pr_jt_lib:f(error,5)}."
    ),
    Lib = fixture(pr_jt_lib, "-export([f/2]). f(X,N)->case X of ok->N+1; error->N*2 end."),
    ?assertEqual([], typed(Lib)),
    {[_, Plain], _} = packbeam_prune:run([App, Lib], [], [{pr_jt_app, start, 0}], #{}),
    ?assertEqual([], typed(Plain)),
    {[_, Typed], _} = packbeam_prune:run([App, Lib], [], [{pr_jt_app, start, 0}], #{
        jit_types => true
    }),
    case type_version(Typed) of
        %% Older compilers write a version of the chunk that is left untyped.
        Version when Version < 3 ->
            ?assertEqual([], typed(Typed));
        _ ->
            Types = typed(Typed),
            ?assert(lists:member({f, 2, {x, 0}, atom}, Types), Types),
            ?assert(
                lists:any(
                    fun
                        ({f, 2, {x, 1}, {integer, Lo, Hi}}) -> 1 =< Lo andalso Hi =< 5;
                        (_) -> false
                    end,
                    Types
                ),
                Types
            )
    end,
    ?assertEqual({2, 10}, {
        execute(pr_jt_lib, Typed, f, [ok, 1]), execute(pr_jt_lib, Typed, f, [error, 5])
    }).

%% A test every context passes is only work: it goes, with its error branch.
proven_test_dropped_test() ->
    App = fixture(pr_jt_app, "-export([start/0]). start()->pr_jt_lib:g({point,1,2})."),
    Lib = fixture(pr_jt_lib, "-export([g/1]). g({point,X,_})->X."),
    {[_, Plain], _} = packbeam_prune:run([App, Lib], [], [{pr_jt_app, start, 0}], #{}),
    ?assert(lists:member(is_tagged_tuple, op_names(Plain))),
    {[_, Typed], _} = packbeam_prune:run([App, Lib], [], [{pr_jt_app, start, 0}], #{
        jit_types => true
    }),
    ?assertNot(lists:member(is_tagged_tuple, op_names(Typed))),
    ?assertEqual(1, execute(pr_jt_lib, Typed, g, [{point, 1, 2}])).

%% A function an unresolved call may run with arguments the analysis did not
%% see gets no types from it.
retained_function_untyped_test() ->
    A = fixture(
        pr_jt_named,
        "-export([start/0]). start()->pr_jt_y:g(ok),erlang:put(k,{pr_jt_x,f}),M=binary_to_term(erlang:get(key)),F=erlang:get(function),M:F()."
    ),
    X = fixture(pr_jt_x, "-export([f/0]). f()->pr_jt_y:g(erlang:get(z))."),
    Y = fixture(pr_jt_y, "-export([g/1]). g(X)->case X of ok->one; _->two end."),
    {Out, _} = packbeam_prune:run([A, X, Y], [], [{pr_jt_named, start, 0}], #{jit_types => true}),
    ?assertEqual(3, length(Out)),
    ?assertEqual([], typed(lists:nth(3, Out))),
    ?assertEqual(two, execute(pr_jt_y, lists:nth(3, Out), g, [other])).

%% {Function, Arity, Register, Type} for each typed register operand.
typed(B) ->
    D = packbeam_beam:read(B),
    {ok, _, Cs} = beam_lib:all_chunks(B),
    Entries =
        case proplists:get_value("Type", Cs) of
            <<Version:32, _:32, Data/binary>> when Version =:= 3; Version =:= 4 ->
                {Version, entries(Version, Data)};
            _ ->
                none
        end,
    lists:usort([
        {F, A, reg(R), type(Entries, T)}
     || {F, A, Ops} <- maps:get(functions, D),
        {_, _, As} <- Ops,
        {typed, R, {_, T}} <- lists:flatten([operands(X) || X <- As]),
        type(Entries, T) =/= any
    ]).
operands({list, L}) -> [operands(X) || X <- L];
operands(X) -> [X].
reg({?XREG, N}) -> {x, N};
reg({T, N}) -> {T, N}.
type(none, _) -> any;
type({Version, Entries}, I) -> packbeam_types:decode(Version, lists:nth(I + 1, Entries)).
entries(_, <<>>) ->
    [];
entries(Version, <<H:16, _/binary>> = B) ->
    Shift =
        case Version of
            3 -> 12;
            4 -> 13
        end,
    Size =
        2 + 8 * ((H bsr Shift) band 1) + 8 * ((H bsr (Shift + 1)) band 1) +
            ((H bsr (Shift + 2)) band 1),
    <<E:Size/binary, R/binary>> = B,
    [E | entries(Version, R)].
type_version(B) ->
    {ok, _, Cs} = beam_lib:all_chunks(B),
    case proplists:get_value("Type", Cs) of
        <<Version:32, _/binary>> -> Version;
        undefined -> 0
    end.
op_names(B) ->
    [Name || {_, _, Ops} <- maps:get(functions, packbeam_beam:read(B)), {_, Name, _} <- Ops].

fixture(M, Body) ->
    Source = lists:flatten(io_lib:format("-module(~p). ~s", [M, Body])),
    {ok, Tokens, _} = erl_scan:string(Source),
    {ok, M, B} = compile:forms(forms(Tokens, [], []), [binary, no_line_info]),
    B.
forms([], [], Acc) ->
    lists:reverse(Acc);
forms([{dot, _} = D | T], Cur, Acc) ->
    {ok, F} = erl_parse:parse_form(lists:reverse([D | Cur])),
    forms(T, [], [F | Acc]);
forms([H | T], Cur, Acc) ->
    forms(T, [H | Cur], Acc).
execute(M, B, F, A) ->
    code:purge(M),
    code:delete(M),
    {module, M} = code:load_binary(M, "typed.beam", B),
    try
        apply(M, F, A)
    after
        code:delete(M),
        code:purge(M)
    end.
