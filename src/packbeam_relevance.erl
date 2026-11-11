%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% @doc Which values can decide a dynamic call.
%%
%% Pruning needs precise values only where they select a call target: the
%% module and function of `M:F(...)', the fun called, the arguments of spawn
%% and apply, the names given to atom-creating built-ins. This pass walks each
%% function's bytecode backwards from those operands and marks the registers
%% whose values can flow into them, across calls: a relevant call result makes
%% the callee's return relevant, a relevant callee parameter makes the
%% argument register relevant. The analysis then keeps per-argument contexts
%% only for relevant parameters and reads return values only where they are
%% relevant: `_ = m:f(Args)' costs one imprecise context of `f'.
%%
%% Every step over-approximates: an instruction it does not model links all
%% its registers, an exception handler reads the y registers of any
%% instruction it protects, and a dynamic call takes the relevance of every
%% function it can reach.
-module(packbeam_relevance).
-include("compact_term.hrl").
-export([compute/1, compute/2]).

%% Built-ins whose arguments the analysis always reads: they select call
%% targets, load code, or create atoms that can later name a module.
-define(SINKS, [
    {erlang, apply, 2},
    {erlang, apply, 3},
    {erlang, hibernate, 3},
    {erlang, make_fun, 3},
    {erlang, function_exported, 3},
    {erlang, get_module_info, 1},
    {erlang, get_module_info, 2},
    {erlang, process_flag, 2},
    {erlang, load_module, 2},
    {erlang, load_nif, 2},
    {erlang, open_port, 2},
    {erlang, list_to_atom, 1},
    {erlang, list_to_existing_atom, 1},
    {erlang, binary_to_atom, 1},
    {erlang, binary_to_atom, 2},
    {erlang, binary_to_existing_atom, 1},
    {erlang, binary_to_existing_atom, 2},
    {erlang, binary_to_term, 1},
    {erlang, binary_to_term, 2},
    {code, ensure_loaded, 1}
]).
-define(SPAWNS, [spawn, spawn_link, spawn_monitor, spawn_opt, spawn_request]).
%% Calls the analysis models natively: their result is computed from their
%% arguments, not from the body of the function.
-define(MODELLED, [
    {lists, keyfind, 3},
    {lists, keydelete, 3},
    {lists, keyreplace, 4},
    {lists, keystore, 4},
    {lists, reverse, 1},
    {lists, reverse, 2},
    {atomvm, get_start_beam, 1},
    {atomvm, get_boot, 0},
    {binary, part, 3}
]).
%% Register sets are bitmasks: x registers in the low bits, y registers above.
-define(XBITS, 1024).
-define(XMASK, ((1 bsl ?XBITS) - 1)).
%% A frame has at most as many y registers as the loader accepts.
-define(YMASK, ((1 bsl 1024) - 1)).
%% Functions that send a message, with the message's position: options can
%% follow it.
-define(SENDS, [
    {send, 2, 2},
    {send, 3, 2},
    {send_after, 3, 3},
    {send_after, 4, 3},
    {start_timer, 3, 3},
    {start_timer, 4, 3}
]).

%% Modules: module => indexed module (packbeam_prune:index_module/1).
%% Returns the relevant parameter indexes (0-based) of each function and the
%% functions whose return value is relevant.
-spec compute(#{module() => map()}) -> #{params := map(), returns := map()}.
compute(Modules) ->
    compute(Modules, #{}).

%% Options:
%% - seeds: the functions whose dynamic calls and atom creations are sinks
%%   (all by default). Pruning refines only where a previous analysis could not
%%   resolve a call target.
%% - edges: a call graph a previous analysis found. A dynamic call then passes
%%   the relevance of the targets it reached there, rather than of every
%%   function of its arity.
-spec compute(#{module() => map()}, map()) -> #{params := map(), returns := map()}.
compute(Modules, Options) ->
    Fns = [
        {{M, F, A}, Code}
     || {M, D} <- maps:to_list(Modules), {{F, A}, Code} <- maps:to_list(maps:get(function_code, D))
    ],
    %% A dynamic call of arity A reaches exported functions of arity A and
    %% lambdas taking A arguments before their captured values.
    DynTargets = maps:groups_from_list(
        fun({A, _}) -> A end,
        fun({_, MFA}) -> MFA end,
        [
            {A, {M, F, A}}
         || {M, D} <- maps:to_list(Modules), {F, A} <- maps:get(exports, D)
        ] ++
            [
                {Ar - Free, {M, maps:get(Atom, maps:get(atoms, D)), Ar}}
             || {M, D} <- maps:to_list(Modules),
                [Atom, Ar, _, _, Free, _] <- maps:values(maps:get(funs, D))
            ]
    ),
    DynArities = maps:groups_from_list(
        fun({_, MFA}) -> MFA end,
        fun({A, _}) -> A end,
        [{A, MFA} || {A, MFAs} <- maps:to_list(DynTargets), MFA <- MFAs]
    ),
    Env = #{
        modules => Modules,
        dyn_targets => DynTargets,
        dyn_arities => DynArities,
        seeds => maps:get(seeds, Options, all),
        edges => maps:get(edges, Options, undefined)
    },
    Code = maps:from_list([{MFA, layout(C)} || {MFA, C} <- Fns]),
    Deps = dependencies(Code, Env),
    G0 = #{params => #{}, returns => #{}, dyn_returns => #{}, dyn_params => #{}, received => false},
    G = iterate(maps:keys(Code), Code, Env, Deps, G0),
    #{params => maps:get(params, G), returns => returns(G, Env)}.

returns(G, #{dyn_targets := DynTargets}) ->
    maps:merge(
        maps:get(returns, G),
        maps:from_list([
            {MFA, true}
         || A <- maps:keys(maps:get(dyn_returns, G)), MFA <- maps:get(A, DynTargets, [])
        ])
    ).

layout({Is, Labels}) ->
    N = tuple_size(Is),
    Normal = list_to_tuple([successors(I, Is, Labels) || I <- lists:seq(1, N)]),
    Succ = handlers(Is, Labels, Normal),
    Pred = maps:groups_from_list(
        fun({S, _}) -> S end,
        fun({_, P}) -> P end,
        [{S, I} || I <- lists:seq(1, N), S <- element(I, Succ)]
    ),
    #{is => Is, succ => Succ, pred => Pred, entry => entry_index(Is)}.

%% Any instruction protected by a try or a catch can raise into its handler,
%% which reads the y registers as they were at that point.
handlers(Is, Labels, Normal) ->
    Protected = lists:foldl(
        fun(I, Acc) ->
            case element(I, Is) of
                {_, Op, [Y, {?COMPACT_LABEL, L}]} when Op =:= 'try'; Op =:= 'catch' ->
                    H = maps:get(L, Labels),
                    Region = region(element(I, Normal) -- [H], Is, Normal, regs(Y), H, #{}),
                    lists:foldl(
                        fun(J, A) -> maps:update_with(J, fun(Hs) -> [H | Hs] end, [H], A) end,
                        Acc,
                        maps:keys(Region)
                    );
                _ ->
                    Acc
            end
        end,
        #{},
        lists:seq(1, tuple_size(Is))
    ),
    list_to_tuple([
        lists:usort(maps:get(I, Protected, []) ++ element(I, Normal))
     || I <- lists:seq(1, tuple_size(Is))
    ]).
region([], _Is, _Normal, _Y, _H, Seen) ->
    Seen;
region([I | Work], Is, Normal, Y, H, Seen) ->
    case maps:is_key(I, Seen) orelse I =:= H of
        true ->
            region(Work, Is, Normal, Y, H, Seen);
        false ->
            case element(I, Is) of
                {_, End, [Reg]} when End =:= try_end; End =:= catch_end ->
                    case regs(Reg) of
                        Y -> region(Work, Is, Normal, Y, H, Seen);
                        _ -> region(element(I, Normal) ++ Work, Is, Normal, Y, H, Seen#{I => true})
                    end;
                _ ->
                    region(element(I, Normal) ++ Work, Is, Normal, Y, H, Seen#{I => true})
            end
    end.

%% Who reads what a function's result publishes: its callers read its
%% parameters, dynamic calls of an arity read the parameters of every target
%% of that arity, and senders read whether received messages matter.
dependencies(Code, Env) ->
    Uses = [
        {MFA, Use}
     || {{M, _, _} = MFA, #{is := Is}} <- maps:to_list(Code),
        D <- [maps:get(M, maps:get(modules, Env))],
        {_, Op, As} <- tuple_to_list(Is),
        Use <- uses(Op, As, D)
    ],
    Dynamic =
        case maps:get(edges, Env) of
            undefined -> [];
            Edges -> [{MFA, {params, T}} || {MFA, Ts} <- maps:to_list(Edges), T <- Ts]
        end,
    maps:groups_from_list(fun({_, Use}) -> Use end, fun({MFA, _}) -> MFA end, Uses ++ Dynamic).

uses(Op, [{?COMPACT_LITERAL, _}, {?COMPACT_LABEL, L} | _], D) when
    Op =:= call; Op =:= call_only; Op =:= call_last
->
    {F, Ar} = maps:get(L, maps:get(label_owner, D)),
    [{params, {maps:get(module, D), F, Ar}}];
uses(Op, [{?COMPACT_LITERAL, _}, {?COMPACT_LITERAL, Idx} | _], D) when
    Op =:= call_ext; Op =:= call_ext_only; Op =:= call_ext_last
->
    [{params, maps:get(Idx, maps:get(imports, D))}, received];
uses(Op, [{?COMPACT_LITERAL, A} | _], _D) when Op =:= apply; Op =:= apply_last; Op =:= call_fun ->
    [{dyn, A}];
uses(call_fun2, [_, {?COMPACT_LITERAL, A}, _], _D) ->
    [{dyn, A}];
uses(Op, [{?COMPACT_LITERAL, Idx} | _], D) when Op =:= make_fun2; Op =:= make_fun3 ->
    [Atom, Ar, _, _, _, _] = maps:get(Idx, maps:get(funs, D)),
    [{params, {maps:get(module, D), maps:get(Atom, maps:get(atoms, D)), Ar}}];
uses(send, _, _) ->
    [received];
uses(_, _, _) ->
    [].

iterate(Work, Code, Env, Deps, G) ->
    iterate(Work, maps:from_list([{W, true} || W <- Work]), Code, Env, Deps, G, #{}).
iterate([], _Queued, _Code, _Env, _Deps, G, _Results) ->
    G;
iterate([MFA | Work], Queued, Code, Env, Deps, G, Results) ->
    Q = maps:remove(MFA, Queued),
    R = function(MFA, maps:get(MFA, Code), Env, G),
    case maps:get(MFA, Results, none) of
        R ->
            iterate(Work, Q, Code, Env, Deps, G, Results);
        _ ->
            {G1, Moved} = apply_result(MFA, R, G, Env, Deps),
            New = [M || M <- lists:usort(Moved), not maps:is_key(M, Q)],
            Q1 = lists:foldl(fun(M, Acc) -> Acc#{M => true} end, Q, New),
            iterate(Work ++ New, Q1, Code, Env, Deps, G1, Results#{MFA => R})
    end.

apply_result(MFA, {Params, CalleeReturns, DynReturns, Received}, G, Env, Deps) ->
    #{params := Ps, returns := Rs, dyn_returns := Ds, dyn_params := DPs, received := Rec} = G,
    #{dyn_targets := DynTargets, dyn_arities := DynArities} = Env,
    Arities = maps:get(MFA, DynArities, []),
    ParamReaders =
        case maps:get(MFA, Ps, []) of
            Params -> [];
            _ -> maps:get({params, MFA}, Deps, [])
        end,
    DynParams = lists:foldl(
        fun(A, Acc) ->
            Mask = lists:foldl(fun(I, M) -> M bor (1 bsl I) end, 0, [I || I <- Params, I < A]),
            Acc#{A => maps:get(A, Acc, 0) bor Mask}
        end,
        DPs,
        Arities
    ),
    DynReaders = lists:append([
        maps:get({dyn, A}, Deps, [])
     || A <- Arities, maps:get(A, DynParams) =/= maps:get(A, DPs, 0)
    ]),
    NewReturns = [C || C <- CalleeReturns, not maps:is_key(C, Rs)],
    NewDyn = [A || A <- DynReturns, not maps:is_key(A, Ds)],
    Readers =
        case Received andalso not Rec of
            true -> maps:get(received, Deps, []);
            false -> []
        end,
    {
        G#{
            params => Ps#{MFA => Params},
            dyn_params => DynParams,
            returns => lists:foldl(fun(C, Acc) -> Acc#{C => true} end, Rs, NewReturns),
            dyn_returns => lists:foldl(fun(A, Acc) -> Acc#{A => true} end, Ds, NewDyn),
            received => Rec orelse Received
        },
        ParamReaders ++ DynReaders ++ NewReturns ++
            lists:append([maps:get(A, DynTargets, []) || A <- NewDyn]) ++ Readers
    }.

function({M, _, _} = MFA, #{is := Is, succ := Succ, pred := Pred, entry := Entry}, Env, G) ->
    D = maps:get(M, maps:get(modules, Env)),
    N = tuple_size(Is),
    Ctx = #{
        mfa => MFA,
        module => D,
        env => Env,
        g => G,
        returns => is_return(MFA, G, Env),
        seeded =>
            case maps:get(seeds, Env) of
                all -> true;
                Seeds -> maps:is_key(MFA, Seeds)
            end
    },
    In = solve(lists:seq(N, 1, -1), Is, Succ, Pred, Ctx, #{}),
    Params = bit_indexes(maps:get(Entry, In, 0) band ?XMASK, 0),
    {CalleeReturns, DynReturns, Received} = lists:foldl(
        fun(I, Acc) ->
            {_, Op, As} = element(I, Is),
            effects(Op, As, out(I, Succ, In), Ctx, Acc)
        end,
        {[], [], false},
        lists:seq(1, N)
    ),
    {Params, lists:usort(CalleeReturns), lists:usort(DynReturns), Received}.

solve(Work, Is, Succ, Pred, Ctx, In) ->
    solve(Work, maps:from_list([{W, true} || W <- Work]), Is, Succ, Pred, Ctx, In).
solve([], _Queued, _Is, _Succ, _Pred, _Ctx, In) ->
    In;
solve([I | Work], Queued, Is, Succ, Pred, Ctx, In) ->
    Q = maps:remove(I, Queued),
    {_, Op, As} = element(I, Is),
    New = transfer(Op, As, out(I, Succ, In), Ctx),
    case maps:get(I, In, 0) of
        New ->
            solve(Work, Q, Is, Succ, Pred, Ctx, In);
        _ ->
            Ps = [P || P <- maps:get(I, Pred, []), not maps:is_key(P, Q)],
            Q1 = lists:foldl(fun(P, Acc) -> Acc#{P => true} end, Q, Ps),
            solve(Ps ++ Work, Q1, Is, Succ, Pred, Ctx, In#{I => New})
    end.

out(I, Succ, In) ->
    lists:foldl(fun(S, Acc) -> maps:get(S, In, 0) bor Acc end, 0, element(I, Succ)).

successors(I, Is, Labels) ->
    {_, Op, As} = element(I, Is),
    %% A local call names its callee's label: that is not a jump.
    Targets =
        case lists:member(Op, [call, call_only, call_last]) of
            true -> [];
            false -> [maps:get(L, Labels) || L <- labels(As), L =/= 0, maps:is_key(L, Labels)]
        end,
    Fall =
        case
            lists:member(Op, [
                return,
                call_only,
                call_last,
                call_ext_only,
                call_ext_last,
                apply_last,
                jump,
                func_info,
                int_code_end,
                badmatch,
                case_end,
                if_end,
                try_case_end,
                select_val,
                select_tuple_arity,
                wait,
                loop_rec_end
            ]) orelse I >= tuple_size(Is)
        of
            true -> [];
            false -> [I + 1]
        end,
    lists:usort(Fall ++ Targets).

labels(L) when is_list(L) -> lists:append([labels(X) || X <- L]);
labels({?COMPACT_LABEL, N}) -> [N];
labels({list, L}) -> labels(L);
labels(_) -> [].

entry_index(Is) ->
    hd([I + 1 || I <- lists:seq(1, tuple_size(Is)), element(2, element(I, Is)) =:= func_info]).

transfer(move, [Src, Dst], Out, _) ->
    def_use([Dst], [Src], Out);
transfer(swap, [A, B], Out, _) ->
    RA = regs(A),
    RB = regs(B),
    Rest = Out band bnot (RA bor RB),
    Rest bor if_set(Out band RA, RB) bor if_set(Out band RB, RA);
transfer(get_tuple_element, [Src, _, Dst], Out, _) ->
    def_use([Dst], [Src], Out);
transfer(get_list, [Src, H, T], Out, _) ->
    def_use([H, T], [Src], Out);
transfer(Op, [Src, Dst], Out, _) when Op =:= get_hd; Op =:= get_tl ->
    def_use([Dst], [Src], Out);
transfer(put_tuple2, [Dst, List], Out, _) ->
    def_use([Dst], [List], Out);
transfer(put_list, [H, T, Dst], Out, _) ->
    def_use([Dst], [H, T], Out);
transfer(update_record, [_Hint, _Size, Src, Dst, List], Out, _) ->
    def_use([Dst], [Src, List], Out);
transfer(set_tuple_element, [Value, Tuple, _], Out, _) ->
    Out bor if_set(Out band regs(Tuple), regs(Value));
transfer(Op, [_Fail, Src, Dst, _Live, List], Out, _) when
    Op =:= put_map_assoc; Op =:= put_map_exact
->
    def_use([Dst], [Src, List], Out);
transfer(get_map_elements, [_Fail, Src, {list, Pairs}], Out, _) ->
    {Keys, Dsts} = pairs(Pairs),
    def_use(Dsts, [Src | Keys], Out);
transfer(init_yregs, [{list, Regs}], Out, _) ->
    kill(Regs, Out);
transfer(trim, [{?COMPACT_LITERAL, N}, _], Out, _) ->
    (Out band ?XMASK) bor ((((Out bsr ?XBITS) bsl N) band ?YMASK) bsl ?XBITS);
transfer(return, [], Out, #{returns := Returns}) ->
    case Returns of
        true -> Out bor 1;
        false -> Out
    end;
transfer(Op, [{?COMPACT_LITERAL, A}, {?COMPACT_LABEL, L} | _], Out, #{module := D} = Ctx) when
    Op =:= call; Op =:= call_only; Op =:= call_last
->
    {F, Ar} = maps:get(L, maps:get(label_owner, D)),
    call({maps:get(module, D), F, Ar}, A, Out, Ctx);
transfer(Op, [{?COMPACT_LITERAL, A}, {?COMPACT_LITERAL, Idx} | _], Out, #{module := D} = Ctx) when
    Op =:= call_ext; Op =:= call_ext_only; Op =:= call_ext_last
->
    Target = maps:get(Idx, maps:get(imports, D)),
    case classify(Target, Ctx) of
        code ->
            call(Target, A, Out, Ctx);
        native ->
            Used =
                case sink(Target, Ctx) orelse result_needed(Op, Out, Ctx) of
                    true -> xregs(A);
                    false -> message_arg(Target, Ctx)
                end,
            yregs(Out) bor Used
    end;
transfer(Op, [{?COMPACT_LITERAL, A} | _], Out, Ctx) when Op =:= apply; Op =:= apply_last ->
    yregs(Out) bor sink_bits(3 bsl A, Ctx) bor dyn_params(A, Ctx);
transfer(call_fun, [{?COMPACT_LITERAL, A}], Out, Ctx) ->
    yregs(Out) bor sink_bits(1 bsl A, Ctx) bor dyn_params(A, Ctx);
transfer(call_fun2, [_Tag, {?COMPACT_LITERAL, A}, Fun], Out, Ctx) ->
    yregs(Out) bor sink_bits(regs(Fun), Ctx) bor dyn_params(A, Ctx);
transfer(Op, [{?COMPACT_LITERAL, Idx} | Tail], Out, #{module := D, g := G}) when
    Op =:= make_fun2; Op =:= make_fun3
->
    [Atom, Ar, _, _, Free, _] = maps:get(Idx, maps:get(funs, D)),
    Lambda = {maps:get(module, D), maps:get(Atom, maps:get(atoms, D)), Ar},
    {Dst, Caps} =
        case Tail of
            [Dest, {list, Cs}] -> {Dest, Cs};
            _ -> {{?COMPACT_XREG, 0}, [{?COMPACT_XREG, J} || J <- lists:seq(0, Free - 1)]}
        end,
    Params = maps:get(Lambda, maps:get(params, G), []),
    Used = [
        C
     || {J, C} <- lists:zip(lists:seq(0, length(Caps) - 1), Caps),
        lists:member(Ar - Free + J, Params)
    ],
    kill([Dst], Out) bor regs(Used);
transfer(send, [], Out, #{g := G}) ->
    Msg =
        case maps:get(received, G) orelse Out band 1 =/= 0 of
            true -> 2;
            false -> 0
        end,
    yregs(Out) bor Msg;
transfer(loop_rec, [_Fail, Dst], Out, _) ->
    kill([Dst], Out);
transfer(Op, As, Out, #{module := D} = Ctx) when
    Op =:= bif0; Op =:= bif1; Op =:= bif2; Op =:= gc_bif1; Op =:= gc_bif2; Op =:= gc_bif3
->
    {Idx, Args, Dst} = bif_args(Op, As),
    Target = maps:get(Idx, maps:get(imports, D)),
    case sink(Target, Ctx) orelse Out band regs(Dst) =/= 0 of
        true -> kill([Dst], Out) bor regs(Args);
        false -> kill([Dst], Out)
    end;
transfer(Op, As, Out, _) ->
    Name = atom_to_list(Op),
    case
        lists:prefix("is_", Name) orelse
            lists:member(Op, [test_arity, select_val, select_tuple_arity])
    of
        %% A test only chooses the path.
        true ->
            Out;
        %% Anything else may pass any operand into any other.
        false ->
            Regs = regs(As),
            Out bor if_set(Out band Regs, Regs)
    end.

effects(
    Op, [{?COMPACT_LITERAL, _A}, {?COMPACT_LABEL, L} | _], Out, #{module := D} = Ctx, {Cs, Ds, Rec}
) when
    Op =:= call; Op =:= call_only; Op =:= call_last
->
    {F, Ar} = maps:get(L, maps:get(label_owner, D)),
    case result_needed(Op, Out, Ctx) of
        true -> {[{maps:get(module, D), F, Ar} | Cs], Ds, Rec};
        false -> {Cs, Ds, Rec}
    end;
effects(
    Op,
    [{?COMPACT_LITERAL, _A}, {?COMPACT_LITERAL, Idx} | _],
    Out,
    #{module := D} = Ctx,
    {Cs, Ds, Rec}
) when
    Op =:= call_ext; Op =:= call_ext_only; Op =:= call_ext_last
->
    Target = maps:get(Idx, maps:get(imports, D)),
    case classify(Target, Ctx) =:= code andalso result_needed(Op, Out, Ctx) of
        true -> {[Target | Cs], Ds, Rec};
        false -> {Cs, Ds, Rec}
    end;
effects(Op, [{?COMPACT_LITERAL, A} | _], Out, Ctx, Acc) when
    Op =:= apply; Op =:= apply_last; Op =:= call_fun
->
    dyn_effects(Op, A, Out, Ctx, Acc);
effects(call_fun2, [_Tag, {?COMPACT_LITERAL, A}, _], Out, Ctx, Acc) ->
    dyn_effects(call_fun2, A, Out, Ctx, Acc);
effects(loop_rec, [_Fail, Dst], Out, _Ctx, {Cs, Ds, Rec}) ->
    {Cs, Ds, Rec orelse Out band regs(Dst) =/= 0};
effects(_, _, _, _, Acc) ->
    Acc.

dyn_effects(Op, A, Out, Ctx, {Cs, Ds, Rec}) ->
    case {result_needed(Op, Out, Ctx), maps:get(edges, maps:get(env, Ctx))} of
        {false, _} -> {Cs, Ds, Rec};
        {true, undefined} -> {Cs, [A | Ds], Rec};
        {true, _} -> {dyn_callees(A, Ctx) ++ Cs, Ds, Rec}
    end.

%% A local or Erlang-coded callee: its relevant parameters are read, the rest
%% of the x registers are clobbered by the call.
call(Target, _A, Out, #{g := G}) ->
    Params = maps:get(Target, maps:get(params, G), []),
    lists:foldl(fun(I, Acc) -> Acc bor (1 bsl I) end, yregs(Out), Params).

result_needed(Op, Out, #{returns := Returns}) ->
    case lists:member(Op, [call_only, call_last, call_ext_only, call_ext_last, apply_last]) of
        true -> Returns;
        false -> Out band 1 =/= 0
    end.

is_return(MFA, G, #{dyn_targets := DynTargets}) ->
    maps:is_key(MFA, maps:get(returns, G)) orelse
        lists:any(
            fun(A) -> lists:member(MFA, maps:get(A, DynTargets, [])) end,
            maps:keys(maps:get(dyn_returns, G))
        ).

classify({erlang, _, _}, _) ->
    native;
classify({M, F, A} = MFA, #{env := #{modules := Modules}}) ->
    case lists:member(MFA, ?MODELLED) orelse lists:member(MFA, ?SINKS) of
        true ->
            native;
        false ->
            case maps:find(M, Modules) of
                {ok, D} ->
                    case
                        maps:is_key({F, A}, maps:get(function_code, D)) andalso
                            not lists:member({F, A}, maps:get(nif_stubs, D))
                    of
                        true -> code;
                        false -> native
                    end;
                error ->
                    native
            end
    end.

sink(_, #{seeded := false}) -> false;
sink({erlang, F, _} = MFA, _) -> lists:member(MFA, ?SINKS) orelse lists:member(F, ?SPAWNS);
sink(MFA, _) -> lists:member(MFA, ?SINKS).

sink_bits(Bits, #{seeded := true}) -> Bits;
sink_bits(_, _) -> 0.

%% A call the previous analysis did not resolve can reach any function of its
%% arity.
dyn_callees(A, #{seeded := true, env := #{dyn_targets := DynTargets}}) ->
    maps:get(A, DynTargets, []);
dyn_callees(A, #{mfa := MFA, env := #{edges := Edges, dyn_arities := DynArities}}) ->
    [T || T <- maps:get(MFA, Edges, []), lists:member(A, maps:get(T, DynArities, []))].

message_arg({erlang, F, A}, #{g := G}) ->
    case maps:get(received, G) andalso [P || {F1, A1, P} <- ?SENDS, F1 =:= F, A1 =:= A] of
        [P] -> 1 bsl (P - 1);
        _ -> 0
    end;
message_arg(_, _) ->
    0.

dyn_params(A, #{env := #{edges := undefined}, g := G}) ->
    maps:get(A, maps:get(dyn_params, G), 0);
dyn_params(A, #{g := G} = Ctx) ->
    Ps = maps:get(params, G),
    lists:foldl(
        fun(I, Acc) -> Acc bor (1 bsl I) end,
        0,
        [I || T <- dyn_callees(A, Ctx), I <- maps:get(T, Ps, []), I < A]
    ).

def_use(Defs, Uses, Out) ->
    Killed = kill(Defs, Out),
    Killed bor if_set(Out band regs(Defs), regs(Uses)).

kill(Defs, Out) ->
    Out band bnot regs(Defs).

yregs(Out) -> Out band bnot ?XMASK.
xregs(A) -> (1 bsl A) - 1.

if_set(0, _) -> 0;
if_set(_, Bits) -> Bits.

bit_indexes(0, _) -> [];
bit_indexes(Bits, I) when Bits band 1 =:= 1 -> [I | bit_indexes(Bits bsr 1, I + 1)];
bit_indexes(Bits, I) -> bit_indexes(Bits bsr 1, I + 1).

pairs([K, V | T]) ->
    {Ks, Vs} = pairs(T),
    {[K | Ks], [V | Vs]};
pairs(_) ->
    {[], []}.

bif_args(bif0, [{?COMPACT_LITERAL, I}, Dst]) ->
    {I, [], Dst};
bif_args(Op, [_Fail, _Live, {?COMPACT_LITERAL, I} | Tail]) when
    Op =:= gc_bif1; Op =:= gc_bif2; Op =:= gc_bif3
->
    {I, lists:droplast(Tail), lists:last(Tail)};
bif_args(_, [_Fail, {?COMPACT_LITERAL, I} | Tail]) ->
    {I, lists:droplast(Tail), lists:last(Tail)}.

regs(L) when is_list(L) -> lists:foldl(fun(X, Acc) -> regs(X) bor Acc end, 0, L);
regs({typed, R, _}) -> regs(R);
regs({?COMPACT_XREG, N}) -> 1 bsl N;
regs({?COMPACT_YREG, N}) -> 1 bsl (?XBITS + N);
regs({list, L}) -> regs(L);
regs(T) when is_tuple(T) -> regs(tuple_to_list(T));
regs(_) -> 0.
