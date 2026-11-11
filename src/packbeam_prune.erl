%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(packbeam_prune).
-include("compact_term.hrl").
-export([run/4, prune/3, format_warning/1, driver_suggestions/1]).
-ifdef(TEST).
-export([join/2, index_module/1]).
-endif.
%% Elements kept for a constructed proper list before it becomes an
%% element-only summary.
-define(SPINE_LIMIT, 16).
%% Distinct payloads kept per control-message tag before they are joined.
-define(MESSAGE_ALTERNATIVES, 64).
%% Alternatives kept for one value before it is widened. A server dispatching
%% on a request tag needs more than a handful: the requests reachable code
%% sends across a whole library run past a dozen.
-define(CHOICE_LIMIT, 32).
%% Argument vectors kept per function before contexts are merged.
-define(CONTEXT_LIMIT, 64).
%% Nodes kept in a constructed value before it keeps fewer levels.
-define(VALUE_SIZE, 1024).
%% Every argument and return value analyzed as unknown.
-define(COARSE, #{params => #{}, returns => #{}}).
%% Rounds that raise context budgets after the context-insensitive analysis.
-define(REFINE_ROUNDS, 4).
%% Tests that only read their operands and branch.
-define(PURE_TESTS, [
    is_integer,
    is_float,
    is_number,
    is_atom,
    is_pid,
    is_reference,
    is_port,
    is_nil,
    is_binary,
    is_bitstr,
    is_list,
    is_nonempty_list,
    is_tuple,
    is_map,
    is_function,
    is_boolean,
    test_arity,
    is_tagged_tuple,
    is_eq_exact,
    is_ne_exact,
    is_eq,
    is_ne,
    is_lt,
    is_ge
]).

%% Compatibility with the original reachability prototype.
-spec prune([file:filename()], mfa(), file:filename()) -> [mfa()].
prune(Paths, Entry, _OutputDir) ->
    Bs = [
        begin
            {ok, B} = file:read_file(P),
            B
        end
     || P <- Paths
    ],
    {_, R} = run(Bs, [], [Entry], #{}),
    case [M || #{reason := {missing_module, M}} <- maps:get(warnings, R)] of
        [M | _] -> error({undef, M});
        [] -> maps:get(reachable, R)
    end.

%% @doc Rewrite output BEAMs using a closed-world reachability analysis.
%% Reference BEAMs participate in analysis but are never returned or modified.
%% Unknown targets widen retention and are reported in the warnings map.
-spec run([binary()], [binary()], [mfa()], map()) -> {[binary()], map()}.
run(Binaries, References, Roots, Options) ->
    Modules = lists:foldl(
        fun(B, Acc) ->
            D = index_module(packbeam_beam:read(B)),
            M = maps:get(module, D),
            case maps:is_key(M, Acc) of
                true -> error({duplicate_module, M});
                false -> Acc#{M => D}
            end
        end,
        #{},
        Binaries ++ References
    ),
    AllRoots = Roots ++ maps:get(keep, Options, []),
    case AllRoots of
        [] -> error(no_pruning_roots);
        _ -> ok
    end,
    lists:foreach(fun(MFA) -> validate_root(MFA, Modules) end, AllRoots),
    S0 = #{
        modules => Modules,
        relevance =>
            case maps:get(precision, Options, full) of
                coarse -> ?COARSE;
                _ -> undefined
            end,
        %% Indexes built once: the analysis asks these questions at every
        %% dynamic call site, on every round.
        exported_by => exported_by(Modules),
        nifs_index => maps:from_keys(packbeam_drivers:nifs(), true),
        args => #{},
        widened => #{},
        groups => #{},
        returns => #{},
        warnings => [],
        %% Call sites whose target the values did not resolve, warned or not:
        %% adaptive precision gives contexts to what reaches them.
        unresolved => #{},
        edges => #{},
        %% The reverse of `edges', and what the current round changed.
        callers => #{},
        fresh => [],
        returned => #{},
        rewidened => #{},
        dispatches => #{},
        literals => #{},
        literal_edges => #{},
        surfaced => #{},
        closures => #{},
        messages => #{},
        opaque_messages => false,
        forwarded_messages => false,
        %% Protocol-tagged tuples reachable code builds, by tag and arity, and
        %% the arities for which a tuple may carry a tag the analysis did not
        %% see built.
        protocol_tuples => #{},
        open_protocol => [],
        escaped_tags => #{},
        unknown_tags => [],
        unknown_send => false,
        opaque_arities => [],
        opaque_senders => #{},
        native_ports => [],
        all => false,
        pending_package => false,
        bounded_fallback => false,
        all_named => false,
        discovery_budget => maps:get(discovery_budget, Options, 100000),
        discovery_limited => false,
        drivers => [],
        nifs => [],
        code => #{},
        %% Modules whose existence the program observes: each keeps
        %% `module_info/0', as compiled, so it is still loaded.
        present => #{},
        recording => false,
        boot_data => maps:get(boot_data, Options, unknown),
        start_module => maps:get(start_module, Options, undefined),
        context_limit => maps:get(context_limit, Options, ?CONTEXT_LIMIT),
        context_budgets => #{},
        jit_types => maps:get(jit_types, Options, false),
        open_world => maps:get(open_world, Options, false),
        unknown_driver => false
    },
    Seed = fun(State) ->
        fixpoint(
            lists:foldl(
                fun({_, _, A} = MFA, Acc) ->
                    case {MFA, maps:get(start_module, Options, undefined)} of
                        {{init, boot, 1}, M} when M =/= undefined ->
                            %% Run from a file, or appended to the executable
                            %% as an escript: `["-s", escript, "--", Path | Args]'.
                            Escript = lists:foldr(
                                fun(V, T) -> {cons, V, T} end,
                                {cons, unknown, {sequence, unknown}},
                                [{const, <<"-s">>}, {const, escript}, {const, <<"--">>}]
                            ),
                            %% The start module is named here, not by any
                            %% literal the analyzed code holds.
                            Named = surface_atoms([M], none, Acc),
                            reach(MFA, [Escript], reach(MFA, [{const, [<<"-s">>, M]}], Named));
                        _ ->
                            reach(MFA, lists:duplicate(A, unknown), Acc)
                    end
                end,
                State,
                AllRoots
            )
        )
    end,
    {Base, First} =
        case maps:get(precision, Options, full) of
            adaptive ->
                Insensitive = S0#{context_limit => 1},
                {Budgets, Last} = adapt(Seed, Insensitive, Modules, #{}, #{}, Seed(Insensitive), 0),
                {Insensitive#{context_budgets => Budgets}, Last};
            insensitive ->
                Insensitive = S0#{context_limit => 1},
                {Insensitive, Seed(Insensitive)};
            Precision when Precision =:= full; Precision =:= coarse ->
                {S0, Seed(S0)}
        end,
    %% Contexts accumulate, so a value that was still being computed in an
    %% early round leaves an over-wide argument context behind for good. Run
    %% the analysis again from the roots, seeded with the summaries learned in
    %% the first pass, and keep whichever pass discovered less.
    Settled = retain(Seed(Base#{returns => maps:get(returns, First)}), #{}),
    S = record_states(Settled),
    Named = named_closure(S),
    Whole = unanalyzed_closure(retained(Named, S), maps:get(args, S), Modules),
    Reach = lists:usort(
        case maps:get(all, S) of
            true ->
                [{M, F, A} || {M, D} <- maps:to_list(Modules), {F, A, _} <- maps:get(functions, D)];
            false ->
                maps:keys(maps:get(args, S)) ++
                    [{M, module_info, 0} || M <- maps:keys(maps:get(present, S))] ++
                    Whole
        end
    ),
    Parents = shortest_parents(AllRoots, maps:get(edges, S)),
    Out =
        case maps:get(all, S) of
            true ->
                Binaries;
            false ->
                [
                    packbeam_beam:trim(D, [{F, A} || {Mod, F, A} <- Reach, Mod =:= M])
                 || B <- Binaries,
                    M <- [beam_module(B)],
                    D <- [residual_module(maps:get(M, Modules), S)],
                    lists:any(fun({Mod, _, _}) -> Mod =:= M end, Reach)
                ]
        end,
    {Out, #{
        reachable => Reach,
        graph => #{
            %% Complete means every dispatch was resolved to known targets:
            %% neither the whole package nor a name/arity bound was needed.
            complete => not (maps:get(all, S) orelse maps:get(bounded_fallback, S)),
            discovery_limited => maps:get(discovery_limited, S),
            calls => edge_lists(maps:get(edges, S)),
            dispatches => maps:get(dispatches, S),
            arguments => maps:get(args, S),
            relevance => maps:get(relevance, S),
            literals => maps:get(literals, S),
            literal_uses => maps:get(literal_edges, S),
            messages => maps:get(messages, S),
            opaque_senders => maps:get(opaque_senders, S),
            widened_contexts => maps:from_list([
                {MFA, length(maps:get(MFA, maps:get(args, S), []))}
             || MFA <- maps:keys(maps:get(widened, S))
            ])
        },
        warnings => [
            warning_context(W, Parents, References, S)
         || W <- maps:get(warnings, S)
        ],
        drivers => lists:usort(maps:get(drivers, S)),
        nifs => lists:usort(maps:get(nifs, S)),
        driver_analysis_complete => not maps:get(unknown_driver, S) andalso not maps:get(all, S)
    }}.

%% Context sensitivity where it can remove code. A first analysis keeps one
%% context per function: values stay precise, but the calls of a function
%% share one argument vector, so a generic server loop mixes the servers it
%% runs. Each round finds the calls whose target the values did not resolve,
%% walks the code backwards from them to the functions whose parameters reach
%% them, and gives those functions the full context budget.
adapt(Seed, Base, Modules, Budgets, Seeds, Prev, Round) ->
    %% A call that resolved to several targets may reach only one of them
    %% from each context: merged contexts can make it imprecise too.
    Imprecise = [
        Caller
     || {{Caller, _}, #{targets := [_, _ | _]}} <- maps:to_list(maps:get(dispatches, Prev))
    ],
    Candidates = maps:merge(maps:get(unresolved, Prev), maps:from_keys(Imprecise, true)),
    New = maps:merge(
        Seeds, maps:filter(fun(C, _) -> can_prune(C, Candidates, Prev) end, Candidates)
    ),
    case maps:size(New) =:= maps:size(Seeds) orelse Round >= ?REFINE_ROUNDS of
        %% The last round ran with these budgets: it is the first pass.
        true ->
            {Budgets, Prev};
        false ->
            #{params := Params} = packbeam_relevance:compute(Modules, #{
                seeds => New, edges => edge_lists(maps:get(edges, Prev))
            }),
            Next = maps:merge(
                Budgets,
                maps:from_list([{MFA, ?CONTEXT_LIMIT} || {MFA, [_ | _]} <- maps:to_list(Params)])
            ),
            adapt(
                Seed,
                Base,
                Modules,
                Next,
                New,
                Seed(Base#{context_budgets => Next}),
                Round + 1
            )
    end.
%% Refining a call can only remove what nothing else reaches: a callee that
%% another, resolved call site also reaches stays either way.
can_prune(Caller, Candidates, S) ->
    Callers = maps:get(callers, S),
    lists:any(
        fun(Target) ->
            lists:all(
                fun(Other) -> maps:is_key(Other, Candidates) end,
                maps:keys(maps:get(Target, Callers, #{}))
            )
        end,
        maps:keys(maps:get(Caller, maps:get(edges, S), #{}))
    ).
context_limit(MFA, S) ->
    maps:get(MFA, maps:get(context_budgets, S), maps:get(context_limit, S)).

%% The questions every call asks of a module -- which functions it defines,
%% which function a label belongs to, what runs on load -- answered once.
-spec index_module(packbeam_beam:beam_module()) -> packbeam_beam:beam_module().
index_module(D) ->
    Fs = maps:get(functions, D),
    D#{
        function_code => maps:from_list([{{F, A}, layout(Ops)} || {F, A, Ops} <- Fs]),
        label_owner => maps:from_list([
            {L, {F, A}}
         || {F, A, Ops} <- Fs, {_, label, [{?COMPACT_LITERAL, L}]} <- Ops
        ]),
        on_load => [
            {F, A}
         || {F, A, Ops} <- Fs, lists:any(fun({_, Op, _}) -> Op =:= on_load end, Ops)
        ]
    }.

layout(Ops) ->
    {
        list_to_tuple(Ops),
        maps:from_list([
            {N, I}
         || {I, {_, label, [{?COMPACT_LITERAL, N}]}} <- lists:zip(lists:seq(1, length(Ops)), Ops)
        ])
    }.

%% {Function, Arity} => the modules exporting it.
exported_by(Modules) ->
    maps:groups_from_list(
        fun({FA, _}) -> FA end,
        fun({_, M}) -> M end,
        [{FA, M} || {M, D} <- maps:to_list(Modules), FA <- maps:get(exports, D)]
    ).

validate_root({M, F, A}, Ms) ->
    case maps:find(M, Ms) of
        error ->
            error({missing_root, {M, F, A}});
        {ok, D} ->
            case
                lists:any(fun({N, Ar, _}) -> N =:= F andalso Ar =:= A end, maps:get(functions, D))
            of
                true -> ok;
                false -> error({missing_root, {M, F, A}})
            end
    end.
%% Replay the settled analysis once, keeping the instruction states this time.
%% Nothing can be discovered here that the fixed point did not already hold.
record_states(#{all := true} = S) ->
    S;
record_states(S) ->
    Recorded = lists:foldl(
        fun({MFA, Args}, Acc) -> analyze(MFA, Args, Acc) end,
        S#{recording => true},
        maps:to_list(maps:get(args, S))
    ),
    Recorded#{recording => false}.

%% What the fixed point is over. The decoded modules and the indexes built
%% from them never change, and comparing them every round would walk every
%% module's bytecode.
facts(S) ->
    maps:with(
        [
            unknown_tags,
            unknown_send,
            escaped_tags,
            protocol_tuples,
            open_protocol,
            args,
            returns,
            widened,
            groups,
            warnings,
            edges,
            dispatches,
            literals,
            literal_edges,
            messages,
            opaque_messages,
            opaque_arities,
            opaque_senders,
            native_ports,
            surfaced,
            closures,
            drivers,
            nifs,
            all,
            pending_package,
            bounded_fallback,
            all_named,
            discovery_limited,
            unknown_driver,
            boot_data
        ],
        S
    ).

fixpoint(S) ->
    fixpoint(S, all).

%% Re-analyzing every context on every round is what makes this slow: most
%% rounds change one summary. Analyze the contexts a change can reach, and
%% confirm with a full round, so the result is the same fixed point.
fixpoint(#{all := true} = S, _) ->
    S;
fixpoint(S, Dirty) ->
    Next = lists:foldl(
        fun({MFA, Args}, Acc) -> analyze_context(MFA, Args, Acc) end,
        S#{fresh => [], returned => #{}, rewidened => #{}},
        contexts(S, Dirty)
    ),
    case facts(Next) =:= facts(S) of
        false ->
            fixpoint(Next, dirty(S, Next));
        true when Dirty =/= all ->
            %% A settled worklist proves nothing about the contexts it left
            %% out. One full round decides.
            fixpoint(Next, all);
        true ->
            %% A context whose analyzed paths never reach a return does not
            %% return: a server loop, or a function that always raises. Its
            %% callers' continuations are unreachable, and widening the summary
            %% to an arbitrary value instead poisons every state that the loop
            %% carries.
            S#{all => maps:get(pending_package, S)}
    end.

contexts(S, all) ->
    [{MFA, Args} || {MFA, As} <- lists:sort(maps:to_list(maps:get(args, S))), Args <- As];
contexts(S, Dirty) ->
    Args = maps:get(args, S),
    [{MFA, A} || {MFA, A} <- Dirty, lists:member(A, maps:get(MFA, Args, []))].

%% What a round changed, and who reads it. A new context has not been
%% analyzed yet. A call site reads the summary of the context it calls, and
%% the widening that decides which context that is: when either moves, the
%% callers run again. A change to the global facts reaches any context, but
%% the full round that confirms a settled worklist covers it. Discovery adds
%% one call to a chain per round: each round analyzes that link, not
%% everything reached so far.
dirty(Old, New) ->
    Fresh = maps:get(fresh, New),
    Read = maps:keys(maps:merge(maps:get(returned, New), maps:get(rewidened, New))),
    case {Fresh, Read, inputs(Old) =:= inputs(New)} of
        {[], [], false} ->
            all;
        _ ->
            Callers = maps:get(callers, New),
            Args = maps:get(args, New),
            Readers = lists:usort(
                lists:append([maps:keys(maps:get(M, Callers, #{})) || M <- Read])
            ),
            lists:usort(Fresh ++ [{MFA, A} || MFA <- Readers, A <- maps:get(MFA, Args, [])])
    end.
%% The facts a context's analysis reads beyond its own arguments and the
%% summaries of what it calls.
inputs(S) ->
    maps:with(
        [
            unknown_tags,
            unknown_send,
            escaped_tags,
            protocol_tuples,
            open_protocol,
            messages,
            opaque_messages,
            opaque_arities,
            surfaced,
            closures,
            widened,
            native_ports,
            all,
            all_named,
            pending_package,
            boot_data
        ],
        S
    ).
reach(MFA, Args, S) ->
    case lists:member(none, Args) of
        true -> S;
        false -> reach_defined(MFA, Args, S)
    end.
reach_defined({M, F, A} = MFA, Args, S) ->
    Ms = maps:get(modules, S),
    case maps:find(M, Ms) of
        error ->
            S;
        {ok, D} ->
            case maps:is_key({F, A}, maps:get(function_code, D)) of
                false ->
                    S;
                true ->
                    S1 = add_context(MFA, relevant_args(MFA, Args, S), S),
                    %% on_load executes when a reachable module is first loaded.
                    OnLoad = [{M, N, Ar} || {N, Ar} <- maps:get(on_load, D)],
                    lists:foldl(
                        fun(K, Acc) ->
                            case maps:is_key(K, maps:get(args, Acc)) of
                                true -> Acc;
                                false -> reach_from(K, [], MFA, Acc)
                            end
                        end,
                        S1,
                        OnLoad
                    )
            end
    end.
%% Keep callback modules correlated with their arguments. Context growth is
%% bounded; recursive functions widen only after exhausting the context budget.
add_context(MFA, Args, S) ->
    Old = maps:get(args, S),
    Cs = maps:get(MFA, Old, []),
    Wide = maps:get(widened, S),
    case maps:get(MFA, Wide, false) of
        all ->
            [Prev] = Cs,
            set_contexts(MFA, [join_args(Prev, Args)], S);
        {grouped, Level} ->
            Groups = maps:get(MFA, maps:get(groups, S)),
            Id = context_identity(Level, Args),
            Rep =
                case maps:find(Id, Groups) of
                    {ok, Prev} -> join_args(Prev, Args);
                    error -> Args
                end,
            partitioned(MFA, Level, Groups#{Id => Rep}, S);
        false ->
            New = lists:usort([Args | Cs]),
            case length(New) =< context_limit(MFA, S) of
                true -> set_contexts(MFA, New, S);
                false -> partition(MFA, keys, New, S)
            end
    end.
%% Past the budget, contexts are merged only with contexts of the same
%% identity: the constant atom arguments, and the constant atoms in the fields
%% of record-like arguments. A keyed lookup keeps one context per key. A
%% generic server loop sees one context per request and per state it reaches,
%% but the server it runs is written in its state record (`{state, Name, Mod,
%% ...}'). Merging across that line would hand one server's callback data to
%% another server's callbacks.
%%
%% A context's partition is decided when it arrives and kept: the merged value
%% can have another identity (two constant tuples join into a choice), and
%% recomputing it would move the merge out of its partition while its call
%% site keeps re-creating it. Too many identities fall back to the record
%% fields alone, then to a single context. Each step is final, so the set only
%% grows.
partition(MFA, Level, Contexts, S) ->
    Groups = maps:map(
        fun(_, Members) -> join_contexts(Members) end,
        maps:groups_from_list(fun(Args) -> context_identity(Level, Args) end, Contexts)
    ),
    partitioned(MFA, Level, Groups, S).
partitioned(MFA, Level, Groups, S) ->
    Wide = maps:get(widened, S),
    case {maps:size(Groups) =< context_limit(MFA, S), Level} of
        {true, _} ->
            rewidened(
                MFA,
                S,
                set_contexts(MFA, lists:usort(maps:values(Groups)), S#{
                    groups => (maps:get(groups, S))#{MFA => Groups},
                    widened => Wide#{MFA => {grouped, Level}}
                })
            );
        {false, keys} ->
            partition(MFA, records, maps:values(Groups), S);
        {false, records} ->
            rewidened(
                MFA,
                S,
                set_contexts(MFA, [join_contexts(maps:values(Groups))], S#{
                    groups => maps:remove(MFA, maps:get(groups, S)),
                    widened => Wide#{MFA => all}
                })
            )
    end.
set_contexts(MFA, Contexts, S) ->
    Args = maps:get(args, S),
    Prev = maps:get(MFA, Args, []),
    case Contexts of
        Prev ->
            S;
        _ ->
            S#{
                args => Args#{MFA => Contexts},
                fresh => [{MFA, A} || A <- Contexts -- Prev] ++ maps:get(fresh, S)
            }
    end.
%% Call sites read which context a call maps to: when that changes, they run
%% again.
rewidened(MFA, Before, S) ->
    Same =
        maps:get(MFA, maps:get(widened, Before), false) =:= maps:get(MFA, maps:get(widened, S)) andalso
            maps:get(MFA, maps:get(groups, Before), none) =:=
                maps:get(MFA, maps:get(groups, S), none),
    case Same of
        true -> S;
        false -> S#{rewidened => (maps:get(rewidened, S))#{MFA => true}}
    end.
join_contexts([H | T]) -> lists:foldl(fun join_args/2, H, T).
context_identity(Level, Args) -> [arg_identity(Level, A) || A <- Args].
arg_identity(_, {const, T}) when is_tuple(T) ->
    arg_identity(records, {tuple, [{const, X} || X <- tuple_to_list(T)]});
arg_identity(_, {tuple, Vs}) ->
    {length(Vs), [field_identity(V) || V <- Vs]};
arg_identity(keys, {const, A}) when is_atom(A) ->
    A;
arg_identity(_, _) ->
    '_'.
field_identity({const, A}) when is_atom(A) -> A;
field_identity(_) -> '_'.
return_value(MFA, Args0, S) ->
    Args = relevant_args(MFA, Args0, S),
    Key =
        case maps:get(MFA, maps:get(widened, S), false) of
            false ->
                {MFA, Args};
            all ->
                {MFA, hd(maps:get(MFA, maps:get(args, S)))};
            {grouped, Level} ->
                Groups = maps:get(MFA, maps:get(groups, S)),
                {MFA, maps:get(context_identity(Level, Args), Groups, Args)}
        end,
    relevant_return(MFA, maps:get(Key, maps:get(returns, S), none), S).
return_summary(MFA, S) ->
    lists:foldl(
        fun(A, V) -> join(return_value(MFA, A, S), V) end,
        none,
        maps:get(MFA, maps:get(args, S), [])
    ).
%% A parameter that cannot decide a dynamic call is analyzed as any value:
%% calls that differ only there share one context.
relevant_args(MFA, Args, S) ->
    case maps:get(relevance, S) of
        undefined ->
            Args;
        #{params := Params} ->
            Relevant = maps:get(MFA, Params, []),
            [
                case lists:member(I, Relevant) of
                    true -> V;
                    false -> unknown
                end
             || {I, V} <- lists:zip(lists:seq(0, length(Args) - 1), Args)
            ]
    end.
%% A caller that cannot pass the return value on to a dynamic call only
%% learns whether the call returns.
relevant_return(MFA, V, S) ->
    case maps:get(relevance, S) of
        undefined ->
            V;
        #{returns := Returns} ->
            case maps:is_key(MFA, Returns) orelse V =:= none of
                true -> V;
                false -> unknown
            end
    end.
%% Abstract values: none (not yet returned), unknown, constants, finite choices,
%% closures, and structural containers with independently known fields.
join_args(A, B) -> lists:zipwith(fun join/2, A, B).
-spec join(term(), term()) -> term().
join(none, B) ->
    B;
join(A, none) ->
    A;
join(A, A) ->
    A;
%% A term that came from the mailbox keeps that provenance only while nothing
%% else is mixed into it.
join(received, _) ->
    unknown;
join(_, received) ->
    unknown;
join({const, T}, {tuple, Vs} = B) when is_tuple(T), tuple_size(T) =:= length(Vs) ->
    join({tuple, [{const, X} || X <- tuple_to_list(T)]}, B);
join({tuple, Vs} = A, {const, T} = B) when is_tuple(T), tuple_size(T) =:= length(Vs) -> join(B, A);
join({const, M}, {map, _} = B) when is_map(M) -> join(abstract_map({const, M}), B);
join({map, _} = A, {const, M} = B) when is_map(M) -> join(B, A);
join({tuple, As} = A, {tuple, Bs} = B) when length(As) =:= length(Bs) ->
    case {As, Bs} of
        {[{const, TA} | _], [{const, TB} | _]} when is_atom(TA), is_atom(TB), TA =/= TB ->
            join_choices(A, B);
        _ ->
            {tuple, join_args(As, Bs)}
    end;
join({map, A}, {map, B}) ->
    {map,
        maps:from_list([
            {K, join(maps:get(K, A, unknown), maps:get(K, B, unknown))}
         || K <- lists:usort(maps:keys(A) ++ maps:keys(B))
        ])};
join(unknown, _) ->
    unknown;
join(_, unknown) ->
    unknown;
join(A, B) ->
    case {list_shape(A), list_shape(B)} of
        {proper, proper} ->
            join_lists(A, B);
        {open, open} ->
            {cons, H1, T1} = A,
            {cons, H2, T2} = B,
            {cons, join(H1, H2), join(T1, T2)};
        _ ->
            join_choices(A, B)
    end.
%% Whether list_elements/1 holds, without joining the elements: a proper
%% list, a list whose tail is not known to be a list (open), or neither.
list_shape({sequence, _}) ->
    proper;
list_shape({const, L}) when is_list(L) -> const_list_shape(L);
list_shape({cons, _, T}) ->
    case list_shape(T) of
        proper -> proper;
        _ -> open
    end;
list_shape({choices, Vs}) ->
    case lists:all(fun(V) -> list_shape(V) =:= proper end, Vs) of
        true -> proper;
        false -> other
    end;
list_shape(_) ->
    other.
const_list_shape([]) -> proper;
const_list_shape([_ | T]) -> const_list_shape(T);
const_list_shape(_) -> other.
%% A list whose tail is not known to be a list: a cell, then anything. Two
%% such lists join cell by cell. A proper list stays an alternative: its
%% length is what an apply reads.
is_open_list(V) -> list_shape(V) =:= open.
%% Preserve positional information and alternative arities for apply/spawn.
%% Only recursive/unbounded list shapes need an element-only summary.
join_lists(A, B) ->
    case {fixed_list(A), fixed_list(B)} of
        {{ok, As}, {ok, Bs}} when length(As) =:= length(Bs) ->
            lists:foldr(fun(V, T) -> {cons, V, T} end, {const, []}, join_args(As, Bs));
        _ ->
            Vs = lists:umerge(choices(A), choices(B)),
            case
                length(Vs) =< ?CHOICE_LIMIT andalso
                    lists:all(fun(V) -> fixed_list(V) =/= error end, Vs)
            of
                true ->
                    {choices, Vs};
                false ->
                    {ok, EA} = list_elements(A),
                    {ok, EB} = list_elements(B),
                    {sequence, join(EA, EB)}
            end
    end.
fixed_list({const, []}) ->
    {ok, []};
fixed_list({const, [H | T]}) ->
    case fixed_list({const, T}) of
        {ok, Vs} -> {ok, [{const, H} | Vs]};
        error -> error
    end;
fixed_list({cons, H, T}) ->
    case fixed_list(T) of
        {ok, Vs} -> {ok, [H | Vs]};
        error -> error
    end;
fixed_list(_) ->
    error.
%% Alternatives are kept sorted: merging them is linear.
join_choices(A, B) ->
    case normalize_choices(lists:umerge(choices(A), choices(B))) of
        [V] -> V;
        Vs when length(Vs) =< ?CHOICE_LIMIT -> {choices, Vs};
        Vs -> widen_choices(Vs)
    end.
%% A choice never keeps two alternatives that a direct join would merge, or
%% joining a value the choice already covers would change it, and a widened
%% context would never settle: an abstract map covers the constant maps, a
%% tuple whose tag is not a constant atom covers every tuple of its arity, and
%% a record whose fields were joined covers the alternatives with its tag and
%% arity.
normalize_choices(Vs) ->
    case lists:any(fun is_structure/1, Vs) of
        true -> normalize_structures(Vs);
        false -> Vs
    end.
is_structure({tuple, _}) -> true;
is_structure({map, _}) -> true;
is_structure({cons, _, _}) -> true;
is_structure(_) -> false.
normalize_structures(Vs) ->
    case lists:filter(fun is_open_list/1, Vs) of
        [] -> normalize_containers(Vs);
        [Open | _] -> normalize_containers(merge_covered(Open, fun is_open_list/1, Vs))
    end.
normalize_containers(Vs0) ->
    Vs1 =
        case [V || {map, _} = V <- Vs0] of
            [] -> Vs0;
            [First | _] -> merge_covered(First, fun is_map_value/1, Vs0)
        end,
    Vs2 = lists:foldl(
        fun(N, Acc) ->
            case [V || {tuple, Fs} = V <- Acc, length(Fs) =:= N, record_key(V) =:= none] of
                [] -> Acc;
                [Wild | _] -> merge_covered(Wild, fun(V) -> tuple_size_of(V) =:= N end, Acc)
            end
        end,
        Vs1,
        lists:usort([length(Fs) || {tuple, Fs} = V <- Vs1, record_key(V) =:= none])
    ),
    lists:foldl(
        fun(K, Acc) ->
            case [V || {tuple, _} = V <- Acc, record_key(V) =:= K] of
                [] -> Acc;
                [Record | _] -> merge_covered(Record, fun(V) -> record_key(V) =:= K end, Acc)
            end
        end,
        Vs2,
        lists:usort([record_key(V) || {tuple, _} = V <- Vs2, record_key(V) =/= none])
    ).
%% Starting from the abstract value matters: two constants alone would join
%% into a choice again.
merge_covered(Cover, Covers, Vs) ->
    {In, Out} = lists:partition(Covers, Vs),
    lists:usort([lists:foldl(fun(V, Acc) -> join(Acc, V) end, Cover, In) | Out]).
is_map_value({map, _}) -> true;
is_map_value({const, M}) -> is_map(M);
is_map_value(_) -> false.
tuple_size_of({tuple, Fs}) -> length(Fs);
tuple_size_of({const, T}) when is_tuple(T) -> tuple_size(T);
tuple_size_of(_) -> none.
record_key({tuple, [{const, Tag} | _] = Vs}) when is_atom(Tag) -> {Tag, length(Vs)};
record_key({const, T}) when is_tuple(T), tuple_size(T) > 0, is_atom(element(1, T)) ->
    {element(1, T), tuple_size(T)};
record_key(_) ->
    none.
%% Losing a changing counter or state field must not erase a stable callback
%% module/start MFA elsewhere in the same record.
widen_choices(Vs) ->
    %% Merge each record's alternatives first: its tag stays tied to its
    %% fields, so a test on the tag still tells which field holds what.
    {Records, Others} = lists:partition(fun(V) -> record_key(V) =/= none end, Vs),
    Merged =
        Others ++
            [
                lists:foldl(fun(V, Acc) -> join(Acc, record_tuple(V)) end, none, Group)
             || Group <- maps:values(maps:groups_from_list(fun record_key/1, Records))
            ],
    case {Merged, length(Merged) =< ?CHOICE_LIMIT} of
        {[V], _} -> V;
        {_, true} -> {choices, lists:usort(Merged)};
        {_, false} -> widen_arities(Vs)
    end.
record_tuple({const, T}) -> {tuple, [{const, X} || X <- tuple_to_list(T)]};
record_tuple(V) -> V.
widen_arities(Vs) ->
    case lists:usort([tuple_arity(V) || V <- Vs]) of
        [{const, N}] ->
            {tuple, [
                lists:foldl(fun(V, Acc) -> join(tuple_get(I, V), Acc) end, none, Vs)
             || I <- lists:seq(0, N - 1)
            ]};
        _ ->
            unknown
    end.
%% A proper list of arbitrary length, with a bound on every element.
%% Unlike a truncated cons spine, this keeps closure provenance through loops.
list_elements({sequence, E}) ->
    {ok, E};
list_elements({const, []}) ->
    {ok, none};
list_elements({const, [H | T]}) ->
    %% `is_list/1' holds for an improper list too, so walk the cells.
    case list_elements({const, T}) of
        {ok, E} -> {ok, join({const, H}, E)};
        error -> error
    end;
list_elements({cons, H, T}) ->
    case list_elements(T) of
        {ok, E} -> {ok, join(H, E)};
        error -> error
    end;
list_elements({choices, Vs}) ->
    lists:foldl(
        fun
            (V, {ok, A}) ->
                case list_elements(V) of
                    {ok, E} -> {ok, join(A, E)};
                    error -> error
                end;
            (_, error) ->
                error
        end,
        {ok, none},
        Vs
    );
list_elements(_) ->
    error.
project(F, Vs) -> lists:foldl(fun(V, A) -> join(F(V), A) end, none, Vs).
choices({choices, Vs}) -> Vs;
choices(V) -> [V].
join_regs(A, B) ->
    maps:from_list([
        {K, join(maps:get(K, A, unknown), maps:get(K, B, unknown))}
     || K <- lists:usort(maps:keys(A) ++ maps:keys(B)),
        element(1, K) =/= origin orelse
            (maps:is_key(K, A) andalso maps:get(K, A) =:= maps:get(K, B, undefined))
    ]).

analyze(_, _, #{all := true} = S) ->
    S;
analyze(MFA, Contexts, S) ->
    lists:foldl(fun(Args, Acc) -> analyze_context(MFA, Args, Acc) end, S, Contexts).
analyze_context(_, _, #{all := true} = S) ->
    S;
analyze_context({M, F, A} = MFA, Args, S) ->
    D = maps:get(M, maps:get(modules, S)),
    {Is, Labels} = maps:get({F, A}, maps:get(function_code, D)),
    Regs = maps:from_list([
        {{?COMPACT_XREG, N}, V}
     || {N, V} <- lists:zip(lists:seq(0, length(Args) - 1), Args)
    ]),
    flow([{entry_index(Is), Regs}], #{}, Is, Labels, MFA, D, S#{active => {MFA, Args}}).
flow(_, _, _, _, _, _, #{all := true} = S) ->
    S;
flow(_, _, _, _, _, _, #{pending_package := true, discovery_budget := 0, recording := false} = S) ->
    S#{all => true, discovery_limited => true};
flow([], Seen, _Is, _, _MFA, _D, S) ->
    %% The visited instruction states are only needed to rewrite the output.
    %% Keeping them out of the fixed point keeps the state small enough to
    %% compare, and one pass over the settled facts rebuilds them.
    case maps:get(recording, S) of
        false ->
            S;
        true ->
            Key = maps:get(active, S),
            S#{code => (maps:get(code, S))#{Key => Seen}}
    end;
flow([{I, R} | Work], Seen, Is, Labels, MFA, D, S) when I =< tuple_size(Is) ->
    Joined =
        case maps:find(I, Seen) of
            error -> R;
            {ok, Old} -> join_regs(Old, R)
        end,
    case maps:find(I, Seen) of
        {ok, Joined} ->
            flow(Work, Seen, Is, Labels, MFA, D, S);
        _ ->
            {_, Op, As} = element(I, Is),
            S0 = escape_tags(Op, As, D, literal_roots(As, MFA, D, S#{instruction => I})),
            {R1, S1} = step(Op, As, Joined, MFA, D, S0),
            Targets = [maps:get(L, Labels) || L <- labels(As), L =/= 0, maps:is_key(L, Labels)],
            DefaultNext =
                case Op of
                    func_info -> [];
                    call -> [I + 1];
                    return -> [];
                    call_only -> [];
                    call_last -> [];
                    call_ext_only -> [];
                    call_ext_last -> [];
                    apply_last -> [];
                    int_code_end -> [];
                    badmatch -> [];
                    case_end -> [];
                    if_end -> [];
                    select_val -> Targets;
                    select_tuple_arity -> Targets;
                    jump -> Targets;
                    wait -> Targets;
                    loop_rec_end -> Targets;
                    _ -> [I + 1 | Targets]
                end,
            BranchNext =
                case branch(Op, As, Joined, D) of
                    success -> [I + 1];
                    {jump, L} -> [maps:get(L, Labels)];
                    unknown -> DefaultNext
                end,
            %% A not-yet-known return is bottom, not arbitrary runtime data.
            %% Revisit the continuation when the callee's summary grows.
            Next =
                case
                    lists:member(Op, [call, call_ext, call_fun, call_fun2, apply]) andalso
                        maps:get({?COMPACT_XREG, 0}, R1, unknown) =:= none
                of
                    true -> [];
                    false -> BranchNext
                end,
            %% Failure edges observe the input registers; successful writes may not have occurred.
            %% An edge that no possible value takes is not followed.
            NewWork = [
                {N, refine_message_edge(Op, As, N, I, Labels, Refined, D, S1)}
             || N <- lists:usort(Next),
                Refined <- [
                    refine_value_edge(
                        Op,
                        As,
                        N,
                        I,
                        Labels,
                        case {N =:= I + 1, Op} of
                            {true, _} -> R1;
                            {false, wait} -> R1;
                            {false, wait_timeout} -> R1;
                            {false, 'try'} -> #{};
                            {false, 'catch'} -> #{};
                            _ -> Joined
                        end,
                        D
                    )
                ],
                Refined =/= none
            ],
            Budgeted =
                case maps:get(pending_package, S1) of
                    true -> S1#{discovery_budget => max(0, maps:get(discovery_budget, S1) - 1)};
                    false -> S1
                end,
            flow(Work ++ NewWork, Seen#{I => Joined}, Is, Labels, MFA, D, Budgeted)
    end;
flow([_ | Work], Seen, Is, L, M, D, S) ->
    flow(Work, Seen, Is, L, M, D, S).
%% Projection origins propagate a constraint on a tuple field back to its
%% source and sibling projections: a local backward step inside the forward
%% fixed point, not a guess based on every module present in the package.
refine_value_edge(Op, [Src, {?COMPACT_LABEL, Fail}, {list, Pairs}], N, _I, Labels, R, D) when
    Op =:= select_val; Op =:= select_tuple_arity
->
    Pred = fun(V) ->
        SV =
            case Op of
                select_val -> V;
                select_tuple_arity -> tuple_arity(V)
            end,
        case SV of
            {const, C} -> maps:get(select_target(C, Pairs, Fail, R, D), Labels, 0) =:= N;
            _ -> true
        end
    end,
    R0 =
        case {Op, val(Src, R, D)} of
            {select_tuple_arity, {mailbox, Id, []}} ->
                Sizes = [
                    A
                 || [V, {?COMPACT_LABEL, L}] <- pairs(Pairs),
                    maps:get(L, Labels, 0) =:= N,
                    {const, A} <- [branch_value(V, R, D)]
                ],
                case {Sizes, maps:get(Fail, Labels, 0) =:= N} of
                    {[Size], false} -> R#{{mailbox_shape, Id} => {const, Size}};
                    _ -> R
                end;
            _ ->
                R
        end,
    narrow_register(Src, Pred, R0);
refine_value_edge(Op, [{?COMPACT_LABEL, Fail}, Src | Tail], N, I, Labels, R, D) ->
    case maps:get(Fail, Labels, 0) =:= N andalso N =:= I + 1 of
        true ->
            R;
        false ->
            Expected = N =:= I + 1,
            Pred = fun(V) ->
                case predicate(Op, [V | [branch_value(A, R, D) || A <- Tail]]) of
                    unknown -> true;
                    B -> B =:= Expected
                end
            end,
            case {narrow_register(Src, Pred, R), Expected} of
                {none, _} -> none;
                {R1, true} -> shape_register(Op, Src, Tail, R1, D);
                {R1, false} -> R1
            end
    end;
refine_value_edge(_, _, _, _, _, R, _) ->
    R.
%% A guard that succeeded proves a shape. Give an otherwise unknown value the
%% shape the test accepted, so a matched message or record keeps its tag and
%% arity on the success edge. Values that already carry more information,
%% including mailbox identities, are left untouched.
shape_register(Op, Src, Tail, R, D) ->
    case {register(Src), val(Src, R, D)} of
        {undefined, _} ->
            R;
        {Reg, unknown} ->
            case test_shape(Op, Tail, R, D) of
                {ok, V} -> put(Reg, V, R);
                error -> R
            end;
        _ ->
            R
    end.
test_shape(is_tagged_tuple, [Size, Tag], R, D) ->
    case {branch_value(Size, R, D), branch_value(Tag, R, D)} of
        {{const, N}, {const, _} = T} when is_integer(N), N >= 1 ->
            {ok, {tuple, [T | lists:duplicate(N - 1, unknown)]}};
        _ ->
            error
    end;
test_shape(test_arity, [Size], R, D) ->
    case branch_value(Size, R, D) of
        {const, N} when is_integer(N), N >= 0 -> {ok, {tuple, lists:duplicate(N, unknown)}};
        _ -> error
    end;
test_shape(is_eq_exact, [Value], R, D) ->
    case branch_value(Value, R, D) of
        {const, _} = C -> {ok, C};
        _ -> error
    end;
test_shape(is_nonempty_list, [], _, _) ->
    {ok, {cons, unknown, unknown}};
test_shape(_, _, _, _) ->
    error.
register({typed, R, _}) -> register(R);
register({T, _} = R) when T =:= ?COMPACT_XREG; T =:= ?COMPACT_YREG -> R;
register(_) -> undefined.
narrow_register(Src, Pred, R) ->
    case register(Src) of
        undefined ->
            R;
        Reg ->
            {Root, Path} =
                case maps:get({origin, Reg}, R, undefined) of
                    {K, P} when is_list(P) -> {K, P};
                    _ -> {Reg, []}
                end,
            Old = maps:get(Root, R, unknown),
            V = constrain(Old, Path, Pred),
            case V of
                %% No value takes this edge.
                none ->
                    none;
                Old ->
                    R;
                _ ->
                    Base = R#{Root => V},
                    maps:fold(
                        fun
                            ({origin, K}, {Owner, P}, Acc) when Owner =:= Root, is_list(P) ->
                                Acc#{K => project_path(V, P)};
                            (_, _, Acc) ->
                                Acc
                        end,
                        Base,
                        R
                    )
            end
    end.
constrain({choices, Vs}, Path, Pred) ->
    project(fun(V) -> constrain(V, Path, Pred) end, Vs);
constrain(V, [], Pred) ->
    case Pred(V) of
        true -> V;
        false -> none
    end;
constrain(V, [I | Rest], Pred) ->
    case constrain(tuple_get(I, V), Rest, Pred) of
        none ->
            none;
        W ->
            case tuple_arity(V) of
                {const, _} -> tuple_set(I, W, V);
                _ -> V
            end
    end.
project_path(V, Path) -> lists:foldl(fun tuple_get/2, V, Path).
drop_origins(R) -> maps:filter(fun({T, _}, _) -> T =/= origin andalso T =/= detached end, R).
put_projection(Dst, Src, Path, V, R) ->
    Reg = register(Dst),
    case register(Src) of
        undefined ->
            put(Reg, V, R);
        Reg when Path =:= [] ->
            put(Reg, V, R);
        Source ->
            {Root, Prefix} =
                case maps:get({origin, Source}, R, undefined) of
                    {K, P} when is_list(P) -> {K, P};
                    _ -> {Source, []}
                end,
            %% Reading a field into the register that holds the tuple: the
            %% tuple lives on, detached, so the field stays tied to it.
            case Root =:= Reg of
                true ->
                    (put(Reg, V, detach(Reg, R)))#{
                        {origin, Reg} => {{detached, Reg}, Prefix ++ Path}
                    };
                false ->
                    (put(Reg, V, R))#{{origin, Reg} => {Root, Prefix ++ Path}}
            end
    end.

%% Control requests are bounded by what reachable code sends, whoever receives
%% it: registered names hide the receiver. Opaque sends, unknown ports and
%% distributed peers disable the bound.
protocol_tag(system) ->
    true;
protocol_tag(T) when is_atom(T) ->
    %% Control protocols reserve a leading $ ('$gen_call', '$atomvm_...'), so a
    %% port or a peer never produces these tags behind the analyzer's back.
    case atom_to_list(T) of
        [$$ | _] -> true;
        _ -> false
    end;
protocol_tag(_) ->
    false.
message(none, S) ->
    S;
%% Re-sending a message taken from the mailbox cannot introduce a shape that
%% no reachable sender produced. A projection of one can, and stays opaque.
message({mailbox, _, _} = V, S) ->
    message_term(V, S);
message(received, S) ->
    message_term(received, S);
message({const, T}, S) when not is_tuple(T) -> S;
message({type, _}, S) ->
    S;
message({cons, _, _}, S) ->
    S;
message({map, _}, S) ->
    S;
message({sequence, _}, S) ->
    S;
message({choices, Vs}, S) ->
    lists:foldl(fun message/2, S, Vs);
message(V, S) ->
    S1 = message_term(V, S),
    %% Native port protocols can echo the caller's reply tag. Include that
    %% generated reply too; a caller-supplied atom need not be a reference.
    case {maps:get(native_ports, S), tuple_arity(V)} of
        {[], _} ->
            S1;
        {Ports, {const, 2}} ->
            case lists:member("echo", Ports) of
                true -> message_term(tuple_get(1, V), S1);
                false -> S1
            end;
        {_, {const, 3}} ->
            case tuple_get(0, V) of
                {const, T} when not is_pid(T) -> S1;
                _ -> message_term(construct(tuple, [tuple_get(1, V), unknown]), S1)
            end;
        _ ->
            S1
    end.
message_term(none, S) ->
    S;
%% A term taken from the mailbox was built by whoever sent that message: it is
%% one of the recorded messages, or a part of one. Record the forwarding and
%% bound such sends by the matching subterms instead of losing every bound.
message_term({mailbox, _, _}, S) ->
    S#{forwarded_messages => true};
message_term(received, S) ->
    S#{forwarded_messages => true};
message_term({choices, Vs}, S) ->
    lists:foldl(fun message_term/2, S, Vs);
message_term({const, T}, S) when not is_tuple(T) -> S;
message_term({type, _}, S) ->
    S;
message_term({cons, _, _}, S) ->
    S;
message_term({map, _}, S) ->
    S;
message_term({sequence, _}, S) ->
    S;
message_term(V, S) ->
    case {tuple_arity(V), tuple_get(0, V)} of
        {{const, A}, {const, Tag}} when is_atom(Tag) ->
            Ms = maps:get(messages, S),
            K = {Tag, A},
            S#{messages => Ms#{K => alternative(V, maps:get(K, Ms, []))}};
        {{const, _}, {type, reference}} ->
            S;
        {{const, _}, {type, pid}} ->
            S;
        {_, {const, _}} ->
            S;
        _ ->
            {Caller, _} = maps:get(active, S),
            Senders = maps:get(opaque_senders, S),
            %% A tuple of known arity with a tag we cannot see is sent: that
            %% arity may carry any escaped protocol tag. A term of unknown
            %% shape may be any tuple the program built with such a tag.
            S0 =
                case tuple_arity(V) of
                    {const, OA} -> open_protocol(OA, S);
                    _ -> S#{unknown_send => true}
                end,
            S0#{
                opaque_messages => true,
                opaque_arities => lists:usort([
                    case tuple_arity(V) of
                        {const, A} -> A;
                        _ -> any
                    end
                    | maps:get(opaque_arities, S)
                ]),
                opaque_senders => Senders#{Caller => join(maps:get(Caller, Senders, none), V)}
            }
    end.
%% Keep the payloads a tag was sent with side by side: joining them early
%% turns a handful of request shapes into an arbitrary value, and the server
%% then looks able to handle requests nobody sends. Past the bound they are
%% joined once and for all, so this still settles.
alternative(V, {joined, W}) ->
    {joined, join(W, V)};
alternative(V, Vs) ->
    case lists:member(V, Vs) of
        true ->
            Vs;
        false ->
            New = lists:usort([V | Vs]),
            case length(New) =< ?MESSAGE_ALTERNATIVES of
                true -> New;
                false -> {joined, lists:foldl(fun join/2, none, New)}
            end
    end.
alternatives({joined, V}) -> [V];
alternatives(Vs) -> Vs.

%% A tag that is not a constant could be any protocol tag.
built(V, S) ->
    case {tuple_arity(V), tuple_get(0, V)} of
        {{const, A}, Tag} -> built(A, Tag, V, S);
        _ -> S
    end.
built(A, {const, T}, V, S) when is_atom(T) ->
    case protocol_tag(T) of
        true ->
            PT = maps:get(protocol_tuples, S),
            K = {T, A},
            S#{protocol_tuples => PT#{K => alternative(V, maps:get(K, PT, []))}};
        false ->
            S
    end;
built(_, {const, _}, _, S) ->
    S;
built(_, {type, _}, _, S) ->
    %% Pids and references are never atoms.
    S;
built(_, none, _, S) ->
    S;
built(A, {choices, Tags}, V, S) ->
    lists:foldl(fun(T, Acc) -> built(A, T, tuple_set(0, T, V), Acc) end, S, Tags);
built(A, _, _, S) ->
    %% Built, not sent: this only matters if an unknown term is sent.
    Known = maps:get(unknown_tags, S),
    case lists:member(A, Known) of
        true -> S;
        false -> S#{unknown_tags => lists:usort([A | Known])}
    end.
open_protocol(A, S) ->
    Open = maps:get(open_protocol, S),
    case lists:member(A, Open) of
        true -> S;
        false -> S#{open_protocol => lists:usort([A | Open])}
    end.
%% A tuple whose tag the analysis cannot see only carries a protocol tag if
%% that atom was used as a plain value somewhere: stored, passed or returned,
%% rather than written as the tag of the tuple being built or compared
%% against. `gen:call' is how `'$gen_call'' and `system' escape that way.
%% Decoding and atom creation from runtime data can produce any atom.
escape_tags(Op, _, _, S) when
    Op =:= select_val;
    Op =:= select_tuple_arity;
    Op =:= is_eq_exact;
    Op =:= is_ne_exact;
    Op =:= is_eq;
    Op =:= is_ne;
    Op =:= is_tagged_tuple;
    Op =:= func_info;
    Op =:= label;
    Op =:= line
->
    S;
escape_tags(put_tuple2, [_, {list, [_ | Fields]}], D, S) ->
    escape_atoms(
        operand_atoms(Fields, maps:get(atoms, D), none) ++ literal_data_atoms(Fields, D), S
    );
escape_tags(_, As, D, S) ->
    escape_atoms(
        operand_atoms(As, maps:get(atoms, D), none) ++ literal_data_atoms(As, D),
        S
    ).
literal_data_atoms(As, D) ->
    [
        A
     || {literal, N} <- literal_operands(As),
        A <- data_atoms(binary_to_term(maps:get(N, maps:get(literals, D))))
    ].
literal_operands(As) when is_list(As) -> lists:append([literal_operands(A) || A <- As]);
literal_operands({list, As}) -> literal_operands(As);
literal_operands({literal, _} = L) -> [L];
literal_operands(_) -> [].
data_atoms(A) when is_atom(A) -> [A];
data_atoms(T) when is_tuple(T), tuple_size(T) > 0 ->
    [_ | Fields] = tuple_to_list(T),
    lists:append([data_atoms(F) || F <- Fields]);
data_atoms([H | T]) ->
    data_atoms(H) ++ data_atoms(T);
data_atoms(M) when is_map(M) -> data_atoms(maps:to_list(M));
data_atoms(_) ->
    [].
escape_atoms(Atoms, S) ->
    case [A || A <- Atoms, is_atom(A), protocol_tag(A)] of
        [] ->
            S;
        Tags ->
            E = maps:get(escaped_tags, S),
            case [T || T <- Tags, not maps:is_key(T, E)] of
                [] -> S;
                New -> S#{escaped_tags => maps:merge(E, maps:from_keys(New, true))}
            end
    end.
escape_all(S) ->
    escape_atoms_all(S).
escape_atoms_all(S) ->
    E = maps:get(escaped_tags, S),
    case maps:is_key('*', E) of
        true -> S;
        false -> S#{escaped_tags => E#{'*' => true}}
    end.

literal_protocol(T, S) when is_tuple(T), tuple_size(T) > 0 ->
    S1 =
        case element(1, T) of
            Tag when is_atom(Tag) -> built(tuple_size(T), {const, Tag}, {const, T}, S);
            _ -> S
        end,
    literal_protocol(tuple_to_list(T), S1);
literal_protocol([H | T], S) ->
    literal_protocol(T, literal_protocol(H, S));
literal_protocol(M, S) when is_map(M) ->
    literal_protocol(maps:values(M), S);
literal_protocol(_, S) ->
    S.

mailbox_arity(Id, R) ->
    case maps:get({mailbox_shape, Id}, R, unknown) of
        {const, N} -> N;
        _ -> any
    end.
%% A protocol-tagged message is a tuple some reachable code built, so the
%% tuples built with that tag bound it, whatever an opaque send carries. Only
%% a tuple whose tag the analysis could not see -- built from runtime data, a
%% peer, or an unknown port -- leaves that arity unbounded.
message_value(Tag, Arity, S) ->
    Open =
        maps:get(open_protocol, S) ++
            case maps:get(unknown_send, S) of
                true -> maps:get(unknown_tags, S);
                false -> []
            end,
    Escaped = maps:get(escaped_tags, S),
    case
        (lists:member(any, Open) orelse lists:member(Arity, Open) orelse
            (Arity =:= any andalso Open =/= [])) andalso
            (maps:is_key(Tag, Escaped) orelse maps:is_key('*', Escaped))
    of
        true ->
            unknown;
        false ->
            Matching = lists:append([
                alternatives(E)
             || Table <- [maps:get(messages, S), maps:get(protocol_tuples, S)],
                {{T, A}, E} <- maps:to_list(Table),
                T =:= Tag,
                (Arity =:= any orelse A =:= Arity)
            ]),
            Sent =
                case lists:usort(Matching) of
                    [] -> none;
                    [V] -> V;
                    Vs when length(Vs) =< ?MESSAGE_ALTERNATIVES -> {choices, Vs};
                    Vs -> lists:foldl(fun join/2, none, Vs)
                end,
            %% A forwarded term was built somewhere too: if it is a
            %% protocol tuple, it is among those built with its tag, or it
            %% was built with a tag the escape rule above accounts for.
            Sent
    end.
refine_message_edge(
    select_val, [Src, {?COMPACT_LABEL, Fail}, {list, Pairs}], N, _I, Labels, R, D, S
) ->
    case val(Src, R, D) of
        {mailbox, Id, [0]} ->
            Tags = [
                T
             || [V, {?COMPACT_LABEL, L}] <- pairs(Pairs),
                maps:get(L, Labels, 0) =:= N,
                {const, T} <- [val(V, R, D)]
            ],
            case {Tags, maps:get(Fail, Labels, 0) =:= N} of
                {[Tag], false} ->
                    case protocol_tag(Tag) of
                        true -> resolve_mailbox(R, Id, message_value(Tag, mailbox_arity(Id, R), S));
                        false -> R
                    end;
                _ ->
                    R
            end;
        _ ->
            R
    end;
refine_message_edge(is_tagged_tuple, [_Fail, Src, Size, Tag], N, I, _Labels, R, D, S) when
    N =:= I + 1
->
    case {val(Src, R, D), val(Tag, R, D)} of
        {{mailbox, Id, []}, {const, T}} ->
            case protocol_tag(T) of
                true ->
                    Arity =
                        case branch_value(Size, R, D) of
                            {const, A} -> A;
                            _ -> any
                        end,
                    resolve_mailbox(R, Id, message_value(T, Arity, S));
                false ->
                    R
            end;
        _ ->
            R
    end;
refine_message_edge(is_eq_exact, [_Fail, Src, Tag], N, I, _Labels, R, D, S) when N =:= I + 1 ->
    case {val(Src, R, D), val(Tag, R, D)} of
        {{mailbox, Id, [0]}, {const, T}} ->
            case protocol_tag(T) of
                true -> resolve_mailbox(R, Id, message_value(T, mailbox_arity(Id, R), S));
                false -> R
            end;
        _ ->
            R
    end;
refine_message_edge(_, _, _, _, _, R, _, _) ->
    R.
pairs([]) -> [];
pairs([A, B | T]) -> [[A, B] | pairs(T)].
%% An unbounded message is still a term that came from the mailbox: keeping
%% that provenance is more precise than an arbitrary value, and it keeps a
%% forward of the message bounded.
resolve_mailbox(R, _Id, unknown) ->
    R;
resolve_mailbox(R, Id, V) ->
    maps:map(
        fun
            (_, {mailbox, I, Path}) when I =:= Id -> lists:foldl(fun tuple_get/2, V, Path);
            (_, X) -> X
        end,
        R
    ).
forget_mailbox({mailbox, _, _}) -> received;
forget_mailbox({tuple, Vs}) -> {tuple, [forget_mailbox(V) || V <- Vs]};
forget_mailbox({cons, H, T}) -> {cons, forget_mailbox(H), forget_mailbox(T)};
forget_mailbox({map, M}) -> {map, maps:map(fun(_, V) -> forget_mailbox(V) end, M)};
forget_mailbox({choices, Vs}) -> project(fun forget_mailbox/1, Vs);
forget_mailbox({sequence, E}) -> {sequence, forget_mailbox(E)};
forget_mailbox(V) -> V.

labels(L) when is_list(L) -> lists:append([labels(X) || X <- L]);
labels({?COMPACT_LABEL, N}) -> [N];
labels({list, L}) -> labels(L);
labels(_) -> [].

val({typed, A, _}, R, D) -> val(A, R, D);
val({?COMPACT_ATOM, 0}, _, _) -> {const, []};
val({?COMPACT_ATOM, N}, _, D) -> {const, maps:get(N, maps:get(atoms, D))};
val({?COMPACT_INTEGER, N}, _, _) -> {const, N};
val({literal, N}, _, D) -> {const, binary_to_term(maps:get(N, maps:get(literals, D)))};
val({T, _} = K, R, _) when T =:= ?COMPACT_XREG; T =:= ?COMPACT_YREG -> maps:get(K, R, unknown);
val(_, _, _) -> unknown.
erase_register(K, R) -> maps:remove(K, put(K, unknown, R)).
put(K, V, R0) ->
    %% Overwriting a tuple whose fields other registers still hold keeps the
    %% tuple under a detached key: a later test on one field narrows the
    %% others, as the compiler reads every field before testing the tag. One
    %% pass finds out, and drops the origins the write invalidates.
    {Detach, Kept} = maps:fold(
        fun
            ({origin, Dst} = O, {Root, P} = Origin, {D, Acc}) ->
                {
                    D orelse (Root =:= K andalso is_list(P) andalso Dst =/= K),
                    case Dst =/= K andalso Root =/= K of
                        true -> Acc#{O => Origin};
                        false -> Acc
                    end
                };
            ({origin, Dst} = O, Origin, {D, Acc}) ->
                {
                    D,
                    case Dst =/= K of
                        true -> Acc#{O => Origin};
                        false -> Acc
                    end
                };
            (Key, Value, {D, Acc}) ->
                {D, Acc#{Key => Value}}
        end,
        {false, #{}},
        R0
    ),
    case Detach of
        false -> Kept#{K => V};
        true -> put_detached(K, V, detach(K, R0))
    end.
put_detached(K, V, R) ->
    Clean = maps:filter(
        fun
            ({origin, Dst}, {Root, _}) -> Dst =/= K andalso Root =/= K;
            ({origin, Dst}, _) -> Dst =/= K;
            (_, _) -> true
        end,
        R
    ),
    Clean#{K => V}.
detach(K, R) ->
    Detached = {detached, K},
    maps:fold(
        fun
            ({origin, _} = O, {Root, P}, Acc) when Root =:= K, is_list(P) ->
                Acc#{O => {Detached, P}};
            ({origin, _}, {Root, _}, Acc) when Root =:= Detached ->
                Acc;
            (Key, _, Acc) when Key =:= Detached ->
                Acc;
            (Key, V, Acc) ->
                Acc#{Key => V}
        end,
        #{Detached => maps:get(K, R, unknown)},
        R
    ).
xargs(N, R) -> [maps:get({?COMPACT_XREG, I}, R, unknown) || I <- lists:seq(0, N - 1)].
clobber(R, V) ->
    (maps:filter(fun({T, _}, _) -> T =:= ?COMPACT_YREG end, R))#{{?COMPACT_XREG, 0} => V}.
result(_MFA, V0, S) ->
    V = forget_mailbox(V0),
    Old = maps:get(returns, S),
    {MFA, _} = Key = maps:get(active, S),
    Prev = maps:get(Key, Old, none),
    New = join(Prev, V),
    Returned =
        case relevant_return(MFA, Prev, S) =:= relevant_return(MFA, New, S) of
            true -> maps:get(returned, S);
            false -> (maps:get(returned, S))#{MFA => true}
        end,
    S#{returns => Old#{Key => New}, returned => Returned}.

%% Installing/removing an exception handler only changes its stack slot.
%% Handler edges separately discard facts about registers at the throw site.
step(Op, [Dst | _], R, _, _, S) when
    Op =:= 'try'; Op =:= 'catch'; Op =:= try_end
->
    {erase_register(Dst, R), S};
step(catch_end, [Dst], R, _, _, S) ->
    {erase_register({?COMPACT_XREG, 0}, erase_register(Dst, R)), S};
step(Op, As, R, _, _, S) when Op =:= bs_start_match3; Op =:= bs_start_match4 ->
    {erase_register(lists:last(As), R), S};
step(bs_match, As, R, _, _, S) ->
    {lists:foldl(fun erase_register/2, R, operand_registers(As)), S};
step(init_yregs, [{list, Regs}], R, _, _, S) ->
    {lists:foldl(fun(Reg, Acc) -> put(Reg, {const, []}, Acc) end, R, Regs), S};
step(trim, [{?COMPACT_LITERAL, N}, {?COMPACT_LITERAL, _Remaining}], R, _, _, S) ->
    {
        maps:from_list([
            case K of
                {?COMPACT_YREG, I} -> {{?COMPACT_YREG, I - N}, V};
                _ -> {K, V}
            end
         || {K, V} <- maps:to_list(drop_origins(R)),
            case K of
                {?COMPACT_YREG, OldIndex} -> OldIndex >= N;
                _ -> true
            end
        ]),
        S
    };
step(move, [Src, Dst], R, _, D, S) ->
    {put_projection(Dst, Src, [], val(Src, R, D), R), S};
step(swap, [A, B], R, _, D, S) ->
    {(drop_origins(R))#{A => val(B, R, D), B => val(A, R, D)}, S};
step(return, [], R, MFA, _, S) ->
    {R, result(MFA, maps:get({?COMPACT_XREG, 0}, R, unknown), S)};
step(Op, [{?COMPACT_LITERAL, A}, {?COMPACT_LABEL, L} | _], R, MFA, D, S) when
    Op =:= call; Op =:= call_only; Op =:= call_last
->
    {F, Ar} = maps:get(L, maps:get(label_owner, D)),
    Target = {maps:get(module, D), F, Ar},
    S1 = reach_from(Target, xargs(A, R), MFA, S),
    V = return_value(Target, xargs(A, R), S1),
    {clobber(R, V), tail_result(Op, MFA, V, S1)};
step(Op, [{?COMPACT_LITERAL, A}, {?COMPACT_LITERAL, Idx} | _], R, MFA, D, S) when
    Op =:= call_ext; Op =:= call_ext_only; Op =:= call_ext_last
->
    Target = maps:get(Idx, maps:get(imports, D)),
    {V, S1} = external(Target, xargs(A, R), MFA, S),
    {clobber(R, V), tail_result(Op, MFA, V, S1)};
step(Op, [{?COMPACT_LITERAL, A} | _], R, MFA, _D, S) when Op =:= apply; Op =:= apply_last ->
    {V, S1} = dynamic(
        maps:get({?COMPACT_XREG, A}, R, unknown),
        maps:get({?COMPACT_XREG, A + 1}, R, unknown),
        xargs(A, R),
        MFA,
        S
    ),
    {clobber(R, V), tail_result(Op, MFA, V, S1)};
step(Op, [{?COMPACT_LITERAL, Idx} | Tail], R, MFA, D, S) when Op =:= make_fun2; Op =:= make_fun3 ->
    [Atom, A, _Label, _Index, Free, _] = maps:get(Idx, maps:get(funs, D)),
    Target = {maps:get(module, D), maps:get(Atom, maps:get(atoms, D)), A},
    {Dest, Captured} =
        case Tail of
            [Dst, {list, Caps}] -> {Dst, [val(C, R, D) || C <- Caps]};
            _ -> {{?COMPACT_XREG, 0}, xargs(Free, R)}
        end,
    %% The instruction stays in the residual code, so its target must be
    %% retained even when a captured value is bottom on this path.
    S1 = reach_from(
        Target,
        lists:duplicate(A - Free, unknown) ++
            [
                case C of
                    none -> unknown;
                    _ -> C
                end
             || C <- Captured
            ],
        MFA,
        S
    ),
    Cs = maps:get(closures, S1),
    Known = maps:get(Target, Cs, []),
    Vector = lists:nthtail(
        A - Free,
        relevant_args(
            Target,
            lists:duplicate(A - Free, unknown) ++
                [
                    case C of
                        none -> unknown;
                        _ -> C
                    end
                 || C <- Captured
                ],
            S1
        )
    ),
    %% Remember what each closure captured where it was built: a call through
    %% an unknown fun value runs it with those, not with arbitrary values.
    %% Past the bound they are joined once and for all, so this settles.
    Captures =
        case Known of
            {joined, J} ->
                {joined, join_args(J, Vector)};
            _ ->
                case lists:usort([Vector | Known]) of
                    Few when length(Few) =< ?SPINE_LIMIT -> Few;
                    Many -> {joined, join_contexts(Many)}
                end
        end,
    {put(Dest, {closure, Target}, R), S1#{closures => Cs#{Target => Captures}}};
step(call_fun, [{?COMPACT_LITERAL, A}], R, MFA, _D, S) ->
    fun_call(maps:get({?COMPACT_XREG, A}, R, unknown), A, R, MFA, S);
step(call_fun2, [{?COMPACT_LITERAL, Idx}, {?COMPACT_LITERAL, A}, Fun], R, MFA, D, S) ->
    case val(Fun, R, D) of
        Unbound when Unbound =:= unknown; Unbound =:= received ->
            [Atom, Arity, _, _, Free, _] = maps:get(Idx, maps:get(funs, D)),
            Target = {maps:get(module, D), maps:get(Atom, maps:get(atoms, D)), Arity},
            S1 = reach_from(Target, xargs(A, R) ++ lists:duplicate(Free, unknown), MFA, S),
            {
                clobber(R, return_value(Target, xargs(A, R) ++ lists:duplicate(Free, unknown), S1)),
                S1
            };
        V ->
            fun_call(V, A, R, MFA, S)
    end;
step(call_fun2, [_Tag, {?COMPACT_LITERAL, A}, Fun], R, MFA, D, S) ->
    fun_call(val(Fun, R, D), A, R, MFA, S);
step(send, [], R, _, _, S) ->
    V = maps:get({?COMPACT_XREG, 1}, R, unknown),
    {clobber(R, V), message(V, S)};
step(put_tuple2, [Dst, {list, As}], R, _, D, S) ->
    Vs = [val(A, R, D) || A <- As],
    V = construct(tuple, Vs),
    {put(Dst, V, R), built(V, S)};
step(put_list, [H, T, Dst], R, _, D, S) ->
    {put(Dst, construct(cons, [val(H, R, D), val(T, R, D)]), R), S};
step(loop_rec, [_Fail, Dst], R, MFA, _, S) ->
    %% A fresh receive is not correlated with an earlier message from the
    %% same loop/site. Forget old mailbox identities before introducing it.
    R0 = maps:map(
        fun(_, V) -> forget_mailbox(V) end,
        maps:filter(fun({T, _}, _) -> T =/= mailbox_shape end, R)
    ),
    {put(Dst, {mailbox, MFA, []}, R0), S};
step(recv_marker_reserve, [Dst], R, _, _, S) ->
    {erase_register(Dst, R), S};
step(Op, _As, R, _, _, S) when Op =:= wait; Op =:= wait_timeout ->
    {maps:filter(fun({T, _}, _) -> T =:= ?COMPACT_YREG end, R), S};
step(Op, _As, R, _, _, S) when
    Op =:= remove_message;
    Op =:= timeout;
    Op =:= loop_rec_end;
    Op =:= recv_marker_bind;
    Op =:= recv_marker_clear;
    Op =:= recv_marker_use
->
    {R, S};
step(update_record, [_Hint, {?COMPACT_LITERAL, Size}, Src, Dst, {list, Updates}], R, _, D, S) ->
    Fields = [tuple_get(I, val(Src, R, D)) || I <- lists:seq(0, Size - 1)],
    V = record_updates(Updates, construct(tuple, Fields), R, D),
    %% The inplace hint proves the source unique and dying. Other live
    %% registers do not alias it, and must keep their list/record facts.
    {put(Dst, V, R), S};
step(set_tuple_element, [Value, Dst, {?COMPACT_LITERAL, I}], R, _, D, S) ->
    %% The compiler emits this only for a fresh, uniquely owned tuple.
    %% In particular, unrelated registers holding list tails remain valid.
    Reg =
        case Dst of
            {typed, K, _} -> K;
            _ -> Dst
        end,
    V = tuple_set(I, val(Value, R, D), val(Dst, R, D)),
    {
        put(Reg, V, R),
        case I of
            0 -> built(V, S);
            _ -> S
        end
    };
step(get_tuple_element, [Src, {?COMPACT_LITERAL, I}, Dst], R, _, D, S) ->
    {put_projection(Dst, Src, [I], tuple_get(I, val(Src, R, D)), R), S};
step(get_list, [Src, H, T], R, _, D, S) ->
    V = val(Src, R, D),
    {put(T, list_tail(V), put(H, list_head(V), R)), S};
step(get_hd, [Src, Dst], R, _, D, S) ->
    {put(Dst, list_head(val(Src, R, D)), R), S};
step(get_tl, [Src, Dst], R, _, D, S) ->
    {put(Dst, list_tail(val(Src, R, D)), R), S};
step(Op, [_Fail, Src, Dst, _Live, {list, Pairs}], R, _, D, S) when
    Op =:= put_map_assoc; Op =:= put_map_exact
->
    Base = abstract_map(val(Src, R, D)),
    {put(Dst, map_puts(Pairs, Base, R, D), R), S};
step(get_map_elements, [_Fail, Src, {list, Pairs}], R, _, D, S) ->
    {map_gets(Pairs, val(Src, R, D), R, D), S};
step(Op, As, R, MFA, D, S) when
    Op =:= bif0; Op =:= bif1; Op =:= bif2; Op =:= gc_bif1; Op =:= gc_bif2; Op =:= gc_bif3
->
    {Idx, Args, Dst} = bif_args(Op, As),
    {V, S1} = external(maps:get(Idx, maps:get(imports, D)), [val(X, R, D) || X <- Args], MFA, S),
    Next =
        case {maps:get(Idx, maps:get(imports, D)), Args} of
            {{erlang, element, 2}, [Index, Src]} ->
                case val(Index, R, D) of
                    {const, N} when is_integer(N), N > 0 -> put_projection(Dst, Src, [N - 1], V, R);
                    _ -> put(Dst, V, R)
                end;
            _ ->
                put(Dst, V, R)
        end,
    {Next, S1};
step(Op, _As, R, _, _, S) when
    Op =:= label;
    Op =:= func_info;
    Op =:= line;
    Op =:= on_load;
    Op =:= allocate;
    Op =:= allocate_zero;
    Op =:= allocate_heap;
    Op =:= allocate_heap_zero;
    Op =:= deallocate;
    Op =:= test_heap;
    Op =:= jump;
    Op =:= trim
->
    %% trim shifts y registers: drop facts rather than leave stale positions.
    {
        case
            lists:member(Op, [
                trim, allocate, allocate_zero, allocate_heap, allocate_heap_zero, deallocate
            ])
        of
            true -> maps:filter(fun({T, _}, _) -> T =/= ?COMPACT_YREG end, drop_origins(R));
            false -> R
        end,
        S
    };
step(Op, As, R, _, _, S) ->
    %% Tests and selects only read registers. Binary instructions write only
    %% the registers they name, such as the match context of OTP 25's
    %% `bs_match_string'. Everything else loses facts.
    Name = atom_to_list(Op),
    case
        {
            lists:prefix("is_", Name) orelse
                lists:member(Op, [test_arity, select_val, select_tuple_arity]),
            lists:prefix("bs_", Name)
        }
    of
        {true, _} -> {R, S};
        {false, true} -> {lists:foldl(fun erase_register/2, R, operand_registers(As)), S};
        {false, false} -> {#{}, S}
    end.
bif_args(bif0, [{?COMPACT_LITERAL, I}, Dst]) ->
    {I, [], Dst};
bif_args(Op, [_Fail, _Live, {?COMPACT_LITERAL, I} | Tail]) when
    Op =:= gc_bif1; Op =:= gc_bif2; Op =:= gc_bif3
->
    {I, lists:droplast(Tail), lists:last(Tail)};
bif_args(_, [_Fail, {?COMPACT_LITERAL, I} | Tail]) ->
    {I, lists:droplast(Tail), lists:last(Tail)}.
map_gets([], _, R, _) ->
    R;
map_gets([Key, Dst | T], Map, R, D) ->
    map_gets(T, Map, put(Dst, abstract_map_get(val(Key, R, D), Map), R), D).
abstract_map_get(K, {choices, Vs}) ->
    project(fun(V) -> abstract_map_get(K, V) end, Vs);
abstract_map_get(_, none) ->
    none;
abstract_map_get({const, K}, {const, M}) when is_map(M) ->
    case maps:find(K, M) of
        {ok, V} -> {const, V};
        error -> unknown
    end;
abstract_map_get({const, K}, {map, M}) ->
    maps:get(K, M, unknown);
abstract_map_get(_, _) ->
    unknown.
abstract_map({const, M}) when is_map(M) -> {map, maps:map(fun(_, V) -> {const, V} end, M)};
abstract_map({map, _} = M) -> M;
abstract_map(_) -> unknown.
map_puts([], M, _, _) ->
    bounded(M, 12);
map_puts([K, V | Tail], {map, M}, R, D) ->
    case val(K, R, D) of
        {const, Key} -> map_puts(Tail, {map, M#{Key => val(V, R, D)}}, R, D);
        _ -> unknown
    end;
map_puts(_, _, _, _) ->
    unknown.
record_updates([], V, _, _) ->
    V;
record_updates([{?COMPACT_LITERAL, I}, Value | Rest], V, R, D) ->
    record_updates(Rest, tuple_set(I - 1, val(Value, R, D), V), R, D).
tuple_set(I, V, {choices, Ts}) ->
    project(fun(T) -> tuple_set(I, V, T) end, Ts);
tuple_set(I, V, {const, T}) when is_tuple(T) ->
    tuple_set(I, V, {tuple, [{const, X} || X <- tuple_to_list(T)]});
tuple_set(I, V, {tuple, Vs}) when I >= 0, I < length(Vs) ->
    {Before, [_ | After]} = lists:split(I, Vs),
    construct(tuple, Before ++ [V | After]);
tuple_set(_, _, _) ->
    unknown.
tuple_get(I, {mailbox, Id, Path}) -> {mailbox, Id, Path ++ [I]};
tuple_get(_, none) -> none;
tuple_get(I, {choices, Vs}) -> project(fun(V) -> tuple_get(I, V) end, Vs);
tuple_get(I, {const, T}) when is_tuple(T), tuple_size(T) > I -> {const, element(I + 1, T)};
tuple_get(I, {tuple, Vs}) when length(Vs) > I -> lists:nth(I + 1, Vs);
tuple_get(_, {const, _}) -> none;
tuple_get(_, _) -> unknown.
list_head(none) -> none;
list_head({const, []}) -> none;
list_head({sequence, E}) -> E;
list_head({choices, Vs}) -> project(fun list_head/1, Vs);
list_head({const, [H | _]}) -> {const, H};
list_head({cons, H, _}) -> H;
list_head(_) -> unknown.
list_tail(none) -> none;
list_tail({const, []}) -> none;
list_tail({sequence, _} = V) -> V;
list_tail({choices, Vs}) -> project(fun list_tail/1, Vs);
list_tail({const, [_ | T]}) -> {const, T};
list_tail({cons, _, T}) -> T;
list_tail(_) -> unknown.
construct(Kind, Vs) ->
    case lists:member(none, Vs) of
        true -> none;
        false -> construct_defined(Kind, Vs)
    end.
construct_defined(Kind, Vs) ->
    V =
        case lists:all(fun(X) -> is_tuple(X) andalso element(1, X) =:= const end, Vs) of
            true ->
                Values = [X || {const, X} <- Vs],
                case {Kind, Values} of
                    {tuple, _} -> {const, list_to_tuple(Values)};
                    {cons, [H, T]} -> {const, [H | T]}
                end;
            false ->
                case {Kind, Vs} of
                    {tuple, _} -> {tuple, Vs};
                    {cons, [H, T]} -> {cons, H, T}
                end
        end,
    sized(V, [12, 8, 5, 3, 2, 1]).
%% The depth bound alone lets a value built in a loop grow exponentially: a
%% tree whose every branch holds alternatives, as the compiler's
%% `#cg_cons{}' chains do. Past a size, keep fewer levels.
sized(V, [Depth | Depths]) ->
    B = bounded(V, Depth),
    case size_left(B, ?VALUE_SIZE) < 0 of
        true -> sized(V, Depths);
        false -> B
    end;
sized(_, []) ->
    unknown.
size_left(_, N) when N < 0 -> N;
size_left({tuple, Vs}, N) -> lists:foldl(fun size_left/2, N - 1, Vs);
size_left({choices, Vs}, N) -> lists:foldl(fun size_left/2, N - 1, Vs);
size_left({cons, H, T}, N) -> size_left(T, size_left(H, N - 1));
size_left({sequence, E}, N) -> size_left(E, N - 1);
size_left({map, M}, N) -> lists:foldl(fun size_left/2, N - 1, maps:values(M));
size_left(_, N) -> N - 1.
%% Bound structural depth so recursive container construction reaches a fixed
%% point. The cells of a proper list are siblings, not nesting: walking a
%% configuration list must not spend the depth budget the way entering a
%% nested container does. Spine length is bounded on its own instead.
bounded({closure, _} = V, _) ->
    V;
bounded({const, T} = V, _) when is_atom(T); is_number(T); is_binary(T); T =:= [] -> V;
bounded({choices, Vs}, N) ->
    project(fun(V) -> bounded(V, N) end, Vs);
bounded(_, 0) ->
    unknown;
bounded({tuple, Vs}, N) ->
    {tuple, [bounded(V, N - 1) || V <- Vs]};
bounded({sequence, E}, N) ->
    {sequence, bounded(E, N - 1)};
bounded({cons, H, T}, N) ->
    bound_spine(H, T, N, ?SPINE_LIMIT);
bounded({map, M}, N) ->
    {map, maps:map(fun(_, V) -> bounded(V, N - 1) end, M)};
bounded({const, T} = V, N) ->
    case term_depth(T, N, ?SPINE_LIMIT) of
        true -> V;
        false -> bound_constant(T, N)
    end;
bounded(V, _) ->
    V.
%% A longer spine than the bound keeps an element summary, so a list built in
%% a loop still reaches a fixed point.
bound_spine(H, T, N, 0) ->
    case list_elements({cons, H, T}) of
        {ok, E} -> {sequence, bounded(E, N - 1)};
        error -> unknown
    end;
bound_spine(H, {cons, H1, T1}, N, L) ->
    {cons, bounded(H, N - 1), bound_spine(H1, T1, N, L - 1)};
bound_spine(H, {const, [H1 | T1]}, N, L) ->
    {cons, bounded(H, N - 1), bound_spine({const, H1}, {const, T1}, N, L - 1)};
bound_spine(H, T, N, _) ->
    {cons, bounded(H, N - 1), bounded(T, N - 1)}.
%% Preserve shallow fields of deep literals instead of losing the entire term.
bound_constant([H | T], N) ->
    bound_spine({const, H}, {const, T}, N, ?SPINE_LIMIT);
bound_constant(T, N) when is_tuple(T) ->
    {tuple, [bounded({const, X}, N - 1) || X <- tuple_to_list(T)]};
bound_constant(M, N) when is_map(M) ->
    {map, maps:map(fun(_, V) -> bounded({const, V}, N - 1) end, M)};
bound_constant(_, _) ->
    unknown.
term_depth(_, 0, _) ->
    false;
term_depth([_ | _], _, 0) ->
    false;
term_depth([H | T], N, L) ->
    term_depth(H, N - 1, ?SPINE_LIMIT) andalso term_depth(T, N, L - 1);
term_depth(T, N, _) when is_tuple(T) ->
    lists:all(fun(X) -> term_depth(X, N - 1, ?SPINE_LIMIT) end, tuple_to_list(T));
term_depth(M, N, _) when is_map(M) ->
    lists:all(
        fun({K, V}) ->
            term_depth(K, N - 1, ?SPINE_LIMIT) andalso term_depth(V, N - 1, ?SPINE_LIMIT)
        end,
        maps:to_list(M)
    );
term_depth(_, _, _) ->
    true.
tail_result(Op, MFA, V, S) when
    Op =:= call_only;
    Op =:= call_last;
    Op =:= call_ext_only;
    Op =:= call_ext_last;
    Op =:= apply_last
->
    result(MFA, V, S);
tail_result(_, _, _, S) ->
    S.
fun_call({choices, Vs}, A, R, MFA, S) ->
    S1 = lists:foldl(
        fun(V, Acc) ->
            {_, Next} = fun_call(V, A, R, MFA, Acc),
            Next
        end,
        S,
        Vs
    ),
    {clobber(R, unknown), S1};
fun_call({closure, Target}, _A, R, MFA, S) ->
    {clobber(R, return_summary(Target, S)), edge(MFA, Target, S)};
fun_call({const, F}, _A, R, MFA, S) when is_function(F) ->
    {module, M} = erlang:fun_info(F, module),
    {name, N} = erlang:fun_info(F, name),
    {arity, A} = erlang:fun_info(F, arity),
    {V, S1} = external({M, N, A}, xargs(A, R), MFA, S),
    {clobber(R, V), S1};
fun_call({const, _}, _A, R, _, S) ->
    %% A known non-function raises badfun; it cannot hide a call target.
    %% Keep the return conservative because exception paths are analyzed
    %% separately and callers may catch the error.
    {clobber(R, unknown), S};
fun_call(none, _A, R, _, S) ->
    {clobber(R, none), S};
fun_call(_, A, R, MFA, S) ->
    S1 = warn(
        {closures, A}, MFA, dynamic_fun_known_arity, unresolved(MFA, S#{bounded_fallback => true})
    ),
    {clobber(R, unknown), reach_closures(A, R, MFA, S1)}.

%% A fun can only be a closure that analyzed code built, or an external fun
%% whose target was already retained where it was built. Bound the call by the
%% created closures that accept this many arguments.
reach_closures(A, R, MFA, S) ->
    Targets = [
        {T, Captured}
     || {{_, _, Total} = T, Captures} <- maps:to_list(maps:get(closures, S)),
        %% One context per closure: what it captured at any of its sites.
        Captured <- [join_contexts(alternatives(Captures))],
        A =:= any orelse Total - length(Captured) =:= A
    ],
    lists:foldl(
        fun({{_, _, Total} = T, Captured}, Acc) ->
            Args =
                case A of
                    any -> lists:duplicate(Total - length(Captured), unknown) ++ Captured;
                    _ -> xargs(A, R) ++ Captured
                end,
            reach_from(T, Args, MFA, Acc)
        end,
        S,
        Targets
    ).

external(Target, Args, Caller, S) ->
    case lists:member(none, Args) of
        true -> {none, S};
        false -> external_defined(Target, Args, Caller, S)
    end.
external_defined(Target, Args, Caller, S0) ->
    S = edge(Caller, Target, S0),
    case maps:is_key(Target, maps:get(nifs_index, S)) of
        true -> {unknown, S#{nifs => lists:usort([Target | maps:get(nifs, S)])}};
        false -> external_dispatch(Target, Args, Caller, S)
    end.
external_dispatch({lists, keyfind, 3}, [Key, {const, N}, L], _Caller, S) when
    is_integer(N), N > 0
->
    %% The native implementation returns an element of this list, never an
    %% arbitrary tuple. Preserve the record's callback/start fields.
    case list_elements(L) of
        {ok, E} -> {join({const, false}, keyfind_value(Key, N - 1, E)), S};
        error -> {unknown, S}
    end;
%% These return a list of the same elements, with one replaced, added or
%% removed. The loop inside them widens its accumulator, which loses the
%% element shape; the result's shape does not depend on that loop.
external_dispatch({lists, keydelete, 3} = Target, [_, _, L] = Args, Caller, S) ->
    key_list(Target, Args, L, none, Caller, S);
external_dispatch({lists, keyreplace, 4} = Target, [_, _, L, New] = Args, Caller, S) ->
    key_list(Target, Args, L, New, Caller, S);
external_dispatch({lists, keystore, 4} = Target, [_, _, L, New] = Args, Caller, S) ->
    key_list(Target, Args, L, New, Caller, S);
external_dispatch({lists, reverse, 1}, [L], _Caller, S) ->
    {reverse_list(L, {const, []}), S};
external_dispatch({lists, reverse, 2}, [L, T], _Caller, S) ->
    {reverse_list(L, T), S};
%% The answer is `{module, M}' for the module asked about, which is how an
%% Elixir protocol finds its implementation. It observes whether M exists, so
%% a named M stays in the output even when nothing else calls it.
external_dispatch({code, ensure_loaded, 1} = Target, [M] = Args, Caller, S) ->
    {Stub, S1} = external_code(Target, Args, Caller, S),
    Named = [Mod || {const, Mod} <- choices(M), is_atom(Mod)],
    case length(Named) =:= length(choices(M)) of
        true ->
            Present = S1#{
                present => lists:foldl(
                    fun(Mod, Acc) ->
                        case maps:find(Mod, maps:get(modules, S1)) of
                            {ok, D} when is_map_key({module_info, 0}, map_get(function_code, D)) ->
                                Acc#{Mod => true};
                            _ ->
                                Acc
                        end
                    end,
                    maps:get(present, S1),
                    Named
                )
            },
            Loaded = lists:foldl(
                fun(Mod, V) -> join(V, {tuple, [{const, module}, {const, Mod}]}) end,
                {tuple, [{const, error}, unknown]},
                Named
            ),
            {Loaded, Present};
        false ->
            {Stub, S1}
    end;
external_dispatch({atomvm, get_start_beam, 1} = Target, [{const, escript}] = Args, Caller, S) ->
    %% The escript pack is this one, and its start entry is the start module.
    {_, S1} = external_code(Target, Args, Caller, S),
    case maps:get(start_module, S) of
        undefined ->
            {unknown, S1};
        M ->
            Entry = <<(atom_to_binary(M, utf8))/binary, ".beam">>,
            {join({tuple, [{const, ok}, {const, Entry}]}, {const, {error, not_found}}), S1}
    end;
external_dispatch(
    {binary, part, 3} = Target, [{const, _}, {const, _}, {const, _}] = Args, Caller, S
) ->
    {_, S1} = external_code(Target, Args, Caller, S),
    V =
        try
            {const, apply(binary, part, [T || {const, T} <- Args])}
        catch
            _:_ -> none
        end,
    {V, S1};
external_dispatch({atomvm, get_boot, 0}, [], Caller, S) ->
    case maps:get(boot_data, S) of
        undefined -> {{const, undefined}, S};
        _ -> {unknown, fallback(package, Caller, binary_boot_script, S)}
    end;
external_dispatch({erlang, F, A} = Target, Args, Caller, S) ->
    %% Native dispatch wins over Erlang NIF stubs in reference/bundled libraries.
    Special = lists:member(F, [
        apply,
        spawn,
        spawn_link,
        spawn_monitor,
        spawn_opt,
        spawn_request,
        hibernate,
        open_port,
        make_fun,
        load_nif,
        nif_error
    ]),
    case Special orelse erlang:is_builtin(erlang, F, A) of
        true ->
            {V, S1} = native(erlang, F, A, Args, Caller, S),
            %% `is_builtin/3' describes the OTP running packbeam. AtomVM
            %% implements some of these in erlang.beam, and that body runs:
            %% keep it and what it calls. The native model still gives the
            %% value, which is what the call site reads.
            case erlang_implements(F, A, S) of
                true ->
                    {_, S2} = external_code(Target, Args, Caller, S1),
                    {V, S2};
                false ->
                    {V, S1}
            end;
        false ->
            case maps:is_key(erlang, maps:get(modules, S)) of
                true -> external_code(Target, Args, Caller, S);
                false -> {unknown, fallback(package, Caller, {missing_module, erlang}, S)}
            end
    end;
external_dispatch(Target, Args, Caller, S) ->
    external_code(Target, Args, Caller, S).
external_code({M, F, A} = Target, Args, Caller, S) ->
    case maps:find(M, maps:get(modules, S)) of
        {ok, D} when M =/= erlang ->
            case maps:is_key({F, A}, maps:get(function_code, D)) of
                %% A supplied module that does not define the function: the
                %% call raises undef, as with a module that is not supplied.
                false -> {none, S};
                true -> defined_code(Target, Args, D, S)
            end;
        {ok, D} ->
            defined_code(Target, Args, D, S);
        error ->
            native(M, F, A, Args, Caller, S)
    end.
defined_code({_, F, A} = Target, Args, D, S) ->
    S1 = reach(Target, Args, S),
    Value =
        case
            {lists:member({F, A}, maps:get(nif_stubs, D)), maps:is_key(Target, maps:get(args, S1))}
        of
            %% A stub describes no behaviour: the native implementation
            %% returns whatever it returns, whether or not the stub's own
            %% clauses accept these arguments.
            {true, _} -> unknown;
            {false, true} -> return_value(Target, Args, S1);
            {false, false} -> unknown
        end,
    {Value, S1}.
native(erlang, apply, 2, [Fun, Args], Caller, S) ->
    {A, R} =
        case list_args(Args) of
            As when is_list(As) ->
                {
                    length(As),
                    maps:from_list([
                        {{?COMPACT_XREG, I}, V}
                     || {I, V} <- lists:zip(lists:seq(0, length(As) - 1), As)
                    ])
                };
            _ ->
                {any, #{}}
        end,
    {_, S1} = fun_call(Fun, A, R, Caller, S),
    {unknown, S1};
native(erlang, hibernate, 3, [M, F, Args], Caller, S) ->
    dynamic(M, F, list_args(Args), Caller, S);
native(erlang, load_nif, 2, _, Caller, S) ->
    {unknown, fallback(package, Caller, native_callback_boundary, S)};
native(erlang, apply, 3, [M, F, Args], Caller, S) ->
    dynamic(M, F, list_args(Args), Caller, S);
native(erlang, F, A, Args, Caller, S) when
    F =:= spawn; F =:= spawn_link; F =:= spawn_monitor; F =:= spawn_opt; F =:= spawn_request
->
    TargetArgs =
        case {F, A, Args} of
            {spawn_opt, 4, _} -> Args;
            {spawn_request, 4, _} -> Args;
            {_, 4, [_Node | Rest]} -> Rest;
            {spawn_opt, 5, [_Node | Rest]} -> Rest;
            {spawn_request, 5, [_Node | Rest]} -> Rest;
            {_, 2, [_Node, RemoteFun]} when F =/= spawn_opt, F =/= spawn_request -> [RemoteFun];
            _ -> Args
        end,
    case TargetArgs of
        [M, N, As | _] ->
            {_, S1} = dynamic(M, N, list_args(As), Caller, S),
            {unknown, S1};
        [Fun | _] ->
            {_, S1} = fun_call(Fun, 0, #{}, Caller, S),
            {unknown, S1};
        _ ->
            {unknown, fallback(package, Caller, unknown_spawn_target, S)}
    end;
%% These raise: no value reaches the caller's continuation. A handler that
%% catches the exception starts from unknown registers, not from this value.
native(erlang, F, A, _Args, _Caller, S) when
    (F =:= error andalso A >= 1 andalso A =< 3);
    (F =:= exit andalso A =:= 1);
    (F =:= throw andalso A =:= 1);
    (F =:= raise andalso A =:= 3)
->
    {none, S};
native(erlang, setnode, _, _, _, S) ->
    %% Distributed peers are outside the supplied closed world.
    {unknown, escape_all(open_protocol(any, message_term(unknown, S)))};
native(erlang, F, _A, Args, _Caller, S) when
    F =:= list_to_tuple;
    F =:= make_tuple;
    F =:= append_element;
    F =:= setelement;
    F =:= binary_to_term
->
    %% Tuple-building BIFs, and decoding: the result's tag is whatever the
    %% data says.
    V = evaluate(F, Args),
    case {F, V} of
        {_, {const, T}} when is_tuple(T) ->
            {V, literal_protocol(T, S)};
        {setelement, _} when element(1, hd(Args)) =:= const, hd(Args) =/= {const, 1} ->
            {V, S};
        {binary_to_term, _} ->
            {V, escape_all(open_protocol(any, S))};
        _ ->
            {V, open_protocol(any, S)}
    end;
native(erlang, F, _A, Args, _Caller, S) when
    F =:= list_to_atom;
    F =:= binary_to_atom;
    F =:= list_to_existing_atom;
    F =:= binary_to_existing_atom
->
    case evaluate(F, Args) of
        {const, A} = V -> {V, escape_atoms([A], S)};
        V -> {V, escape_all(S)}
    end;
native(erlang, send, A, [_Dest, Msg | _], _Caller, S) when A =:= 2; A =:= 3 ->
    {Msg, message(Msg, S)};
native(erlang, send_after, A, [_Time, _Dest, Msg | _], _Caller, S) when A =:= 3; A =:= 4 ->
    {{type, reference}, message(Msg, S)};
native(erlang, start_timer, A, [_Time, _Dest, Msg | _], _Caller, S) when A =:= 3; A =:= 4 ->
    {{type, reference}, message(construct(tuple, [{const, timeout}, {type, reference}, Msg]), S)};
native(erlang, make_ref, 0, [], _Caller, S) ->
    {{type, reference}, S};
native(erlang, monitor, A, _, _Caller, S) when A =:= 2; A =:= 3 -> {{type, reference}, S};
native(erlang, self, 0, [], _Caller, S) ->
    {{type, pid}, S};
native(erlang, open_port, 2, [Name, _], Caller, S) ->
    case Name of
        {const, {spawn, Driver}} when is_list(Driver) ->
            S1 = S#{
                drivers => lists:usort([Driver | maps:get(drivers, S)]),
                native_ports => lists:usort([Driver | maps:get(native_ports, S)])
            },
            case
                lists:member(Driver, [
                    "echo", "console", "gpio", "i2c", "network", "socket", "spi", "uart", "usb_cdc"
                ])
            of
                true -> {unknown, S1};
                false -> {unknown, escape_all(open_protocol(any, message_term(unknown, S1)))}
            end;
        _ ->
            {unknown,
                warn(
                    drivers,
                    Caller,
                    unknown_driver,
                    escape_all(
                        open_protocol(any, message_term(unknown, S#{unknown_driver => true}))
                    )
                )}
    end;
native(erlang, make_fun, 3, [M, F, {const, A}], Caller, S) when is_integer(A), A >= 0 ->
    {_, S1} = dynamic(M, F, lists:duplicate(A, unknown), Caller, S),
    {unknown, S1};
native(erlang, make_fun, 3, [M, F, _A], Caller, S) ->
    %% Any arity: dispatched like an apply whose arguments are unknown.
    {_, S1} = dynamic(M, F, unknown, Caller, S),
    {unknown, S1};
native(erlang, function_exported, 3, [{const, M}, {const, F}, {const, A}], Caller, S) when
    is_atom(M), is_atom(F), is_integer(A), A >= 0
->
    %% Observing one export does not expose every entry point of the module.
    %% Keep its implementation to preserve the answer after rewriting; do not
    %% assume that an available module has already been loaded at runtime.
    case maps:find(M, maps:get(modules, S)) of
        {ok, D} ->
            case lists:member({F, A}, maps:get(exports, D)) of
                true ->
                    {_, Next} = external({M, F, A}, lists:duplicate(A, unknown), Caller, S),
                    {unknown, Next};
                false ->
                    case
                        maps:is_key({M, F, A}, maps:get(nifs_index, S)) orelse
                            (M =:= erlang andalso erlang:is_builtin(erlang, F, A))
                    of
                        true -> {unknown, S};
                        false -> {{const, false}, S}
                    end
            end;
        error ->
            %% Native exports may exist without supplied BEAM stubs.
            {unknown, S}
    end;
native(erlang, function_exported, 3, [M, {const, F}, {const, A}], Caller, S) when
    is_atom(F), is_integer(A), A >= 0
->
    {_, Next} = dynamic(M, {const, F}, lists:duplicate(A, unknown), Caller, S),
    {unknown, Next};
native(erlang, F, _A, [M | _], Caller, S) when F =:= get_module_info; F =:= function_exported ->
    {unknown, retain_module(M, Caller, module_introspection, S)};
native(erlang, process_flag, 2, [{const, error_handler}, M], Caller, S) ->
    {unknown, retain_module(M, Caller, dynamic_error_handler, S)};
native(erlang, load_module, 2, _, Caller, S) ->
    {unknown, fallback(package, Caller, dynamic_code_loading, S)};
native(erlang, F, _A, Args, Caller, S) ->
    V = evaluate(F, Args),
    case V of
        {const, Term} -> {V, term_roots(Term, Caller, S)};
        _ -> {V, S}
    end;
native(math, _F, _A, _Args, _Caller, S) ->
    {unknown, S};
native(persistent_term, F, _A, _Args, _Caller, S) when
    F =:= get; F =:= put; F =:= erase; F =:= info
->
    {unknown, S};
native(M, _F, _A, Args, Caller, #{open_world := true} = S) ->
    {unknown, unanalyzed_call(M, Args, Caller, S)};
native(M, _F, _A, _Args, Caller, S) ->
    %% Closed world: a module with neither a BEAM nor a registered native
    %% implementation cannot be called. The call raises undef, so nothing is
    %% retained for it, and the absence is reported.
    {none, absent_module(M, Caller, S)}.
absent_module(M, Caller, #{open_world := true} = S) ->
    unanalyzed_call(M, [unknown], Caller, S);
absent_module(M, Caller, S) ->
    %% Often a merged context: values of different calls that no single
    %% call passes.
    warn({absent, M}, Caller, {missing_module, M}, unresolved(Caller, S)).

%% Open world: the library installed on the device runs this call. It
%% returns, and it may call the closures and packaged modules it is given,
%% open ports, or send any message. One warning per module is enough.
unanalyzed_call(M, Args, Caller, S0) ->
    S1 =
        case
            lists:any(
                fun(#{scope := Scope}) -> Scope =:= {unanalyzed, M} end, maps:get(warnings, S0)
            )
        of
            true -> S0;
            false -> warn({unanalyzed, M}, Caller, {unanalyzed_module, M}, S0)
        end,
    S = escape_all(
        open_protocol(any, message_term(unknown, unresolved(Caller, S1#{unknown_driver => true})))
    ),
    lists:foldl(fun(V, Acc) -> escape_value(V, Caller, Acc) end, S, Args).

%% What unanalyzed code can call from a value it is given.
escape_value(none, _Caller, S) ->
    S;
escape_value({type, _}, _Caller, S) ->
    S;
escape_value({const, T}, Caller, S) ->
    Modules = maps:get(modules, S),
    lists:foldl(
        fun(M, Acc) -> retain_exports(M, maps:get(M, Modules), Caller, Acc) end,
        term_roots(T, Caller, S),
        [M || M <- term_atoms(T), maps:is_key(M, Modules)]
    );
escape_value({closure, _} = V, Caller, S) ->
    {_, S1} = fun_call(V, any, #{}, Caller, S),
    S1;
escape_value({Shape, Vs}, Caller, S) when Shape =:= tuple; Shape =:= choices ->
    lists:foldl(fun(V, Acc) -> escape_value(V, Caller, Acc) end, S, Vs);
escape_value({map, M}, Caller, S) ->
    lists:foldl(fun(V, Acc) -> escape_value(V, Caller, Acc) end, S, maps:values(M));
escape_value({cons, H, T}, Caller, S) ->
    escape_value(T, Caller, escape_value(H, Caller, S));
escape_value({sequence, E}, Caller, S) ->
    escape_value(E, Caller, S);
escape_value(_, Caller, S) ->
    %% Any closure the analyzed code built, or any module it names.
    reach_closures(any, #{}, Caller, S#{
        all_named => true, unknown_driver => true, bounded_fallback => true
    }).
retain_module({const, M}, Caller, Reason, S) when is_atom(M) ->
    fallback({module, M}, Caller, Reason, S);
retain_module({choices, Vs}, Caller, Reason, S) ->
    lists:foldl(fun(V, Acc) -> retain_module(V, Caller, Reason, Acc) end, S, Vs);
retain_module(none, _, _, S) ->
    S;
retain_module(_, Caller, Reason, S) ->
    fallback(package, Caller, Reason, S).

%% AtomVM implements lists:reverse/1,2 natively; analyzing the nif_error
%% stub would discard exactly the formatter closures we need to retain.
keyfind_value(Key, I, {choices, Vs}) ->
    project(fun(V) -> keyfind_value(Key, I, V) end, Vs);
keyfind_value(_, _, none) ->
    none;
keyfind_value(Key, I, V) ->
    case tuple_arity(V) of
        none ->
            none;
        {const, N} when I >= N -> none;
        _ ->
            case {Key, tuple_get(I, V)} of
                {{const, K}, {const, E}} when K /= E -> none;
                _ -> V
            end
    end.

append_list(none, _) ->
    none;
append_list(_, none) ->
    none;
append_list({const, []}, B) ->
    B;
append_list({choices, Vs}, B) ->
    project(fun(V) -> append_list(V, B) end, Vs);
append_list({const, [H | T]}, B) ->
    construct(cons, [{const, H}, append_list({const, T}, B)]);
append_list({cons, H, T}, B) ->
    construct(cons, [H, append_list(T, B)]);
append_list(A, B) ->
    case {list_elements(A), list_elements(B)} of
        {{ok, EA}, {ok, EB}} -> {sequence, join(EA, EB)};
        _ -> unknown
    end.

reverse_list(none, _) ->
    none;
reverse_list({choices, Vs}, T) ->
    project(fun(V) -> reverse_list(V, T) end, Vs);
reverse_list({const, L}, T) when is_list(L) ->
    lists:foldl(fun(X, A) -> construct(cons, [{const, X}, A]) end, T, L);
reverse_list({cons, H, Rest}, T) ->
    reverse_list(Rest, construct(cons, [H, T]));
reverse_list({sequence, E}, T) ->
    case list_elements(T) of
        {ok, TE} -> {sequence, join(E, TE)};
        error -> unknown
    end;
reverse_list(_, _) ->
    unknown.

list_args({choices, Vs}) ->
    merge_arg_alternatives([list_args(V) || V <- Vs]);
list_args({const, L}) when is_list(L) -> [{const, X} || X <- L];
list_args({cons, H, T}) ->
    case list_args(T) of
        {alternatives, As} -> {alternatives, [[H | A] || A <- As]};
        unknown -> unknown;
        A -> [H | A]
    end;
list_args(_) ->
    unknown.
merge_arg_alternatives(As) ->
    case lists:member(unknown, As) of
        true ->
            unknown;
        false ->
            {alternatives,
                lists:usort(
                    lists:append([
                        case A of
                            {alternatives, Vs} -> Vs;
                            _ -> [A]
                        end
                     || A <- As
                    ])
                )}
    end.
dynamic(M, F, Args, Caller, S) ->
    Key = {Caller, maps:get(instruction, S, 0)},
    A =
        case Args of
            Vs when is_list(Vs) -> {const, length(Vs)};
            {alternatives, As} -> project(fun(Vs) -> {const, length(Vs)} end, As);
            _ -> unknown
        end,
    Targets =
        case {M, F, A} of
            {{const, Mod}, {const, Name}, {const, Ar}} when is_atom(Mod), is_atom(Name) ->
                [{Mod, Name, Ar}];
            {unknown, {const, Name}, {const, Ar}} when is_atom(Name) ->
                signature_targets(Name, Ar, S);
            _ ->
                []
        end,
    Sites = maps:get(dispatches, S),
    Old = maps:get(Key, Sites, #{modules => none, functions => none, arities => none, targets => []}),
    Site = #{
        modules => join(M, maps:get(modules, Old)),
        functions => join(F, maps:get(functions, Old)),
        arities => join(A, maps:get(arities, Old)),
        targets => lists:usort(Targets ++ maps:get(targets, Old))
    },
    dynamic_value(M, F, Args, Caller, S#{dispatches => Sites#{Key => Site}}).
dynamic_value(M, F, {alternatives, As}, Caller, S) ->
    lists:foldl(
        fun(A, {V, Acc}) ->
            {W, Next} = dynamic(M, F, A, Caller, Acc),
            {join(V, W), Next}
        end,
        {none, S},
        As
    );
dynamic_value({choices, Vs}, F, Args, Caller, S) ->
    lists:foldl(
        fun(M, {V, Acc}) ->
            {W, Next} = dynamic(M, F, Args, Caller, Acc),
            {join(V, W), Next}
        end,
        {none, S},
        Vs
    );
dynamic_value(M, {choices, Vs}, Args, Caller, S) ->
    lists:foldl(
        fun(F, {V, Acc}) ->
            {W, Next} = dynamic(M, F, Args, Caller, Acc),
            {join(V, W), Next}
        end,
        {none, S},
        Vs
    );
dynamic_value(none, _, _, _, S) ->
    {none, S};
dynamic_value(_, none, _, _, S) ->
    {none, S};
dynamic_value({const, M}, {const, F}, Args, Caller, S) when is_atom(M), is_atom(F), is_list(Args) ->
    external({M, F, length(Args)}, Args, Caller, S);
dynamic_value({const, M}, {const, F}, _, Caller, S) when is_atom(M), is_atom(F) ->
    {unknown, fallback({function, M, F}, Caller, dynamic_arity, S)};
dynamic_value({const, M}, _, _, Caller, S) when is_atom(M) ->
    {unknown, fallback({module, M}, Caller, dynamic_function, S)};
dynamic_value(_, {const, F}, Args, Caller, S) when is_atom(F), is_list(Args) ->
    signature_call(F, Args, Caller, S);
dynamic_value(_, _, _, Caller, S) ->
    {unknown, fallback(named_modules, Caller, dynamic_module, S)}.
%% Unknown receiver, known function/arity: the closed-world export tables
%% still bound the possible calls. Native targets must participate even when
%% their Erlang stubs were not supplied (notably erlang:apply and spawn).
signature_targets(F, A, S) ->
    BeamTargets = [
        {M, F, A}
     || M <- maps:get({F, A}, maps:get(exported_by, S), []), named_module(M, S)
    ],
    Nifs = [MFA || MFA = {_, N, Ar} <- maps:keys(maps:get(nifs_index, S)), N =:= F, Ar =:= A],
    Builtins =
        case
            erlang:is_builtin(erlang, F, A) orelse
                lists:member({F, A}, [
                    {apply, 2},
                    {apply, 3},
                    {spawn, 1},
                    {spawn, 2},
                    {spawn, 3},
                    {spawn, 4},
                    {spawn_link, 1},
                    {spawn_link, 2},
                    {spawn_link, 3},
                    {spawn_link, 4},
                    {spawn_monitor, 1},
                    {spawn_monitor, 3},
                    {spawn_opt, 2},
                    {spawn_opt, 3},
                    {spawn_opt, 4},
                    {spawn_opt, 5},
                    {hibernate, 3},
                    {make_fun, 3},
                    {load_nif, 2},
                    {nif_error, 1},
                    {nif_error, 2}
                ])
        of
            true -> [{erlang, F, A}];
            false -> []
        end,
    lists:usort(BeamTargets ++ Nifs ++ Builtins).
erlang_implements(F, A, S) ->
    case maps:find(erlang, maps:get(modules, S)) of
        {ok, D} ->
            lists:member({F, A}, maps:get(exports, D)) andalso
                not lists:member({F, A}, maps:get(nif_stubs, D));
        error ->
            false
    end.

%% Keep the implementation reachable: unlike `keyfind/3' these are Erlang
%% functions that still run.
key_list(Target, Args, L, Extra, Caller, S) ->
    {_, S1} = external_code(Target, Args, Caller, S),
    case list_elements(L) of
        {ok, E} -> {{sequence, join(E, Extra)}, S1};
        error -> {unknown, S1}
    end.

signature_call(F, Args, Caller, S0) ->
    A = length(Args),
    S = unresolved(Caller, S0),
    Targets = signature_targets(F, A, S),
    %% The function and arity bound the targets inside the closed world, so
    %% this dispatch needs no diagnostic. An empty candidate set does: the
    %% analysis then believes the call cannot reach any packaged module.
    S1 =
        case Targets of
            [] -> warn({signature, F, A}, Caller, unresolved_callback_module, S);
            _ -> S
        end,
    %% The candidates bound the result too. A native target has no analyzable
    %% body and answers `unknown', which widens the join on its own.
    {V, S2} = lists:foldl(
        fun(Target, {Acc, St}) ->
            {W, Next} = external(Target, Args, Caller, St),
            {join(Acc, W), Next}
        end,
        {none, S1},
        Targets
    ),
    case Targets of
        [] -> {unknown, S2};
        _ -> {V, S2}
    end.
%% `++' and `--' build a list out of their arguments: the elements of both
%% sides survive, which keeps records carried in a list together with their
%% callback fields.
evaluate('++', [A, B]) ->
    append_list(A, B);
evaluate('--', [A, _]) ->
    case list_elements(A) of
        {ok, E} -> {sequence, E};
        error -> unknown
    end;
evaluate(setelement, [{const, I}, T, V]) when is_integer(I), I > 0 -> tuple_set(I - 1, V, T);
evaluate(element, [{const, I}, T]) when is_integer(I), I > 0 -> tuple_get(I - 1, T);
evaluate(hd, [L]) ->
    list_head(L);
evaluate(tl, [L]) ->
    list_tail(L);
evaluate(map_get, [K, M]) ->
    abstract_map_get(K, M);
evaluate(F, Args) ->
    %% Deliberately small pure whitelist: never execute arbitrary input code.
    case
        lists:member(F, [
            element,
            hd,
            tl,
            map_get,
            length,
            tuple_size,
            byte_size,
            '-',
            atom_to_list,
            list_to_atom,
            binary_to_atom,
            binary_to_existing_atom,
            list_to_existing_atom,
            binary_to_term
        ]) andalso
            lists:all(fun(V) -> is_tuple(V) andalso element(1, V) =:= const end, Args)
    of
        true ->
            try
                {const, apply(erlang, F, [V || {const, V} <- Args])}
            catch
                _:_ -> unknown
            end;
        false ->
            unknown
    end.
fallback(Scope, Caller, Reason, S) ->
    fallback_scope(Scope, Caller, Reason, unresolved(Caller, S)).
unresolved(Caller, S) ->
    Unresolved = maps:get(unresolved, S),
    case maps:is_key(Caller, Unresolved) of
        true -> S;
        false -> S#{unresolved => Unresolved#{Caller => true}}
    end.
fallback_scope(_, _, _, #{all := true} = S) ->
    S;
fallback_scope(package, _, _, #{pending_package := true} = S) ->
    S;
fallback_scope(package, Caller, Reason, S) ->
    %% Finish known paths before deciding whether the output can be trimmed.
    warn(package, Caller, Reason, S#{pending_package => true, unknown_driver => true});
fallback_scope(named_modules, _Caller, _Reason, #{all_named := true} = S) ->
    S;
fallback_scope(named_modules, Caller, Reason, S) ->
    %% The module is unknown, but a module can only be selected by a name that
    %% the analyzed code produces. Every module the code names is retained
    %% whole, along with everything those modules can reach; native targets
    %% stay unbounded, so driver advice is suppressed.
    warn(named_modules, Caller, Reason, S#{
        all_named => true, unknown_driver => true, bounded_fallback => true
    });
fallback_scope({function, M, F} = Scope, Caller, Reason, S) ->
    S1 = warn(Scope, Caller, Reason, S),
    case maps:find(M, maps:get(modules, S)) of
        error ->
            absent_module(M, Caller, S1);
        {ok, D} ->
            lists:foldl(
                fun({_, A}, Acc) ->
                    %% Use native dispatch too, so driver usage is still recorded.
                    {_, Next} = external({M, F, A}, lists:duplicate(A, unknown), Caller, Acc),
                    Next
                end,
                S1,
                [FA || FA = {N, _} <- maps:get(exports, D), N =:= F]
            )
    end;
fallback_scope({module, M} = Scope, Caller, Reason, S) ->
    S1 = warn(Scope, Caller, Reason, S),
    case maps:find(M, maps:get(modules, S)) of
        error ->
            absent_module(M, Caller, S1);
        {ok, D} ->
            retain_exports(M, D, Caller, S1)
    end.
retain_exports(M, D, Caller, S) ->
    lists:foldl(
        fun({F, A}, Acc) -> reach_from({M, F, A}, lists:duplicate(A, unknown), Caller, Acc) end,
        S,
        maps:get(exports, D)
    ).
reach_from(Target, Args, Caller, S) -> reach(Target, Args, edge(Caller, Target, S)).
%% Each caller's callees are a set: a call through an unknown fun reaches
%% every closure of its arity, thousands in a large program.
edge(Caller, Target, S) ->
    Edges = maps:get(edges, S),
    Targets = maps:get(Caller, Edges, #{}),
    case maps:is_key(Target, Targets) of
        true ->
            S;
        false ->
            Callers = maps:get(callers, S),
            S#{
                edges => Edges#{Caller => Targets#{Target => true}},
                callers => Callers#{Target => (maps:get(Target, Callers, #{}))#{Caller => true}}
            }
    end.
edge_lists(Edges) ->
    maps:map(fun(_, Targets) -> lists:sort(maps:keys(Targets)) end, Edges).
warning_context(#{caller := {M, _, _} = Caller} = W, Parents, References, S) ->
    Origin =
        case lists:any(fun(B) -> beam_module(B) =:= M end, References) of
            true -> reference;
            false -> output
        end,
    W1 = W#{
        origin => Origin,
        path => warning_path(Caller, Parents, [])
    },
    case maps:get(scope, W) of
        Scope when Scope =:= package; Scope =:= named_modules ->
            W1#{
                opaque_message_arities => maps:get(opaque_arities, S),
                opaque_senders => maps:get(opaque_senders, S)
            };
        {signature, F, A} ->
            W1#{targets => signature_targets(F, A, S)};
        _ ->
            W1
    end.
%% Breadth-first traversal gives one short explanation, including reachability
%% edges for callbacks, on_load hooks and conservative exception paths: each
%% function reached keeps the caller it was first reached from.
shortest_parents(Roots, Edges) ->
    shortest_parents(
        queue:from_list(Roots), maps:from_list([{R, root} || R <- lists:reverse(Roots)]), Edges
    ).
shortest_parents(Queue, Parents, Edges) ->
    case queue:out(Queue) of
        {empty, _} ->
            Parents;
        {{value, MFA}, Rest} ->
            New = [
                T
             || T <- lists:sort(maps:keys(maps:get(MFA, Edges, #{}))), not maps:is_key(T, Parents)
            ],
            shortest_parents(
                lists:foldl(fun queue:in/2, Rest, New),
                lists:foldl(fun(T, Acc) -> Acc#{T => MFA} end, Parents, New),
                Edges
            )
    end.
warning_path(MFA, Parents, Acc) ->
    case maps:find(MFA, Parents) of
        error -> [];
        {ok, root} -> [MFA | Acc];
        {ok, Parent} -> warning_path(Parent, Parents, [MFA | Acc])
    end.
warn(Scope, Caller, Reason, S) ->
    W = #{scope => Scope, caller => Caller, reason => Reason},
    Warnings = maps:get(warnings, S),
    case lists:member(W, Warnings) of
        true -> S;
        false -> S#{warnings => Warnings ++ [W]}
    end.
%% @doc The text the command line prints for one warning of `run/4'.
-spec format_warning(map()) -> io_lib:chars().
format_warning(#{scope := Scope, caller := {M, F, A}, reason := Reason} = Warning) ->
    Effect =
        case Scope of
            package ->
                "ALL output BEAMs are retained without function trimming";
            {signature, Name, Arity} ->
                io_lib:format(
                    "no ~p/~p implementation is retained for this call site",
                    [Name, Arity]
                );
            {function, Mod, Name} ->
                io_lib:format(
                    "all exported arities of ~p:~p and their dependencies are retained; other functions can still be trimmed",
                    [Mod, Name]
                );
            {module, Mod} ->
                io_lib:format(
                    "all exports of ~p and their dependencies are retained; other modules can still be trimmed",
                    [Mod]
                );
            named_modules ->
                "all exports of every packaged module that the analyzed code names, and their dependencies, are retained; modules no reachable code names can still be trimmed";
            {absent, Mod} ->
                io_lib:format(
                    "the call raises undef, so code reachable only through ~p is not retained",
                    [Mod]
                );
            {unanalyzed, Mod} ->
                io_lib:format(
                    "~p is expected on the device; the closures and modules passed to it are retained, and the messages it may send are assumed to be anything",
                    [Mod]
                );
            {closures, Arity} ->
                io_lib:format(
                    "every closure built by the analyzed code that takes ~p argument(s), and their dependencies, are retained; other functions can still be trimmed",
                    [Arity]
                );
            drivers ->
                "driver removal suggestions are suppressed";
            external ->
                "external implementation could not be analyzed"
        end,
    Advice =
        case Reason of
            {missing_module, Missing} ->
                io_lib:format("Supply ~p's implementation with --reference or -e/--external.", [
                    Missing
                ]);
            {unanalyzed_module, _} ->
                "Pass the AtomVM library with --reference for more precise pruning.";
            unknown_driver ->
                "Use a constant open_port driver name for driver analysis.";
            dynamic_fun_known_arity ->
                "The fun value is unknown, but it can only be a closure that the analyzed code built with that number of arguments.";
            unknown_fun ->
                "The analyzer could not bound a fun/closure target, including its flow through containers or native calls. --keep adds roots but cannot constrain this target.";
            binary_boot_script ->
                "The packaged binary boot script has not been analyzed for executable targets.";
            module_introspection ->
                "Module introspection requires retaining the module's exports.";
            unresolved_callback_module ->
                "The module is unknown and no packaged module exporting that function is named by the analyzed code, so the analyzer found no possible target. Name the callback module in code, or add it with --keep.";
            dynamic_module ->
                "The analyzer could not bound the module target; runtime input or lost value information can cause this. --keep adds roots but cannot constrain this target.";
            dynamic_arity ->
                "The module and function are known, but the argument-list length is not bounded.";
            dynamic_function ->
                "The module is known, but the function name is not bounded. Use an explicit set of function targets.";
            unknown_spawn_target ->
                "The analyzer could not bound the spawned process's function or closure target.";
            serialized_local_fun ->
                "A serialized local closure refers to code that cannot be safely relocated by this analysis.";
            native_callback_boundary ->
                "Loaded native code may invoke callbacks that are unavailable to the BEAM analysis.";
            dynamic_code_loading ->
                "Dynamically loaded code may invoke functions outside the analyzed call graph.";
            dynamic_error_handler ->
                "The configured error-handler module may receive runtime callbacks.";
            _ ->
                "The analyzer could not bound this execution boundary. --keep adds roots but cannot constrain unknown targets."
        end,
    PathText =
        case maps:get(path, Warning, []) of
            [] ->
                [];
            Path ->
                io_lib:format(
                    "~n  Reachability path (includes callbacks and exception paths):~n    ~s", [
                        lists:join("\n    -> ", [
                            io_lib:format("~p:~p/~p", [PM, PF, PA])
                         || {PM, PF, PA} <- Path
                        ])
                    ]
                )
        end,
    OriginText =
        case maps:get(origin, Warning, undefined) of
            reference -> " [reference library]";
            output -> " [output module]";
            _ -> ""
        end,
    MailboxText =
        case maps:get(opaque_message_arities, Warning, []) of
            [] ->
                [];
            Arities ->
                io_lib:format(
                    "~n  Opaque mailbox sends:~n    Control-message bounds disabled for tuple arities: ~w~n    (any means unknown shape)~n    Senders:~n      ~s~n    These are analysis-wide precision losses, not a proven data-flow path~n    to this call.",
                    [
                        Arities,
                        lists:join("\n      ", [
                            io_lib:format("~p:~p/~p", [SM, SF, SA])
                         || {SM, SF, SA} <- lists:sort(
                                maps:keys(maps:get(opaque_senders, Warning, #{}))
                            )
                        ])
                    ]
                )
        end,
    io_lib:format("~p:~p/~p~s~n  Reason: ~p~n~s~n~s~s~s", [
        M,
        F,
        A,
        OriginText,
        Reason,
        warning_prose(["Retention: ", Effect, "."]),
        warning_prose(Advice),
        PathText,
        MailboxText
    ]).

warning_prose(Text) ->
    Words = string:lexemes(lists:flatten(Text), " "),
    {Lines, Last} = lists:foldl(
        fun(Word, {Done, Line}) ->
            case length(Line) + 1 + length(Word) > 88 of
                true -> {[Line | Done], "    " ++ Word};
                false when Line =:= "  " -> {Done, Line ++ Word};
                false -> {Done, Line ++ " " ++ Word}
            end
        end,
        {[], "  "},
        Words
    ),
    lists:join("\n", lists:reverse([Last | Lines])).

%% @doc Build options that would disable a driver the pruned code never uses,
%% or none when the analysis could not see every driver use.
-spec driver_suggestions(map()) -> [io_lib:chars()].
driver_suggestions(#{driver_analysis_complete := false}) ->
    [];
driver_suggestions(#{drivers := Ports} = Report) ->
    packbeam_drivers:suggestions(Ports, maps:get(nifs, Report, [])).

literal_roots(As, Caller, D, S) when is_list(As) ->
    lists:foldl(fun(A, Acc) -> literal_roots(A, Caller, D, Acc) end, S, As);
literal_roots({literal, N}, Caller, D, S) ->
    Term = binary_to_term(maps:get(N, maps:get(literals, D))),
    Node = {literal, maps:get(module, D), N},
    Ls = maps:get(literals, S),
    Uses = maps:get(literal_edges, S),
    Next = S#{
        literals => Ls#{Node => #{value => Term, mfa_candidates => literal_mfas(Term)}},
        literal_edges => Uses#{Caller => lists:usort([Node | maps:get(Caller, Uses, [])])}
    },
    term_roots(Term, Caller, literal_protocol(Term, surface_atoms(term_atoms(Term), D, Next)));
literal_roots({list, As}, Caller, D, S) ->
    literal_roots(As, Caller, D, S);
literal_roots({typed, A, _}, Caller, D, S) ->
    literal_roots(A, Caller, D, S);
literal_roots({?COMPACT_ATOM, N}, _Caller, D, S) when N > 0 ->
    surface_atoms([maps:get(N, maps:get(atoms, D))], D, S);
literal_roots(_, _, _, S) ->
    S.

%% A module can only be selected as a dynamic target by a name that the
%% analyzed code itself produces. Record where each packaged module name is
%% mentioned; a module never justifies its own retention.
surface_atoms(Atoms, _D, S) ->
    Sf = maps:get(surfaced, S),
    case [A || A <- Atoms, not maps:is_key(A, Sf)] of
        [] -> S;
        New -> S#{surfaced => maps:merge(Sf, maps:from_keys(New, true))}
    end.
term_atoms(Term) -> lists:usort(term_atoms(Term, [])).
term_atoms(A, Acc) when is_atom(A) -> [A | Acc];
term_atoms(T, Acc) when is_tuple(T) -> term_atoms(tuple_to_list(T), Acc);
term_atoms(M, Acc) when is_map(M) -> term_atoms(maps:to_list(M), Acc);
term_atoms([H | T], Acc) -> term_atoms(T, term_atoms(H, Acc));
term_atoms(_, Acc) -> Acc.
%% Modules retained whole because an unbounded dispatch could select them: the
%% modules the analyzed code names, closed over the atoms of those modules,
%% since a retained module's own literals can name the next one.
named_closure(#{all_named := false}) ->
    [];
named_closure(S) ->
    Ms = maps:get(modules, S),
    Seed = [M || M <- maps:keys(Ms), named_module(M, S)],
    close_named(lists:usort(Seed), [], Ms).
close_named([], Acc, _) ->
    lists:usort(Acc);
close_named([M | Rest], Acc, Ms) ->
    case lists:member(M, Acc) of
        true ->
            close_named(Rest, Acc, Ms);
        false ->
            D = maps:get(M, Ms),
            Atoms =
                maps:values(maps:get(atoms, D)) ++
                    lists:append([
                        term_atoms(binary_to_term(L))
                     || L <- maps:values(maps:get(literals, D))
                    ]),
            Next = [A || A <- Atoms, maps:is_key(A, Ms)],
            close_named(Rest ++ Next, [M | Acc], Ms)
    end.

%% The exports an unresolved call bounded to the named modules may run: those
%% whose name the program has.
retained(Named, S) ->
    Modules = maps:get(modules, S),
    Dispatchable = scanned_atoms(Named, Modules, maps:get(surfaced, S)),
    [
        {M, F, A}
     || M <- Named,
        {F, A} <- maps:get(exports, maps:get(M, Modules)),
        maps:is_key(F, Dispatchable)
    ].

%% Such a call may pass any arguments. Analyze the exports it may run with
%% unknown ones, so what they call has contexts for what they pass it, until
%% the set of exports stops growing.
retain(#{all := true} = S, _) ->
    S;
retain(S, Done) ->
    case [MFA || MFA <- retained(named_closure(S), S), not maps:is_key(MFA, Done)] of
        [] ->
            S;
        New ->
            Reached = lists:foldl(
                fun({_, _, A} = MFA, Acc) -> reach(MFA, lists:duplicate(A, unknown), Acc) end,
                S,
                New
            ),
            retain(fixpoint(Reached), maps:merge(Done, maps:from_keys(New, true)))
    end.

%% Functions retained without being analyzed keep everything they call:
%% locally, through imports, and the lambdas they build.
unanalyzed_closure(Retained, Analyzed, Modules) ->
    unanalyzed_closure(Retained, Analyzed, Modules, #{}).
unanalyzed_closure([], _Analyzed, _Modules, Seen) ->
    maps:keys(Seen);
unanalyzed_closure([MFA | Rest], Analyzed, Modules, Seen) ->
    case maps:is_key(MFA, Seen) orelse maps:is_key(MFA, Analyzed) of
        true ->
            unanalyzed_closure(Rest, Analyzed, Modules, Seen);
        false ->
            unanalyzed_closure(
                static_callees(MFA, Modules) ++ Rest, Analyzed, Modules, Seen#{MFA => true}
            )
    end.
static_callees({M, F, A}, Modules) ->
    case maps:find(M, Modules) of
        error ->
            [];
        {ok, D} ->
            case maps:find({F, A}, maps:get(function_code, D)) of
                error ->
                    [];
                {ok, {Is, _}} ->
                    lists:usort(
                        lists:append([
                            instruction_callees(Op, As, D, Modules) ++ literal_callees(As, D)
                         || {_, Op, As} <- tuple_to_list(Is)
                        ])
                    )
            end
    end.
instruction_callees(Op, [_, {?COMPACT_LABEL, L} | _], D, _) when
    Op =:= call; Op =:= call_only; Op =:= call_last
->
    {F, A} = maps:get(L, maps:get(label_owner, D)),
    [{maps:get(module, D), F, A}];
instruction_callees(Op, [{?COMPACT_LITERAL, Idx} | _], D, _) when
    Op =:= make_fun2; Op =:= make_fun3
->
    [Atom, A, _, _, _, _] = maps:get(Idx, maps:get(funs, D)),
    [{maps:get(module, D), maps:get(Atom, maps:get(atoms, D)), A}];
instruction_callees(Op, As, D, Modules) ->
    case import_of(Op, As) of
        {ok, Idx} ->
            {M, F, A} = Target = maps:get(Idx, maps:get(imports, D)),
            case maps:find(M, Modules) of
                %% A stub is not what runs: the native implementation is.
                {ok, Callee} ->
                    case
                        maps:is_key({F, A}, maps:get(function_code, Callee)) andalso
                            not lists:member({F, A}, maps:get(nif_stubs, Callee))
                    of
                        true -> [Target];
                        false -> []
                    end;
                error ->
                    []
            end;
        none ->
            []
    end.
%% `fun M:F/A' is a literal: calling it runs M:F/A.
literal_callees(As, D) ->
    [
        {M, F, A}
     || {literal, N} <- As,
        Fun <- literal_funs(binary_to_term(maps:get(N, maps:get(literals, D))), []),
        {module, M} <- [erlang:fun_info(Fun, module)],
        {name, F} <- [erlang:fun_info(Fun, name)],
        {arity, A} <- [erlang:fun_info(Fun, arity)]
    ].
literal_funs(F, Acc) when is_function(F) -> [F | Acc];
literal_funs(T, Acc) when is_tuple(T) -> literal_funs(tuple_to_list(T), Acc);
literal_funs(M, Acc) when is_map(M) -> literal_funs(maps:to_list(M), Acc);
literal_funs([H | T], Acc) -> literal_funs(T, literal_funs(H, Acc));
literal_funs(_, Acc) -> Acc.
import_of(Op, [_, {?COMPACT_LITERAL, Idx} | _]) when
    Op =:= call_ext; Op =:= call_ext_only; Op =:= call_ext_last
->
    {ok, Idx};
import_of(bif0, [{?COMPACT_LITERAL, Idx} | _]) ->
    {ok, Idx};
import_of(Op, [_, {?COMPACT_LITERAL, Idx} | _]) when Op =:= bif1; Op =:= bif2 ->
    {ok, Idx};
import_of(Op, [_, _, {?COMPACT_LITERAL, Idx} | _]) when
    Op =:= gc_bif1; Op =:= gc_bif2; Op =:= gc_bif3
->
    {ok, Idx};
import_of(_, _) ->
    none.

%% Named by code the analysis reached. A module that names itself, as
%% `?MODULE' does, is named: only reachable code contributes atoms, so a
%% module nothing reaches never becomes a candidate through its own text.
named_module(M, S) ->
    maps:is_key(M, maps:get(surfaced, S)).

%% Atoms the retained modules can still produce. Their code is kept without
%% being analyzed, so scan the operands and literals it holds; the export
%% table is not a source, which is why compiler-generated entry points such as
%% `module_info/0,1' are not dispatch candidates.
scanned_atoms(Named, Modules, Surfaced) ->
    lists:foldl(
        fun(M, Acc) ->
            D = maps:get(M, Modules),
            Atoms = maps:get(atoms, D),
            Literals = maps:get(literals, D),
            lists:foldl(
                fun({_, _, Ops}, Inner) ->
                    lists:foldl(
                        fun
                            %% `func_info' names the function it introduces;
                            %% that is the export table, not a value the code
                            %% can produce.
                            ({_, func_info, _}, I2) ->
                                I2;
                            ({_, _, As}, I2) ->
                                maps:merge(
                                    I2, maps:from_keys(operand_atoms(As, Atoms, Literals), true)
                                )
                        end,
                        Inner,
                        Ops
                    )
                end,
                Acc,
                maps:get(functions, D)
            )
        end,
        Surfaced,
        Named
    ).
operand_atoms(As, Atoms, Literals) when is_list(As) ->
    lists:append([operand_atoms(A, Atoms, Literals) || A <- As]);
operand_atoms({list, As}, Atoms, Literals) ->
    operand_atoms(As, Atoms, Literals);
operand_atoms({typed, A, _}, Atoms, Literals) ->
    operand_atoms(A, Atoms, Literals);
operand_atoms({?COMPACT_ATOM, N}, Atoms, _) when N > 0 ->
    [maps:get(N, Atoms)];
operand_atoms({literal, _}, _, none) ->
    [];
operand_atoms({literal, N}, _, Literals) ->
    term_atoms(binary_to_term(maps:get(N, Literals)));
operand_atoms(_, _, _) ->
    [].
%% Data edges are separate from calls: an MFA-shaped literal is not an
%% executable root. Only its actual use by apply/spawn makes it reachable.
literal_mfas(Term) ->
    lists:usort(literal_mfas(Term, [])).
literal_mfas({M, F, Args} = T, Acc) when is_atom(M), is_atom(F), is_list(Args) ->
    literal_mfas(tuple_to_list(T), [{M, F, length(Args)} | Acc]);
literal_mfas(T, Acc) when is_tuple(T) -> literal_mfas(tuple_to_list(T), Acc);
literal_mfas(T, Acc) when is_map(T) -> literal_mfas(maps:to_list(T), Acc);
literal_mfas([H | T], Acc) ->
    literal_mfas(T, literal_mfas(H, Acc));
literal_mfas(_, Acc) ->
    Acc.

term_roots(F, Caller, S) when is_function(F) ->
    case erlang:fun_info(F, type) of
        {type, external} ->
            {arity, A} = erlang:fun_info(F, arity),
            {module, M} = erlang:fun_info(F, module),
            {name, N} = erlang:fun_info(F, name),
            {_, S1} = external({M, N, A}, lists:duplicate(A, unknown), Caller, S),
            S1;
        {type, local} ->
            fallback(package, Caller, serialized_local_fun, S)
    end;
term_roots(T, Caller, S) when is_tuple(T) -> term_roots(tuple_to_list(T), Caller, S);
term_roots(M, Caller, S) when is_map(M) -> term_roots(maps:to_list(M), Caller, S);
term_roots([H | T], Caller, S) ->
    term_roots(T, Caller, term_roots(H, Caller, S));
term_roots(_, _, S) ->
    S.

beam_module(B) ->
    {ok, {M, _}} = beam_lib:chunks(B, []),
    M.

%% Solve boot dispatch using the actual root arguments. Residual code removes
%% only branches proved impossible; guards on the surviving path remain for
%% the BEAM loader, with impossible failure edges sent to func_info.
entry_index(Is) ->
    hd([I + 1 || I <- lists:seq(1, tuple_size(Is)), element(2, element(I, Is)) =:= func_info]).
residual_module(D, S) ->
    M = maps:get(module, D),
    Residual = D#{
        functions => [
            {F, A, residual_code({M, F, A}, Ops, D, S)}
         || {F, A, Ops} <- maps:get(functions, D)
        ]
    },
    case maps:get(jit_types, S) of
        true -> packbeam_beam:add_types(Residual);
        false -> Residual
    end.
residual_code(MFA, Ops, D, S) ->
    Seen = contexts_seen(MFA, S),
    case maps:size(Seen) of
        %% Retained without being analyzed: keep the function as it was.
        0 ->
            Ops;
        _ ->
            Is = list_to_tuple(Ops),
            Entry = entry_index(Is),
            [{_, label, [{?COMPACT_LITERAL, ErrorLabel}]} | _] = Ops,
            lists:append([
                specialize(element(I, Is), maps:get(I, Seen, #{}), D, ErrorLabel, S)
             || I <- lists:seq(1, tuple_size(Is)), I =< Entry orelse maps:is_key(I, Seen)
            ])
    end.
contexts_seen(MFA, S) ->
    lists:foldl(
        fun(Args, Acc) ->
            ContextSeen = maps:get({MFA, Args}, maps:get(code, S), #{}),
            maps:fold(
                fun(I, R, A) ->
                    case maps:find(I, A) of
                        error -> A#{I => R};
                        {ok, Prev} -> A#{I => join_regs(Prev, R)}
                    end
                end,
                Acc,
                ContextSeen
            )
        end,
        #{},
        maps:get(MFA, maps:get(args, S), [])
    ).
specialize({Op, Name, As} = Insn, R, D, ErrorLabel, S) ->
    Trusted = maps:get(jit_types, S),
    case branch(Name, As, R, D) of
        %% Trusting the analysis, as the types do, a test every context
        %% passes is only work.
        success when Trusted ->
            case lists:member(Name, ?PURE_TESTS) of
                true -> [];
                false -> [Insn]
            end;
        success ->
            [_Fail | Rest] = As,
            [{Op, Name, [{?COMPACT_LABEL, ErrorLabel} | Rest]}];
        {jump, L} ->
            [{beam_opcodes:opcode(jump, 1), jump, [{?COMPACT_LABEL, L}]}];
        unknown when Trusted ->
            [{Op, Name, typed_operands(Name, As, R, D)}];
        unknown ->
            [Insn]
    end.

%% The operands the JIT reads a type for, by position.
typed_positions(Name) when
    Name =:= is_eq_exact;
    Name =:= is_ne_exact;
    Name =:= is_eq;
    Name =:= is_ne;
    Name =:= is_lt;
    Name =:= is_ge
->
    [2, 3];
typed_positions(is_tagged_tuple) ->
    [2];
typed_positions(select_val) ->
    [1];
typed_positions(bif2) ->
    [3, 4];
typed_positions(gc_bif1) ->
    [4];
typed_positions(gc_bif2) ->
    [4, 5];
typed_positions(call_fun2) ->
    [3];
typed_positions(fconv) ->
    [1];
typed_positions(_) ->
    [].
typed_operands(Name, As, R, D) ->
    Positions = typed_positions(Name),
    [
        case lists:member(P, Positions) of
            true -> typed_operand(A, R, D);
            false -> A
        end
     || {P, A} <- lists:zip(lists:seq(1, length(As)), As)
    ].
typed_operand({typed, Reg, Type} = A, R, D) ->
    case packbeam_types:of_value(val(Reg, R, D)) of
        none -> A;
        T -> {typed, Reg, {new_type, T, Type}}
    end;
typed_operand({Tag, _} = Reg, R, D) when Tag =:= ?COMPACT_XREG; Tag =:= ?COMPACT_YREG ->
    case packbeam_types:of_value(val(Reg, R, D)) of
        none -> Reg;
        T -> {typed, Reg, {new_type, T, none}}
    end;
typed_operand(A, _, _) ->
    A.
branch(Name, [{?COMPACT_LABEL, Fail} | As], R, D) ->
    case predicate(Name, [branch_value(A, R, D) || A <- As]) of
        true -> success;
        false when Fail =/= 0 -> {jump, Fail};
        _ -> unknown
    end;
branch(Name, [Src, {?COMPACT_LABEL, Fail}, {list, Pairs}], R, D) when
    Name =:= select_val; Name =:= select_tuple_arity
->
    V =
        case Name of
            select_val -> val(Src, R, D);
            select_tuple_arity -> tuple_arity(val(Src, R, D))
        end,
    case V of
        {const, C} ->
            Target = select_target(C, Pairs, Fail, R, D),
            case Target of
                0 -> unknown;
                _ -> {jump, Target}
            end;
        _ ->
            unknown
    end;
branch(_, _, _, _) ->
    unknown.
branch_value({?COMPACT_LITERAL, N}, _, _) -> {const, N};
branch_value(A, R, D) -> val(A, R, D).
select_target(_, [], Default, _, _) ->
    Default;
select_target(V, [Value, {?COMPACT_LABEL, L} | Tail], Default, R, D) ->
    case branch_value(Value, R, D) of
        {const, V} -> L;
        _ -> select_target(V, Tail, Default, R, D)
    end.
predicate(Name, [{choices, Vs} | Rest]) ->
    case lists:usort([predicate(Name, [V | Rest]) || V <- Vs]) of
        [true] -> true;
        [false] -> false;
        _ -> unknown
    end;
predicate(is_eq_exact, [{const, A}, {const, B}]) ->
    A =:= B;
predicate(is_ne_exact, [{const, A}, {const, B}]) ->
    A =/= B;
predicate(is_ge, [{const, A}, {const, B}]) ->
    A >= B;
predicate(is_lt, [{const, A}, {const, B}]) ->
    A < B;
predicate(test_arity, [V, {const, N}]) ->
    case tuple_arity(V) of
        {const, A} -> A =:= N;
        none -> false;
        _ -> unknown
    end;
predicate(is_tagged_tuple, [V, {const, N}, Tag]) ->
    case {tuple_arity(V), tuple_get(0, V), Tag} of
        {{const, N}, {const, T}, {const, T}} -> true;
        {none, _, _} -> false;
        {{const, A}, _, _} when A =/= N -> false;
        {{const, N}, {const, A}, {const, B}} -> A =:= B;
        _ -> unknown
    end;
predicate(is_nonempty_list, [{cons, _, _}]) ->
    true;
predicate(is_nil, [{cons, _, _}]) ->
    false;
predicate(is_tuple, [{tuple, _}]) ->
    true;
predicate(is_map, [{map, _}]) ->
    true;
predicate(Name, [{const, V}]) ->
    case Name of
        is_nonempty_list ->
            case V of
                [_ | _] -> true;
                _ -> false
            end;
        is_nil ->
            V =:= [];
        is_atom ->
            is_atom(V);
        is_tuple ->
            is_tuple(V);
        is_map ->
            is_map(V);
        is_binary ->
            is_binary(V);
        is_integer ->
            is_integer(V);
        is_float ->
            is_float(V);
        is_number ->
            is_number(V);
        is_list ->
            is_list(V);
        _ ->
            unknown
    end;
predicate(Name, [V]) ->
    shape_predicate(Name, shape(V));
predicate(_, _) ->
    unknown.
shape({tuple, _}) -> tuple;
shape({map, _}) -> map;
shape({cons, _, _}) -> cons;
shape({sequence, _}) -> list;
shape({type, T}) -> T;
shape({closure, _}) -> function;
shape(_) -> unknown.
shape_predicate(_, unknown) ->
    unknown;
shape_predicate(is_tuple, T) ->
    T =:= tuple;
shape_predicate(is_map, T) ->
    T =:= map;
shape_predicate(is_pid, T) ->
    T =:= pid;
shape_predicate(is_reference, T) ->
    T =:= reference;
shape_predicate(is_port, T) ->
    T =:= port;
shape_predicate(is_function, T) ->
    T =:= function;
shape_predicate(is_list, cons) ->
    unknown;
shape_predicate(is_list, T) ->
    T =:= list;
shape_predicate(is_nonempty_list, list) ->
    unknown;
shape_predicate(is_nonempty_list, T) ->
    T =:= cons;
shape_predicate(is_nil, list) ->
    unknown;
shape_predicate(is_nil, _) ->
    false;
shape_predicate(Name, _) when
    Name =:= is_atom;
    Name =:= is_integer;
    Name =:= is_float;
    Name =:= is_number;
    Name =:= is_binary;
    Name =:= is_bitstr
->
    false;
shape_predicate(_, _) ->
    unknown.
tuple_arity({choices, Vs}) ->
    project(fun tuple_arity/1, Vs);
tuple_arity({const, T}) when not is_tuple(T) -> none;
tuple_arity({const, T}) when is_tuple(T) -> {const, tuple_size(T)};
tuple_arity({tuple, Vs}) ->
    {const, length(Vs)};
tuple_arity(V) ->
    case shape(V) of
        unknown -> unknown;
        _ -> none
    end.
operand_registers(L) when is_list(L) -> lists:usort(lists:append([operand_registers(X) || X <- L]));
operand_registers({Tag, _} = R) when Tag =:= ?COMPACT_XREG; Tag =:= ?COMPACT_YREG -> [R];
operand_registers(T) when is_tuple(T) -> operand_registers(tuple_to_list(T));
operand_registers(_) -> [].
