%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% Lossless compact operand decoding; only relocated operands are re-encoded.
-module(packbeam_beam).
-include("compact_term.hrl").
-export([read/1, trim/2, normalize/1, close_keep/2, add_types/1]).
-export_type([beam_module/0]).

%% A decoded module, as `read/1' returns it: its chunks, tables and code split
%% into functions of compact operations.
-type beam_module() :: #{
    module := module(),
    binary := binary(),
    chunks := [{string(), binary()}],
    functions := [{atom(), arity(), [tuple()]}],
    atom() => term()
}.

%% @doc Rewrite uncompressed literals (`LitU') as a `LitT' chunk, the only
%% form the rest of this module reads.
-spec normalize(binary()) -> binary().
normalize(B) ->
    {ok, _, Cs} = beam_lib:all_chunks(B),
    {ok, N} = beam_lib:build_module([
        case C of
            {"LitU", D} -> {"LitT", <<0:32, D/binary>>};
            _ -> C
        end
     || C <- Cs
    ]),
    N.

%% @doc Decode a BEAM file. Raises `{cannot_prune_native_code, Module}' for a
%% module precompiled to native code.
-spec read(binary()) -> beam_module().
read(B) ->
    N = normalize(B),
    {ok, M, Cs} = beam_lib:all_chunks(N),
    case lists:any(fun({K, _}) -> K =:= "avmN" end, Cs) of
        true -> error({cannot_prune_native_code, M});
        false -> ok
    end,
    {ok, {M, Refs}} = beam_lib:chunks(N, [atoms, imports, exports]),
    Atoms = maps:from_list(proplists:get_value(atoms, Refs)),
    <<Sz:32, Header:Sz/binary, Bytes/binary>> = proplists:get_value("Code", Cs),
    <<Version:32, Max:32, _:32, _:32, Extra/binary>> = Header,
    0 = Version,
    Ops = decode(Bytes),
    Fs = split(Ops, Atoms, [], [], []),
    Imports = indexed([
        {maps:get(MI, Atoms), maps:get(FI, Atoms), Ar}
     || [MI, FI, Ar] <- table(Cs, "ImpT", 3)
    ]),
    #{
        module => M,
        binary => B,
        chunks => Cs,
        header => {Version, Max, Extra},
        atoms => Atoms,
        imports => Imports,
        nif_stubs => nif_stubs(Fs, Imports),
        exports => proplists:get_value(exports, Refs),
        functions => Fs,
        literals => indexed(literals(Cs)),
        funs => indexed(table(Cs, "FunT", 6)),
        records => records(Cs)
    }.

%% Edges to a branch the analysis never entered go to the function's error
%% label: should the analysis be wrong, the code raises instead of jumping
%% into unrelated code.
patch_dangling(Selected) ->
    Kept = [
        N
     || {_, _, Ops} <- Selected, {_, label, [X]} <- Ops, {?COMPACT_LITERAL, N} <- [value(X)]
    ],
    [
        begin
            [{_, label, [E]} | _] = Ops,
            {?COMPACT_LITERAL, Error} = value(E),
            {F, A, [patch(I, Error, Kept) || I <- Ops]}
        end
     || {F, A, Ops} <- Selected
    ].
patch({Op, Name, As}, Error, Kept) -> {Op, Name, [patch_arg(X, Error, Kept) || X <- As]}.
patch_arg({list, As}, Error, Kept) ->
    {list, [patch_arg(X, Error, Kept) || X <- As]};
patch_arg({typed, A, T}, Error, Kept) ->
    {typed, patch_arg(A, Error, Kept), T};
patch_arg(A, Error, Kept) ->
    case value(A) of
        {?COMPACT_LABEL, 0} ->
            A;
        {?COMPACT_LABEL, N} ->
            case lists:member(N, Kept) of
                true -> A;
                false -> {?COMPACT_LABEL, Error}
            end;
        _ ->
            A
    end.

%% A path the analysis proved impossible but left in place may still name a
%% label or a fun of a function nobody asked to keep.
-spec close_keep(beam_module(), [{atom(), arity()}]) -> [{atom(), arity()}].
close_keep(#{functions := Fs} = M, Keep) ->
    close_references(Fs, M, lists:usort(Keep)).
close_references(Fs, M, Keep) ->
    Owner = maps:from_list([
        {N, {F, A}}
     || {F, A, Ops} <- Fs, {_, label, [X]} <- Ops, {?COMPACT_LITERAL, N} <- [value(X)]
    ]),
    Funs = maps:get(funs, M),
    Refs = lists:usort([
        Target
     || {F, A, Ops} <- Fs,
        lists:member({F, A}, Keep),
        {_, Name, As} <- Ops,
        N <- op_labels(Name, As, Funs),
        {ok, Target} <- [maps:find(N, Owner)]
    ]),
    case lists:usort(Keep ++ Refs) of
        Keep -> Keep;
        More -> close_references(Fs, M, More)
    end.
op_labels(Name, [A | _], Funs) when Name =:= make_fun2; Name =:= make_fun3 ->
    {?COMPACT_LITERAL, I} = value(A),
    [L || [_, _, L, _, _, _] <- [maps:get(I, Funs)]];
%% A call to a fun whose target is known names its FunT entry, while an
%% unknown one is tagged `safe' or `unsafe'.
op_labels(call_fun2, [A | _], Funs) ->
    case value(A) of
        {?COMPACT_LITERAL, I} -> [L || [_, _, L, _, _, _] <- [maps:get(I, Funs)]];
        _ -> []
    end;
op_labels(_, As, _) ->
    labels(As).
labels(L) when is_list(L) -> lists:append([labels(X) || X <- L]);
labels({list, L}) -> labels(L);
labels({typed, A, _}) -> labels(A);
labels({?COMPACT_LABEL, N}) -> [N];
labels({raw, ?COMPACT_LABEL, _, _, N}) -> [N];
labels(_) -> [].

indexed(L) -> maps:from_list(lists:zip(lists:seq(0, length(L) - 1), L)).

%% A body calling `erlang:nif_error/1,2' is a placeholder for native code: it
%% need not accept the arguments the native code handles, nor describe what
%% the call returns.
nif_stubs(Fs, Imports) ->
    Stubs = [
        I
     || {I, {erlang, nif_error, A}} <- maps:to_list(Imports),
        A =:= 1 orelse A =:= 2
    ],
    [
        {F, A}
     || {F, A, Ops} <- Fs,
        lists:any(
            fun
                ({_, Op, [{?COMPACT_LITERAL, _}, {?COMPACT_LITERAL, Idx} | _]}) when
                    Op =:= call_ext; Op =:= call_ext_only; Op =:= call_ext_last
                ->
                    lists:member(Idx, Stubs);
                (_) ->
                    false
            end,
            Ops
        )
    ].
%% OTP 29 native record definitions are encoded as instructions, one per
%% record, whose operands name atoms and literals by index.
records(Cs) ->
    case proplists:get_value("Recs", Cs) of
        undefined -> none;
        <<0:32, Count:32, Fields:32, Data/binary>> -> {Count, Fields, decode(Data)};
        _ -> error(unsupported_records_chunk)
    end.
records_chunk({Count, Fields, Defs}, Rel) ->
    iolist_to_binary([
        <<0:32, Count:32, Fields:32>>
        | [[Op, [encode(relocate(A, Rel)) || A <- As]] || {Op, _, As} <- Defs]
    ]).
literals(Cs) ->
    case proplists:get_value("LitT", Cs) of
        undefined -> [];
        <<0:32, D/binary>> -> literal_entries(D);
        <<_:32, D/binary>> -> literal_entries(zlib:uncompress(D))
    end.
literal_entries(<<_:32, D/binary>>) -> literal_entries0(D).
literal_entries0(<<>>) -> [];
literal_entries0(<<S:32, T:S/binary, R/binary>>) -> [T | literal_entries0(R)].
table(Cs, K, W) ->
    case proplists:get_value(K, Cs) of
        undefined -> [];
        <<_:32, D/binary>> -> rows(D, W)
    end.
rows(<<>>, _) ->
    [];
rows(B, W) ->
    S = W * 4,
    <<Row:S/binary, R/binary>> = B,
    [[I || <<I:32>> <= Row] | rows(R, W)].

%% A function begins at the label preceding func_info (possibly with line ops).
split(
    [{_, func_info, [_, {?COMPACT_ATOM, F}, {?COMPACT_LITERAL, A}]} = I | T],
    Atoms,
    Prefix,
    Current,
    Acc
) ->
    {Before, Start} = take_start(lists:reverse(Prefix)),
    Acc1 =
        case Current of
            [] -> Acc;
            {Name, Arity, Body} -> [{Name, Arity, Body ++ Before} | Acc]
        end,
    split(T, Atoms, [], {maps:get(F, Atoms), A, Start ++ [I]}, Acc1);
split([{_, int_code_end, []}], _Atoms, Prefix, {F, A, Body}, Acc) ->
    lists:reverse([{F, A, Body ++ lists:reverse(Prefix)} | Acc]);
split([I | T], Atoms, Prefix, Current, Acc) ->
    split(T, Atoms, [I | Prefix], Current, Acc).
take_start(L) ->
    Rev = lists:reverse(L),
    {Suffix, [Label | Rest]} = lists:splitwith(fun({_, N, _}) -> N =/= label end, Rev),
    {lists:reverse(Rest), [Label | lists:reverse(Suffix)]}.

decode(<<>>) ->
    [];
decode(<<Op:8, R/binary>>) ->
    {Name, N} =
        try
            beam_opcodes:opname(Op)
        catch
            _:_ -> error({unsupported_opcode, Op})
        end,
    {Args, T} = args(N, R),
    [{Op, Name, Args} | decode(T)].
args(0, B) ->
    {[], B};
args(N, B) ->
    {A, R} = arg(B),
    {As, T} = args(N - 1, R),
    {[A | As], T}.
arg(<<B:8, R/binary>>) when B band ?COMPACT_TAG_MASK =:= ?COMPACT_EXTENDED ->
    case B of
        ?COMPACT_EXTENDED_FLOAT ->
            <<F:8/binary, T/binary>> = R,
            {{float_bytes, F}, T};
        ?COMPACT_EXTENDED_LIST ->
            {{?COMPACT_LITERAL, N}, R1} = arg(R),
            {L, T} = args(N, R1),
            {{list, L}, T};
        ?COMPACT_EXTENDED_FP_REGISTER ->
            {A, T} = arg(R),
            {{fr, A}, T};
        ?COMPACT_EXTENDED_ALLOCATION_LIST ->
            {{?COMPACT_LITERAL, N}, R1} = arg(R),
            {L, T} = args(N * 2, R1),
            {{alloc, L}, T};
        ?COMPACT_EXTENDED_LITERAL ->
            {{?COMPACT_LITERAL, N}, T} = arg(R),
            {{literal, N}, T};
        ?COMPACT_EXTENDED_TYPED_REGISTER ->
            {A, R1} = arg(R),
            {Type, T} = arg(R1),
            {{typed, A, Type}, T};
        _ ->
            error({unsupported_operand, B})
    end;
arg(Bin = <<B:8, R/binary>>) ->
    Tag = B band ?COMPACT_TAG_MASK,
    case {B band 8, B band 16} of
        {?COMPACT_LITERAL, _} ->
            {{Tag, B bsr 4}, R};
        {_, 0} ->
            <<Low:8, T/binary>> = R,
            {{Tag, ((B bsr 5) bsl 8) bor Low}, T};
        _ ->
            {Len, R1} =
                case B bsr 5 of
                    7 ->
                        {{?COMPACT_LITERAL, L}, Tail} = arg(R),
                        {L + 9, Tail};
                    L ->
                        {L + 2, R}
                end,
            <<V:Len/unit:8, T/binary>> = R1,
            case Tag of
                ?COMPACT_INTEGER ->
                    {
                        {raw, Tag, binary:part(Bin, 0, byte_size(Bin) - byte_size(T)),
                            byte_size(Bin) - byte_size(T), V},
                        T
                    };
                _ ->
                    {{Tag, V}, T}
            end
    end.
value({raw, T, _, _, V}) -> {T, V};
value(X) -> X.
encode({raw, _, B, N, _}) ->
    binary:part(B, 0, N);
encode({list, L}) ->
    [?COMPACT_EXTENDED_LIST, enc(?COMPACT_LITERAL, length(L)), [encode(A) || A <- L]];
encode({alloc, L}) ->
    [
        ?COMPACT_EXTENDED_ALLOCATION_LIST,
        enc(?COMPACT_LITERAL, length(L) div 2),
        [encode(A) || A <- L]
    ];
encode({literal, N}) ->
    [?COMPACT_EXTENDED_LITERAL, enc(?COMPACT_LITERAL, N)];
encode({fr, A}) ->
    [?COMPACT_EXTENDED_FP_REGISTER, encode(A)];
encode({typed, A, T}) ->
    [?COMPACT_EXTENDED_TYPED_REGISTER, encode(A), encode(T)];
encode({float_bytes, B}) ->
    [?COMPACT_EXTENDED_FLOAT, B];
encode({T, V}) ->
    enc(T, V).
enc(T, V) when V < 16 -> <<(V bsl 4 bor T)>>;
enc(T, V) when V < 2048 -> <<((V bsr 8) bsl 5 bor 8 bor T), (V band 255)>>;
enc(T, V) ->
    %% The loader reads these bytes as a signed number, so as the compiler
    %% does, a value whose first byte has its high bit set gets a zero byte
    %% in front.
    B =
        case binary:encode_unsigned(V) of
            <<0:1, _/bits>> = B0 -> B0;
            B0 -> <<0, B0/binary>>
        end,
    N = byte_size(B),
    case N of
        _ when N < 9 -> [<<((N - 2) bsl 5 bor 24 bor T)>>, B];
        _ -> [<<(224 bor 24 bor T)>>, enc(?COMPACT_LITERAL, N - 9), B]
    end.

%% @doc Rebuild the module with only the functions in `Keep0' and those
%% their code still names, with relocated labels, literals and tables.
-spec trim(beam_module(), [{atom(), arity()}]) -> binary().
trim(#{functions := Fs} = M, Keep0) ->
    Keep = close_keep(M, Keep0),
    Selected0 = [{F, A, Is} || {F, A, Is} <- Fs, lists:member({F, A}, Keep)],
    Selected = patch_dangling(Selected0),
    Ops = lists:append([Is || {_, _, Is} <- Selected]),
    Labels = [N || {_, label, [A]} <- Ops, {?COMPACT_LITERAL, N} <- [value(A)]],
    LM = maps:from_list([{?COMPACT_LITERAL, 0} | lists:zip(Labels, lists:seq(1, length(Labels)))]),
    Records = maps:get(records, M),
    RecordOps =
        case Records of
            none -> [];
            {_, _, Defs} -> Defs
        end,
    LitIds = lists:usort(
        lists:append([collect(literal, As) || {_, _, As} <- Ops ++ RecordOps])
    ),
    LitMap = maps:from_list(lists:zip(LitIds, lists:seq(0, length(LitIds) - 1))),
    FunIds = lists:usort([
        N
     || {_, Name, [A | _]} <- Ops,
        lists:member(Name, [make_fun2, make_fun3, call_fun2]),
        {?COMPACT_LITERAL, N} <- [value(A)]
    ]),
    FM = maps:from_list(lists:zip(FunIds, lists:seq(0, length(FunIds) - 1))),
    Cs = maps:get(chunks, M),
    ImpIds = lists:usort([I || {_, Name, As} <- Ops, I <- [import_index(Name, As)], I =/= none]),
    ImpRows = table(Cs, "ImpT", 3),
    IM = index_map(ImpIds, 0),
    %% The loader expects `any' at index 0, whether or not code names it.
    TypeIds = lists:usort([0 | lists:append([collect(type, As) || {_, _, As} <- Ops])]),
    {TypeVersion, TypeEntries} = type_entries(Cs),
    {TM, KeptTypes} =
        case TypeEntries of
            [] -> {identity, []};
            _ -> {index_map(TypeIds, 0), [lists:nth(I + 1, TypeEntries) || I <- TypeIds]}
        end,
    ExpRows = relocate_table(table(Cs, "ExpT", 3), LM),
    LocRows = relocate_table(table(Cs, "LocT", 3), LM),
    KeptImports = [lists:nth(I + 1, ImpRows) || I <- ImpIds],
    FunRows0 = [maps:get(I, maps:get(funs, M)) || I <- FunIds],
    %% Atom 1 is the module name, and the tables that survive name their own
    %% atoms; index 0 is nil and is not in the table.
    AtomIds = lists:usort(
        [1] ++
            lists:append([collect(?COMPACT_ATOM, As) || {_, _, As} <- Ops]) ++
            lists:append([[Mod, Fun] || [Mod, Fun, _] <- KeptImports]) ++
            [N || [N, _, _] <- ExpRows ++ LocRows] ++
            [N || [N | _] <- FunRows0] ++
            lists:append([collect(?COMPACT_ATOM, As) || {_, _, As} <- RecordOps])
    ),
    AM = index_map([I || I <- AtomIds, I > 0], 1),
    Rel = #{labels => LM, literals => LitMap, funs => FM, imports => IM, types => TM, atoms => AM},
    NewOps = [rewrite(I, Rel) || I <- Ops],
    Code = iolist_to_binary(
        [[Op, [encode(A) || A <- As]] || {Op, _, As} <- NewOps] ++ [?OP_INT_CODE_END]
    ),
    {V, Max, Extra} = maps:get(header, M),
    Header = <<V:32, Max:32, (length(Labels) + 1):32, (length(Selected)):32, Extra/binary>>,
    CodeChunk = <<(byte_size(Header)):32, Header/binary, Code/binary>>,
    LitData = iolist_to_binary([
        <<(length(LitIds)):32>>
        | [
            begin
                B = maps:get(I, maps:get(literals, M)),
                <<(byte_size(B)):32, B/binary>>
            end
         || I <- LitIds
        ]
    ]),
    FunRows = [
        [atom_id(F, AM), A, maps:get(L, LM), maps:get(I, FM), Free, U]
     || {I, [F, A, L, _, Free, U]} <- lists:zip(FunIds, FunRows0)
    ],
    NewCs = [
        case K of
            "Code" ->
                {K, CodeChunk};
            "ExpT" ->
                {K, encode_table(name_column(ExpRows, AM))};
            "LocT" ->
                {K, encode_table(name_column(LocRows, AM))};
            "FunT" ->
                {K, encode_table(FunRows)};
            %% Loaders before OTP 27 always inflate this chunk.
            "LitT" ->
                {K, <<(byte_size(LitData)):32, (zlib:compress(LitData))/binary>>};
            "ImpT" ->
                {K,
                    encode_table([
                        [atom_id(Mod, AM), atom_id(Fun, AM), Ar]
                     || [Mod, Fun, Ar] <- KeptImports
                    ])};
            "Recs" ->
                {K, records_chunk(Records, Rel)};
            "Type" ->
                {K, type_chunk(TypeVersion, KeptTypes, D)};
            A when A =:= "AtU8"; A =:= "Atom" -> {K, atom_chunk(D, AtomIds)};
            _ ->
                {K, D}
        end
     || {K, D} <- Cs,
        lists:member(K, [
            "AtU8",
            "Atom",
            "Code",
            "ExpT",
            "LocT",
            "ImpT",
            "FunT",
            "LitT",
            "StrT",
            "Line",
            "Type",
            "Recs"
        ])
    ],
    {ok, Binary} = beam_lib:build_module(NewCs),
    Binary.
relocate_table(Rows, LM) -> [[F, A, maps:get(L, LM)] || [F, A, L] <- Rows, maps:is_key(L, LM)].
name_column(Rows, AM) -> [[atom_id(N, AM), A, L] || [N, A, L] <- Rows].
index_map(Ids, First) -> maps:from_list(lists:zip(Ids, lists:seq(First, First + length(Ids) - 1))).
atom_id(0, _) -> 0;
atom_id(N, AM) -> maps:get(N, AM).

import_index(Name, [_, {?COMPACT_LITERAL, I} | _]) when
    Name =:= call_ext; Name =:= call_ext_only; Name =:= call_ext_last
->
    I;
import_index(bif0, [{?COMPACT_LITERAL, I} | _]) ->
    I;
import_index(Name, [_, _, {?COMPACT_LITERAL, I} | _]) when
    Name =:= gc_bif1; Name =:= gc_bif2; Name =:= gc_bif3
->
    I;
import_index(Name, [_, {?COMPACT_LITERAL, I} | _]) when Name =:= bif1; Name =:= bif2 ->
    I;
import_index(_, _) ->
    none.

%% A 16-bit header says which of the bounds and the unit follow it. A chunk
%% in another format is left alone rather than guessed at.
type_entries(Cs) ->
    case proplists:get_value("Type", Cs) of
        <<Version:32, _Count:32, Data/binary>> when Version =:= 3; Version =:= 4 ->
            case split_types(Version, Data, []) of
                error -> {Version, []};
                Entries -> {Version, Entries}
            end;
        _ ->
            {none, []}
    end.
split_types(_, <<>>, Acc) ->
    lists:reverse(Acc);
split_types(Version, <<Header:16, Rest/binary>> = All, Acc) ->
    {HasUnit, HasUpper, HasLower} =
        case Version of
            3 ->
                <<0:1, U:1, Up:1, Lo:1, _:12>> = <<Header:16>>,
                {U, Up, Lo};
            4 ->
                <<U:1, Up:1, Lo:1, _:13>> = <<Header:16>>,
                {U, Up, Lo}
        end,
    Extra = 8 * HasLower + 8 * HasUpper + HasUnit,
    case Rest of
        <<_:Extra/binary, Tail/binary>> ->
            Size = 2 + Extra,
            <<Entry:Size/binary, _/binary>> = All,
            split_types(Version, Tail, [Entry | Acc]);
        _ ->
            error
    end;
split_types(_, _, _) ->
    error.

%% @doc Record the types the analysis attached to operands in the `Type'
%% chunk, or drop the new ones when the module has no such chunk.
-spec add_types(beam_module()) -> beam_module().
add_types(#{functions := Fs, chunks := Cs} = M) ->
    case type_entries(Cs) of
        {Version, [_ | _] = Entries} ->
            Known = maps:from_list(lists:zip(Entries, lists:seq(0, length(Entries) - 1))),
            {Typed, {_, Added}} = lists:mapfoldl(
                fun({F, A, Ops}, Acc0) ->
                    {Ops1, Acc1} = lists:mapfoldl(
                        fun({Op, Name, As}, Acc) ->
                            {As1, Acc2} = lists:mapfoldl(
                                fun(X, AccX) -> add_type(X, Version, Entries, AccX) end,
                                Acc,
                                As
                            ),
                            {{Op, Name, As1}, Acc2}
                        end,
                        Acc0,
                        Ops
                    ),
                    {{F, A, Ops1}, Acc1}
                end,
                {Known, []},
                Fs
            ),
            Chunk = type_chunk(Version, Entries ++ lists:reverse(Added), <<>>),
            M#{functions => Typed, chunks => lists:keyreplace("Type", 1, Cs, {"Type", Chunk})};
        _ ->
            M#{
                functions => [
                    {F, A, [{Op, Name, [untyped(X) || X <- As]} || {Op, Name, As} <- Ops]}
                 || {F, A, Ops} <- Fs
                ]
            }
    end.
add_type({typed, Reg, {new_type, T, Existing}}, Version, Entries, {Known, Added} = Acc) ->
    Old =
        case Existing of
            {?COMPACT_LITERAL, N} -> packbeam_types:decode(Version, lists:nth(N + 1, Entries));
            _ -> any
        end,
    case packbeam_types:refine(Old, T) of
        Old ->
            {untyped({typed, Reg, {new_type, T, Existing}}), Acc};
        Type ->
            Entry = packbeam_types:encode(Version, Type),
            case maps:find(Entry, Known) of
                {ok, I} ->
                    {{typed, Reg, {?COMPACT_LITERAL, I}}, Acc};
                error ->
                    I = maps:size(Known),
                    {{typed, Reg, {?COMPACT_LITERAL, I}}, {Known#{Entry => I}, [Entry | Added]}}
            end
    end;
add_type(X, _, _, Acc) ->
    {X, Acc}.
untyped({typed, Reg, {new_type, _, none}}) -> Reg;
untyped({typed, Reg, {new_type, _, Existing}}) -> {typed, Reg, Existing};
untyped(X) -> X.

type_chunk(none, _, Data) ->
    Data;
type_chunk(_, [], Data) ->
    Data;
type_chunk(Version, Entries, _) ->
    iolist_to_binary([<<Version:32, (length(Entries)):32>> | Entries]).

%% A negative count marks the long-atom form, where lengths use the compact
%% term encoding. Names are copied with their own prefix, so either form comes
%% back out as it went in.
atom_chunk(<<Count:32/signed, Data/binary>>, Ids) when Count < 0 ->
    Kept = kept_atoms(split_atoms(compact, Data, []), Ids),
    iolist_to_binary([<<(-length(Kept)):32/signed>> | Kept]);
atom_chunk(<<_Count:32, Data/binary>>, Ids) ->
    Kept = kept_atoms(split_atoms(byte, Data, []), Ids),
    iolist_to_binary([<<(length(Kept)):32>> | Kept]);
atom_chunk(Data, _) ->
    Data.
kept_atoms(Names, Ids) -> [lists:nth(I, Names) || I <- Ids, I =< length(Names)].
split_atoms(_, <<>>, Acc) ->
    lists:reverse(Acc);
split_atoms(byte, <<Len:8, Name:Len/binary, Rest/binary>>, Acc) ->
    split_atoms(byte, Rest, [<<Len:8, Name/binary>> | Acc]);
split_atoms(compact, Bin, Acc) ->
    {{?COMPACT_LITERAL, Len}, Rest} = arg(Bin),
    Prefix = byte_size(Bin) - byte_size(Rest),
    <<Entry:(Prefix + Len)/binary, Tail/binary>> = Bin,
    split_atoms(compact, Tail, [Entry | Acc]).
encode_table(Rows) ->
    iolist_to_binary([<<(length(Rows)):32>> | [<<I:32>> || Row <- Rows, I <- Row]]).
collect(type, {typed, _, {?COMPACT_LITERAL, N}}) -> [N];
collect(?COMPACT_ATOM, {typed, A, _}) -> collect(?COMPACT_ATOM, A);
collect(?COMPACT_ATOM, {?COMPACT_ATOM, 0}) -> [];
collect(Tag, L) when is_list(L) -> lists:append([collect(Tag, X) || X <- L]);
collect(Tag, {Tag, N}) when is_integer(N) -> [N];
collect(Tag, T) when is_tuple(T) -> collect(Tag, tuple_to_list(T));
collect(_, _) -> [].
rewrite({Op, Name, As}, #{labels := LM, funs := FM} = Rel) ->
    Bs = [relocate(A, Rel) || A <- As],
    Cs =
        case {Name, Bs} of
            {label, [A]} ->
                {?COMPACT_LITERAL, N} = value(A),
                [{?COMPACT_LITERAL, maps:get(N, LM)}];
            {call_fun2, [{?COMPACT_LITERAL, I} | T]} ->
                [{?COMPACT_LITERAL, maps:get(I, FM)} | T];
            {N, [A | T]} when N =:= make_fun2; N =:= make_fun3 ->
                {?COMPACT_LITERAL, I} = value(A),
                [{?COMPACT_LITERAL, maps:get(I, FM)} | T];
            _ ->
                relocate_import(Name, Bs, maps:get(imports, Rel))
        end,
    {Op, Name, Cs}.
relocate_import(Name, Bs, IM) ->
    case import_index(Name, Bs) of
        none ->
            Bs;
        I ->
            {Before, [_ | After]} = lists:split(import_position(Name), Bs),
            Before ++ [{?COMPACT_LITERAL, maps:get(I, IM)} | After]
    end.
import_position(bif0) -> 0;
import_position(Name) when Name =:= gc_bif1; Name =:= gc_bif2; Name =:= gc_bif3 -> 2;
import_position(_) -> 1.

relocate({?COMPACT_LABEL, N}, #{labels := LM}) -> {?COMPACT_LABEL, maps:get(N, LM)};
relocate({raw, 5, _, _, N}, #{labels := LM}) -> {?COMPACT_LABEL, maps:get(N, LM)};
relocate({literal, N}, #{literals := L}) -> {literal, maps:get(N, L)};
relocate({?COMPACT_ATOM, 0}, _) -> {?COMPACT_ATOM, 0};
relocate({?COMPACT_ATOM, N}, #{atoms := AM}) -> {?COMPACT_ATOM, maps:get(N, AM)};
relocate({list, As}, Rel) -> {list, [relocate(A, Rel) || A <- As]};
relocate({typed, A, T}, #{types := TM} = Rel) -> {typed, relocate(A, Rel), relocate_type(T, TM)};
relocate(A, _) -> A.
relocate_type(T, identity) -> T;
relocate_type({?COMPACT_LITERAL, N}, TM) -> {?COMPACT_LITERAL, maps:get(N, TM)};
relocate_type(T, _) -> T.
