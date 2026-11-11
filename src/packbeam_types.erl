%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% What the pruning analysis proves about a register, in the terms of the
%% `Type' chunk the compiler writes and the JIT reads.
-module(packbeam_types).
-export([of_value/1, encode/2, decode/2, refine/2]).

-define(ATOM, 16#1).
-define(BITSTRING, 16#2).
-define(CONS, 16#4).
-define(FLOAT, 16#8).
-define(FUN, 16#10).
-define(INTEGER, 16#20).
-define(MAP, 16#40).
-define(NIL, 16#80).
-define(PID, 16#100).
-define(PORT, 16#200).
-define(REFERENCE, 16#400).
-define(TUPLE, 16#800).
%% Bounds must be small integers on a 64-bit machine, as the compiler writes
%% them.
-define(SMALL, (1 bsl 59)).

-type type() ::
    atom
    | nil
    | cons
    | list
    | tuple
    | map
    | function
    | float
    | number
    | pid
    | port
    | reference
    | {integer, integer() | '-inf', integer() | '+inf'}
    | {bitstring, pos_integer()}.
-export_type([type/0]).

%% The type of every value an abstract value stands for, or `none' when no
%% single type the chunk can express holds them all.
-spec of_value(term()) -> type() | none.
of_value({const, C}) ->
    of_term(C);
of_value({tuple, _}) ->
    tuple;
of_value({map, _}) ->
    map;
of_value({cons, _, _}) ->
    cons;
of_value({sequence, _}) ->
    list;
of_value({closure, _}) ->
    function;
of_value({type, T}) when T =:= pid; T =:= port; T =:= reference -> T;
of_value({choices, [V | Vs]}) ->
    lists:foldl(fun(W, T) -> lub(T, of_value(W)) end, of_value(V), Vs);
of_value(_) ->
    none.

of_term(C) when is_atom(C) -> atom;
of_term([]) -> nil;
of_term([_ | _]) -> cons;
of_term(C) when is_tuple(C) -> tuple;
of_term(C) when is_map(C) -> map;
of_term(C) when is_integer(C), C >= -?SMALL, C < ?SMALL -> {integer, C, C};
of_term(C) when is_float(C) -> float;
of_term(C) when is_binary(C) -> {bitstring, 8};
of_term(C) when is_bitstring(C) -> {bitstring, 1};
of_term(C) when is_function(C) -> function;
of_term(C) when is_pid(C) -> pid;
of_term(C) when is_port(C) -> port;
of_term(C) when is_reference(C) -> reference;
of_term(_) -> none.

lub(none, _) ->
    none;
lub(_, none) ->
    none;
lub(T, T) ->
    T;
lub({integer, A, B}, {integer, C, D}) ->
    {integer, min(A, C), max(B, D)};
lub({integer, _, _}, float) ->
    number;
lub(float, {integer, _, _}) ->
    number;
lub(number, {integer, _, _}) ->
    number;
lub({integer, _, _}, number) ->
    number;
lub(number, float) ->
    number;
lub(float, number) ->
    number;
lub({bitstring, A}, {bitstring, B}) ->
    {bitstring, gcd(A, B)};
lub(A, B) when
    (A =:= nil orelse A =:= cons orelse A =:= list), (B =:= nil orelse B =:= cons orelse B =:= list)
->
    list;
lub(_, _) ->
    none.

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

%% Of two types that both hold, the one that tells the JIT more; an entry
%% this module does not read (`any', or a union it has no name for) tells it
%% nothing.
-spec refine(type() | any, type()) -> type().
refine({integer, A, B} = T, {integer, C, D}) ->
    %% Contradicting ranges hold only in code that does not run.
    case {lower(A, C), upper(B, D)} of
        {Lo, Hi} when Lo =< Hi -> {integer, Lo, Hi};
        _ -> T
    end;
refine(any, T) ->
    T;
refine(number, {integer, _, _} = T) ->
    T;
refine(number, float) ->
    float;
refine(list, T) when T =:= nil; T =:= cons -> T;
refine({bitstring, A}, {bitstring, B}) ->
    {bitstring, max(A, B)};
refine(T, _) ->
    T.
lower('-inf', C) -> C;
lower(A, C) -> max(A, C).
upper('+inf', D) -> D;
upper(B, D) -> min(B, D).

%% Versions 3 (OTP 27 and 28) and 4 (OTP 29) differ in where the flags for
%% the extra fields are: version 4 added a type bit below them.
-spec encode(3 | 4, type()) -> binary().
encode(Version, T) ->
    {Bits, Lower, Upper, Unit} =
        case T of
            atom -> {?ATOM, none, none, none};
            nil -> {?NIL, none, none, none};
            cons -> {?CONS, none, none, none};
            list -> {?NIL bor ?CONS, none, none, none};
            tuple -> {?TUPLE, none, none, none};
            map -> {?MAP, none, none, none};
            function -> {?FUN, none, none, none};
            float -> {?FLOAT, none, none, none};
            number -> {?FLOAT bor ?INTEGER, none, none, none};
            pid -> {?PID, none, none, none};
            port -> {?PORT, none, none, none};
            reference -> {?REFERENCE, none, none, none};
            {integer, Lo, Hi} -> {?INTEGER, finite(Lo), finite(Hi), none};
            {bitstring, U} -> {?BITSTRING, none, none, U}
        end,
    Shift = flag_shift(Version),
    Flags =
        flag(Lower, 1 bsl Shift) bor flag(Upper, 2 bsl Shift) bor flag(Unit, 4 bsl Shift),
    iolist_to_binary([
        <<(Bits bor Flags):16>>,
        [<<Lower:64/signed>> || Lower =/= none],
        [<<Upper:64/signed>> || Upper =/= none],
        [<<(Unit - 1):8>> || Unit =/= none]
    ]).
finite(B) when is_integer(B) -> B;
finite(_) -> none.
flag(none, _) -> 0;
flag(_, F) -> F.
flag_shift(3) -> 12;
flag_shift(4) -> 13.

-spec decode(3 | 4, binary()) -> type() | any.
decode(Version, <<Header:16, Extra/binary>>) ->
    Shift = flag_shift(Version),
    Bits = Header band ((1 bsl Shift) - 1),
    {Lower, Rest} = extra(Header band (1 bsl Shift), Extra),
    {Upper, Tail} = extra(Header band (2 bsl Shift), Rest),
    Unit =
        case {Header band (4 bsl Shift), Tail} of
            {0, _} -> 1;
            {_, <<U:8, _/binary>>} -> U + 1
        end,
    case Bits of
        ?ATOM -> atom;
        ?NIL -> nil;
        ?CONS -> cons;
        B when B =:= ?NIL bor ?CONS -> list;
        ?TUPLE -> tuple;
        ?MAP -> map;
        ?FUN -> function;
        ?FLOAT -> float;
        B when B =:= ?FLOAT bor ?INTEGER -> number;
        ?PID -> pid;
        ?PORT -> port;
        ?REFERENCE -> reference;
        ?INTEGER -> {integer, bound(Lower, '-inf'), bound(Upper, '+inf')};
        ?BITSTRING -> {bitstring, Unit};
        _ -> any
    end.
extra(0, B) -> {none, B};
extra(_, <<V:64/signed, B/binary>>) -> {V, B}.
bound(none, Default) -> Default;
bound(V, _) -> V.
