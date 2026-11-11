%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(test_packbeam_prune).

-include_lib("eunit/include/eunit.hrl").

-define(BUILD_DIR, "_build/").
-define(TEST_BEAM_DIR, "_build/test/lib/atomvm_packbeam/test/").

packbeam_prune_simple_test() ->
    Files = [
        test_beam_path("a.beam"),
        test_beam_path("b.beam"),
        test_beam_path("c.beam"),
        test_beam_path("e.beam"),
        test_beam_path("f.beam")
    ],
    ?assertEqual(
        [
            {a, start, 0},
            {b, get_module, 0},
            {b, start, 0},
            {c, get_literal, 0},
            {c, test, 0},
            {e, b_calls_me, 0},
            {f, c_calls_me, 0}
        ],
        packbeam_prune:prune(Files, {a, start, 0}, "")
    ).

packbeam_prune_raise_undef_test() ->
    Files = [
        test_beam_path("a.beam")
    ],
    ?assertError({undef, b}, packbeam_prune:prune(Files, {a, start, 0}, "")).

test_beam_path(BeamFile) ->
    ?TEST_BEAM_DIR ++ BeamFile.
