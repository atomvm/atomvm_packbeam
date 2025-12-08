%%
%% Copyright (c) 2025 Winford (Uncle Grumpy) <<winford@object.stream>>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(packbeam_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BUILD_DIR, "_build/test/").
-define(TEST_BEAM_DIR, "_build/test/lib/atomvm_packbeam/test/").
-define(PACKBEAM, "_build/test/bin/packbeam").

packbeam_create_simple_test() ->
    AVMFile = dest_dir("packbeam__create_simple_test.avm"),
    BEAM_A = test_beam_path("a.beam"),
    BEAM_B = test_beam_path("b.beam"),
    BEAM_C = test_beam_path("c.beam"),
    TXT = "test/priv/test.txt",
    Packbeam = filename:absname(?PACKBEAM),

    CMD = lists:flatten(
        lists:join(" ", [Packbeam, "create", AVMFile, BEAM_A, BEAM_B, BEAM_C, TXT])
    ),
    ?assertMatch("0", string:trim(os:cmd(CMD ++ "; echo $?"))),

    ParsedFiles = packbeam_api:list(AVMFile),

    ?assert(is_list(ParsedFiles)),
    ?assertEqual(length(ParsedFiles), 4),

    [AFile, BFile, CFile, TextFile] = ParsedFiles,

    ?assertMatch(a, get_module(AFile)),
    ?assertMatch("a.beam", get_module_name(AFile)),
    ?assert(is_beam(AFile)),
    ?assert(is_start(AFile)),
    ?assert(lists:member({start, 0}, get_exports(AFile))),

    ?assertMatch(b, get_module(BFile)),
    ?assertMatch("b.beam", get_module_name(BFile)),
    ?assert(is_beam(BFile)),
    ?assert(is_start(BFile)),
    ?assert(lists:member({start, 0}, get_exports(BFile))),

    ?assertMatch(c, get_module(CFile)),
    ?assertMatch("c.beam", get_module_name(CFile)),
    ?assert(is_beam(CFile)),
    ?assertNot(is_start(CFile)),
    ?assertNot(lists:member({start, 0}, get_exports(CFile))),

    ?assertNot(is_beam(TextFile)),
    ?assertMatch(TXT, get_module_name(TextFile)),

    file:delete(AVMFile),
    ok.

packbeam_create_start_test() ->
    AVMFile = dest_dir("packbeam_create_start_test.avm"),
    BEAM_A = test_beam_path("a.beam"),
    BEAM_B = test_beam_path("b.beam"),
    BEAM_C = test_beam_path("c.beam"),
    Packbeam = filename:absname(?PACKBEAM),

    CMD = lists:flatten(
        lists:join(" ", [Packbeam, "create", "--start", "b", AVMFile, BEAM_A, BEAM_B, BEAM_C])
    ),
    ?assertMatch("0", string:trim(os:cmd(CMD ++ "; echo $?"))),

    ParsedFiles = packbeam_api:list(AVMFile),

    ?assert(is_list(ParsedFiles)),
    ?assertEqual(length(ParsedFiles), 3),

    [BFile | _] = ParsedFiles,

    ?assertMatch(b, get_module(BFile)),
    ?assertMatch("b.beam", get_module_name(BFile)),
    ?assert(is_beam(BFile)),
    ?assert(is_start(BFile)),
    ?assert(lists:member({start, 0}, get_exports(BFile))),

    file:delete(AVMFile),
    ok.

packbeam_create_start_main_test() ->
    AVMFile = dest_dir("packbeam_create_start_main_test.avm"),
    BEAM_C = test_beam_path("c.beam"),
    BEAM_F = test_beam_path("f.beam"),
    BEAM_G = test_beam_path("g.beam"),
    Packbeam = filename:absname(?PACKBEAM),

    CMD = lists:flatten(
        lists:join(" ", [Packbeam, "create", "--start", "g", AVMFile, BEAM_C, BEAM_F, BEAM_G])
    ),
    ?assertMatch("0", string:trim(os:cmd(CMD ++ "; echo $?"))),

    ParsedFiles = packbeam_api:list(AVMFile),

    ?assert(is_list(ParsedFiles)),
    ?assertEqual(length(ParsedFiles), 3),

    [GFile | _] = ParsedFiles,

    ?assertMatch(g, get_module(GFile)),
    ?assertMatch("g.beam", get_module_name(GFile)),
    ?assert(is_beam(GFile)),
    ?assert(is_start(GFile)),
    ?assert(lists:member({main, 1}, get_exports(GFile))),
    ?assert(not lists:member({start, 0}, get_exports(GFile))),

    file:delete(AVMFile),
    ok.

packbeam_create_lib_test() ->
    AVMFile = dest_dir("packbeam_create_lib_test.avm"),
    BEAM_C = test_beam_path("c.beam"),
    BEAM_F = test_beam_path("f.beam"),
    BEAM_G = test_beam_path("g.beam"),
    Packbeam = filename:absname(?PACKBEAM),

    CMD = lists:flatten(
        lists:join(" ", [Packbeam, "create", "--lib", AVMFile, BEAM_C, BEAM_F, BEAM_G])
    ),
    ?assertMatch("0", string:trim(os:cmd(CMD ++ "; echo $?"))),

    ParsedFiles = packbeam_api:list(AVMFile),

    ?assert(is_list(ParsedFiles)),
    ?assertEqual(length(ParsedFiles), 3),

    [CFile, FFile, GFile] = ParsedFiles,

    ?assertMatch(g, get_module(GFile)),
    ?assertMatch("g.beam", get_module_name(GFile)),
    ?assert(is_beam(GFile)),
    ?assertNot(is_start(CFile)),
    ?assertNot(is_start(FFile)),
    ?assertNot(is_start(GFile)),
    ?assert(lists:member({main, 1}, get_exports(GFile))),
    ?assertNot(lists:member({start, 0}, get_exports(GFile))),

    file:delete(AVMFile),
    ok.

packbeam_create_lib_from_avm_test() ->
    AVMFile0 = dest_dir("packbeam_create_start_main_test.avm"),
    BEAM_C = test_beam_path("c.beam"),
    BEAM_F = test_beam_path("f.beam"),
    BEAM_G = test_beam_path("g.beam"),
    Packbeam = filename:absname(?PACKBEAM),

    CMD1 = lists:flatten(
        lists:join(" ", [Packbeam, "create", "--start", "g", AVMFile0, BEAM_C, BEAM_F, BEAM_G])
    ),
    ?assertMatch("0", string:trim(os:cmd(CMD1 ++ "; echo $?"))),

    AVMFile = dest_dir("packbeam_create_lib_test.avm"),
    BEAM_A = test_beam_path("a.beam"),
    CMD2 = lists:flatten(lists:join(" ", [Packbeam, "create", "--lib", AVMFile, BEAM_A, AVMFile0])),
    ?assertMatch("0", string:trim(os:cmd(CMD2 ++ "; echo $?"))),

    ParsedFiles = packbeam_api:list(AVMFile),

    ?assert(is_list(ParsedFiles)),
    ?assertEqual(length(ParsedFiles), 4),

    % g was written first in AVMFile0
    [AFile, GFile, CFile, FFile] = ParsedFiles,

    ?assertMatch(g, get_module(GFile)),
    ?assertMatch("g.beam", get_module_name(GFile)),
    ?assert(is_beam(GFile)),
    ?assertNot(is_start(AFile)),
    ?assertNot(is_start(GFile)),
    ?assertNot(is_start(CFile)),
    ?assertNot(is_start(FFile)),
    ?assert(lists:member({main, 1}, get_exports(GFile))),
    ?assertNot(lists:member({start, 0}, get_exports(GFile))),

    file:delete(AVMFile0),
    file:delete(AVMFile),
    ok.

packbeam_create_prune_test() ->
    AVMFile = dest_dir("packbeam_create_prune_test.avm"),
    BEAM_A = test_beam_path("a.beam"),
    BEAM_B = test_beam_path("b.beam"),
    BEAM_C = test_beam_path("c.beam"),
    BEAM_D = test_beam_path("d.beam"),
    BEAM_E = test_beam_path("e.beam"),
    BEAM_F = test_beam_path("f.beam"),
    TXT = "test/priv/test.txt",
    Packbeam = filename:absname(?PACKBEAM),

    CMD = lists:flatten(
        lists:join(" ", [
            Packbeam,
            "create",
            "--prune",
            "--start",
            "b",
            AVMFile,
            BEAM_A,
            BEAM_B,
            BEAM_C,
            BEAM_D,
            BEAM_E,
            BEAM_F,
            TXT
        ])
    ),
    ?assertMatch("0", string:trim(os:cmd(CMD ++ "; echo $?"))),

    ParsedFiles = packbeam_api:list(AVMFile),

    ?assert(is_list(ParsedFiles)),
    ?assertEqual(length(ParsedFiles), 5),

    [BFile, CFile, EFile, FFile, TextFile] = ParsedFiles,

    ?assertMatch(b, get_module(BFile)),
    ?assertMatch("b.beam", get_module_name(BFile)),
    ?assert(is_beam(BFile)),
    ?assert(is_start(BFile)),
    ?assert(lists:member({start, 0}, get_exports(BFile))),

    ?assertMatch(c, get_module(CFile)),
    ?assertMatch("c.beam", get_module_name(CFile)),
    ?assert(is_beam(CFile)),
    ?assertNot(is_start(CFile)),
    ?assertNot(lists:member({start, 0}, get_exports(CFile))),

    ?assertMatch(e, get_module(EFile)),
    ?assertMatch("e.beam", get_module_name(EFile)),
    ?assert(is_beam(EFile)),
    ?assertNot(is_start(EFile)),
    ?assertNot(lists:member({start, 0}, get_exports(EFile))),

    ?assertMatch(f, get_module(FFile)),
    ?assertMatch("f.beam", get_module_name(FFile)),
    ?assert(is_beam(FFile)),
    ?assertNot(is_start(FFile)),
    ?assertNot(lists:member({start, 0}, get_exports(FFile))),

    ?assertNot(is_beam(TextFile)),
    ?assertMatch("test/priv/test.txt", get_module_name(TextFile)),

    file:delete(AVMFile),
    ok.

packbeam_prune_no_start_module_test() ->
    AVMFile = dest_dir("packbeam_prune_no_start_module_test.avm"),
    BEAM_D = test_beam_path("d.beam"),
    BEAM_E = test_beam_path("e.beam"),
    BEAM_F = test_beam_path("f.beam"),
    TXT = "test/priv/test.txt",
    Packbeam = filename:absname(?PACKBEAM),

    CMD = lists:flatten(
        lists:join(" ", [
            Packbeam, "create", "--prune", "--start", "b", AVMFile, BEAM_D, BEAM_E, BEAM_F, TXT
        ])
    ),
    Fail = os:cmd(CMD),
    ?assertMatch(
        ok,
        expect_contains("packbeam: caught exception:", Fail)
    ),

    file:delete(AVMFile),
    ok.

packbeam_dest_fail_test() ->
    BEAM_A = test_beam_path("a.beam"),
    BEAM_B = test_beam_path("b.beam"),
    BEAM_F = test_beam_path("f.beam"),
    TXT = "test/priv/test.txt",
    Packbeam = filename:absname(?PACKBEAM),

    CMD1 = lists:flatten(
        lists:join(" ", [Packbeam, "create", "/tmp", BEAM_A, BEAM_B, BEAM_F, TXT])
    ),
    FAIL1 = os:cmd(CMD1),
    ?assertMatch(
        ok,
        expect_contains("packbeam: caught exception:", FAIL1)
    ),

    CMD2 = lists:flatten(
        lists:join(" ", [
            Packbeam, "create", "/usr/should_not_allowed.avm", BEAM_A, BEAM_B, BEAM_F, TXT
        ])
    ),
    FAIL2 = os:cmd(CMD2),
    ?assertMatch(
        ok,
        expect_contains("packbeam: caught exception:", FAIL2)
    ),

    CMD3 = lists:flatten(
        lists:join(" ", [
            Packbeam, "create", "/likely_doesnt_exist/should_fail.avm", BEAM_A, BEAM_B, BEAM_F, TXT
        ])
    ),
    FAIL3 = os:cmd(CMD3),
    ?assertMatch(
        ok,
        expect_contains("packbeam: caught exception:", FAIL3)
    ),

    ok.

packbeam_create_src_fail_test() ->
    Dest = dest_dir("packbeam_src_fail_test.avm"),
    BEAM_A = test_beam_path("a.beam"),
    BEAM_B = test_beam_path("b.beam"),
    BEAM_C = test_beam_path("c.beam"),
    BEAM_D = test_beam_path("d.beam"),
    NO_BEAM = test_beam_path("does_not_exist.beam"),
    TXT = "test/priv/test.txt",
    Packbeam = filename:absname(?PACKBEAM),

    CMD = lists:flatten(
        lists:join(" ", [
            Packbeam, "create", Dest, BEAM_A, BEAM_B, BEAM_C, BEAM_B, BEAM_C, BEAM_D, NO_BEAM, TXT
        ])
    ),
    FAIL = os:cmd(CMD),
    ?assertMatch(
        ok,
        expect_contains("packbeam: caught exception:", FAIL)
    ),

    ok.

packbeam_list_test() ->
    AVMFile = dest_dir("packbeam_list_test.avm"),
    BEAM_A = test_beam_path("a.beam"),
    BEAM_B = test_beam_path("b.beam"),
    BEAM_C = test_beam_path("c.beam"),
    BEAM_D = test_beam_path("d.beam"),
    TXT = "test/priv/test.txt",
    Packbeam = filename:absname(?PACKBEAM),

    CMD = lists:flatten(
        lists:join(" ", [
            Packbeam,
            "create",
            "--prune",
            "--start",
            "b",
            AVMFile,
            BEAM_A,
            BEAM_B,
            BEAM_C,
            BEAM_D,
            TXT
        ])
    ),
    ?assertMatch("0", string:trim(os:cmd(CMD ++ "; echo $?"))),

    LIST_CMD = lists:flatten(lists:join(" ", [Packbeam, "list", AVMFile])),
    List = os:cmd(LIST_CMD),

    ?assertMatch(
        nomatch,
        expect_contains("d.beam", List)
    ),

    ?assertMatch(
        ok,
        expect_contains("b.beam *", List)
    ),

    LIST_CMD1 = lists:flatten(lists:join(" ", [Packbeam, "list", "/tmp"])),
    FAIL1 = os:cmd(LIST_CMD1),
    ?assertMatch(
        ok,
        expect_contains("packbeam: caught exception:", FAIL1)
    ),

    LIST_CMD2 = lists:flatten(lists:join(" ", [Packbeam, "list", "/should_not_exist.txt"])),
    FAIL2 = os:cmd(LIST_CMD2),
    ?assertMatch(
        ok,
        expect_contains("packbeam: caught exception:", FAIL2)
    ),

    file:delete(AVMFile),
    ok.

packbeam_create_dependent_avm_test() ->
    AVMFile = dest_dir("packbeam_create_dependent_avm_test.avm"),
    BEAM_A = test_beam_path("a.beam"),
    BEAM_B = test_beam_path("b.beam"),
    BEAM_C = test_beam_path("c.beam"),
    BEAM_D = test_beam_path("d.beam"),
    TXT = "test/priv/test.txt",
    Packbeam = filename:absname(?PACKBEAM),

    CMD1 = lists:flatten(
        lists:join(" ", [Packbeam, "create", AVMFile, BEAM_A, BEAM_B, BEAM_C, BEAM_D, TXT])
    ),
    ?assertMatch("0", string:trim(os:cmd(CMD1 ++ "; echo $?"))),

    AVMFile2 = dest_dir("packbeam_create_dependent_avm_test2.avm"),
    BEAM_X = test_beam_path("x.beam"),
    CMD2 = lists:flatten(
        lists:join(" ", [Packbeam, "create", "--prune", "--start", "x", AVMFile2, BEAM_X, AVMFile])
    ),
    ?assertMatch("0", string:trim(os:cmd(CMD2 ++ "; echo $?"))),

    ParsedFiles = packbeam_api:list(AVMFile2),

    ?assert(is_list(ParsedFiles)),
    ?assertEqual(length(ParsedFiles), 5),

    [FirstFile | _Rest] = ParsedFiles,
    ?assertEqual(get_module(FirstFile), x),
    ?assert(is_beam(FirstFile)),
    ?assert(is_start(FirstFile)),

    ?assert(parsed_file_contains_module(x, ParsedFiles)),
    ?assert(parsed_file_contains_module(a, ParsedFiles)),
    ?assert(parsed_file_contains_module(b, ParsedFiles)),
    ?assert(parsed_file_contains_module(c, ParsedFiles)),

    file:delete(AVMFile),
    file:delete(AVMFile2),
    ok.

packbeam_extract_test() ->
    AVMFile = dest_dir("packbeam_extract_test.avm"),
    BEAM_A = test_beam_path("a.beam"),
    BEAM_B = test_beam_path("b.beam"),
    BEAM_C = test_beam_path("c.beam"),
    BEAM_D = test_beam_path("d.beam"),
    TXT = "test/priv/test.txt",
    Packbeam = filename:absname(?PACKBEAM),

    CMD1 = lists:flatten(
        lists:join(" ", [Packbeam, "create", AVMFile, BEAM_A, BEAM_B, BEAM_C, BEAM_D, TXT])
    ),
    ?assertMatch("0", string:trim(os:cmd(CMD1 ++ "; echo $?"))),

    ScratchDir = create_scratch_dir("packbeam_extract_test"),
    CMD2 = lists:flatten(lists:join(" ", [Packbeam, "extract", "--out", ScratchDir, AVMFile])),
    Extract1 = os:cmd(CMD2),
    ?assertMatch(ok, expect_contains("Writing to " ++ ScratchDir, Extract1)),

    ?assert(file_exists(ScratchDir ++ "/a.beam")),
    ?assert(file_exists(ScratchDir ++ "/b.beam")),
    ?assert(file_exists(ScratchDir ++ "/c.beam")),
    ?assert(file_exists(ScratchDir ++ "/d.beam")),
    ?assert(file_exists(ScratchDir ++ "/test/priv/test.txt")),

    ScratchDir2 = create_scratch_dir("packbeam_extract_test2"),
    CMD3 = lists:flatten(
        lists:join(" ", [
            Packbeam, "extract", "--out", ScratchDir2, AVMFile, "c.beam", "test/priv/test.txt"
        ])
    ),
    Extract2 = os:cmd(CMD3),
    ?assertMatch(ok, expect_contains("Writing to " ++ ScratchDir2, Extract2)),

    ?assert(not file_exists(ScratchDir2 ++ "/a.beam")),
    ?assert(not file_exists(ScratchDir2 ++ "/b.beam")),
    ?assert(file_exists(ScratchDir2 ++ "/c.beam")),
    ?assert(not file_exists(ScratchDir2 ++ "/d.beam")),
    ?assert(file_exists(ScratchDir2 ++ "/test/priv/test.txt")),

    ScratchDir3 = create_scratch_dir("packbeam_extract_test3"),
    CMD4 = lists:flatten(
        lists:join(" ", [
            Packbeam, "extract", "--out", ScratchDir3, AVMFile, "c.beam", "does-not-exist"
        ])
    ),
    Extract3 = os:cmd(CMD4),
    ?assertMatch(ok, expect_contains("Writing to " ++ ScratchDir3, Extract3)),

    ?assert(not file_exists(ScratchDir3 ++ "/a.beam")),
    ?assert(not file_exists(ScratchDir3 ++ "/b.beam")),
    ?assert(file_exists(ScratchDir3 ++ "/c.beam")),
    ?assert(not file_exists(ScratchDir3 ++ "/d.beam")),
    ?assert(not file_exists(ScratchDir3 ++ "/test/priv/test.txt")),

    CMD5 = lists:flatten(
        lists:join(" ", [Packbeam, "extract", "--out", ScratchDir3, AVMFile ++ "-garbage"])
    ),
    Garbage = os:cmd(CMD5),
    ?assertMatch(ok, expect_contains("packbeam: caught exception:", Garbage)),

    CMD6 = lists:flatten(
        lists:join(" ", [Packbeam, "extract", "--out", ?BUILD_DIR ++ "does-not-exist", AVMFile])
    ),
    BAD_OUTDIR = os:cmd(CMD6),
    ?assertMatch(ok, expect_contains("packbeam: caught exception:", BAD_OUTDIR)),

    file:delete(AVMFile),
    file:delete(ScratchDir ++ "/a.beam"),
    file:delete(ScratchDir ++ "/b.beam"),
    file:delete(ScratchDir ++ "/c.beam"),
    file:delete(ScratchDir ++ "/d.beam"),
    file:delete(ScratchDir ++ "/test/priv/test.txt"),
    file:delete(ScratchDir2 ++ "/c.beam"),
    file:delete(ScratchDir2 ++ "/test/priv/test.txt"),
    file:delete(ScratchDir3 ++ "/c.beam"),

    ok.

%%
%% helper functions
%%

file_exists(Path) ->
    filelib:is_file(Path).

dest_dir(AVMFile) ->
    filename:absname(?BUILD_DIR ++ AVMFile).

test_beam_path(BeamFile) ->
    filename:absname(?TEST_BEAM_DIR ++ BeamFile).

get_module(ParsedFile) ->
    packbeam_api:get_element_module(ParsedFile).

get_module_name(ParsedFile) ->
    packbeam_api:get_element_name(ParsedFile).

get_exports(ParsedFile) ->
    proplists:get_value(exports, proplists:get_value(chunk_refs, ParsedFile)).

is_start(ParsedFile) ->
    packbeam_api:is_entrypoint(ParsedFile).

is_beam(ParsedFile) ->
    packbeam_api:is_beam(ParsedFile).

parsed_file_contains_module(Module, ParsedFiles) ->
    lists:any(
        fun(ParsedFile) ->
            get_module(ParsedFile) =:= Module
        end,
        ParsedFiles
    ).

create_scratch_dir(Name) ->
    ok = filelib:ensure_dir(?BUILD_DIR ++ "/" ++ Name ++ "/" ++ "dummy"),
    ?BUILD_DIR ++ "/" ++ Name.

expect_contains(String, Output) ->
    case string:find(Output, String) of
        nomatch ->
            nomatch;
        _ ->
            ok
    end.
