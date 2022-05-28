%%
%% Copyright (c) dushin.net
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
-module(test_packbeam).

-include_lib("eunit/include/eunit.hrl").

-define(BUILD_DIR, "_build/").
-define(TEST_BEAM_DIR, "_build/test/lib/packbeam/test/").

packbeam_create_simple_test() ->
    AVMFile = dest_dir("packbeam_create_simple_test.avm"),
    ?assertMatch(ok, packbeam:create(
        AVMFile, [
            test_beam_path("a.beam"),
            test_beam_path("b.beam"),
            test_beam_path("c.beam"),
            test_beam_path("d.beam"),
            "test/priv/test.txt"
        ])
    ),

    ParsedFiles = packbeam:list(AVMFile),

    ?assert(is_list(ParsedFiles)),
    ?assertEqual(length(ParsedFiles), 5),

    [AFile, BFile, CFile, DFile, TextFile] = ParsedFiles,

    % io:format(user, "~p~n", [ParsedFiles]),

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

    ?assertMatch(d, get_module(DFile)),
    ?assertMatch("d.beam", get_module_name(DFile)),
    ?assert(is_beam(DFile)),
    ?assertNot(is_start(DFile)),
    ?assertNot(lists:member({start, 0}, get_exports(DFile))),

    ?assertNot(is_beam(TextFile)),
    ?assertMatch("test/priv/test.txt", get_module_name(TextFile)),

    ok.

packbeam_create_start_test() ->
    AVMFile = dest_dir("packbeam_create_start_test.avm"),
    ?assertMatch(ok,
        packbeam:create(
            AVMFile, [
                test_beam_path("a.beam"),
                test_beam_path("b.beam"),
                test_beam_path("c.beam"),
                test_beam_path("d.beam"),
                "test/priv/test.txt"
            ],
            false,
            b
        )
    ),

    ParsedFiles = packbeam:list(AVMFile),

    ?assert(is_list(ParsedFiles)),
    ?assertEqual(length(ParsedFiles), 5),

    [BFile | _] = ParsedFiles,

    % io:format(user, "~p~n", [ParsedFiles]),

    ?assertMatch(b, get_module(BFile)),
    ?assertMatch("b.beam", get_module_name(BFile)),
    ?assert(is_beam(BFile)),
    ?assert(is_start(BFile)),
    ?assert(lists:member({start, 0}, get_exports(BFile))),

    ok.

packbeam_create_prune_test() ->
    AVMFile = dest_dir("packbeam_create_prune_test.avm"),
    ?assertMatch(ok,
        packbeam:create(
            AVMFile, [
                test_beam_path("a.beam"),
                test_beam_path("b.beam"),
                test_beam_path("c.beam"),
                test_beam_path("d.beam"),
                test_beam_path("e.beam"),
                test_beam_path("f.beam"),
                "test/priv/test.txt"
            ],
            true,
            b
        )
    ),

    ParsedFiles = packbeam:list(AVMFile),

    ?assert(is_list(ParsedFiles)),
    ?assertEqual(length(ParsedFiles), 5),

    [BFile, CFile, EFile, FFile, TextFile] = ParsedFiles,

    % io:format(user, "~p~n", [ParsedFiles]),

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

    ok.

packbeam_prune_no_start_module_test() ->
    AVMFile = dest_dir("packbeam_prune_no_start_module_test.avm"),
    ?assertException(throw, _Term,
        packbeam:create(
            AVMFile, [
                test_beam_path("d.beam"),
                test_beam_path("e.beam"),
                test_beam_path("f.beam"),
                "test/priv/test.txt"
            ],
            true,
            b
        )
    ),

    ok.


packbeam_dest_fail_test() ->
    ?assertMatch({error, _},
        packbeam:create(
            "/tmp", [
                test_beam_path("a.beam"),
                test_beam_path("b.beam"),
                test_beam_path("c.beam"),
                test_beam_path("d.beam"),
                test_beam_path("e.beam"),
                "test/priv/test.txt"
            ]
        )
    ),

    ?assertMatch({error, _},
        packbeam:create(
            "/usr/should_not_allowed.avm", [
                test_beam_path("a.beam"),
                test_beam_path("b.beam"),
                test_beam_path("c.beam"),
                test_beam_path("d.beam"),
                test_beam_path("e.beam"),
                "test/priv/test.txt"
            ]
        )
    ),

    ?assertMatch({error, _},
        packbeam:create(
            "/likely_doesnt_exist/should_fail.avm", [
                test_beam_path("a.beam"),
                test_beam_path("b.beam"),
                test_beam_path("c.beam"),
                test_beam_path("d.beam"),
                test_beam_path("e.beam"),
                "test/priv/test.txt"
            ]
        )
    ),

    ok.

packbeam_src_fail_test() ->
    ?assertException(throw, _Term,
        packbeam:create(
            dest_dir("packbeam_src_fail_test.avm"), [
                test_beam_path("a.beam"),
                test_beam_path("b.beam"),
                test_beam_path("c.beam"),
                test_beam_path("d.beam"),
                test_beam_path("does_not_exist.beam"),
                "test/priv/test.txt"
            ]
        )
    ),

    ok.

packbeam_list_test() ->
    AVMFile = dest_dir("packbeam_list_test.avm"),
    ?assertMatch(ok,
        packbeam:create(
            AVMFile, [
                test_beam_path("a.beam"),
                test_beam_path("b.beam"),
                test_beam_path("c.beam"),
                test_beam_path("d.beam"),
                "test/priv/test.txt"
            ],
            true,
            b
        )
    ),

    packbeam:list(AVMFile),

    ?assertException(throw, _Term,
        packbeam:list(test_beam_path("d.beam"))
    ),

    ?assertException(throw, _Term,
        packbeam:list("/usr")
    ),

    ?assertException(throw, _Term,
        packbeam:list("/should_not_exist.txt")
    ),

    ok.

%%
%% helper functions
%%

dest_dir(AVMFile) ->
    ?BUILD_DIR ++ AVMFile.

test_beam_path(BeamFile) ->
    ?TEST_BEAM_DIR ++ BeamFile.

get_module(ParsedFile) ->
    proplists:get_value(module, ParsedFile).

get_module_name(ParsedFile) ->
    proplists:get_value(module_name, ParsedFile).

get_exports(ParsedFile) ->
    proplists:get_value(exports, proplists:get_value(chunk_refs, ParsedFile)).

is_start(ParsedFile) ->
    proplists:get_value(flags, ParsedFile) band 16#01 == 16#01.

is_beam(ParsedFile) ->
    proplists:get_value(flags, ParsedFile) band 16#02 == 16#02.
