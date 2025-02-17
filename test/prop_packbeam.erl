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
-module(prop_packbeam).
-include_lib("proper/include/proper.hrl").

-define(BUILD_DIR, "_build/").
-define(TEST_BEAM_DIR, "_build/test/lib/atomvm_packbeam/test/").

%%
%% Call graph for modules a-f, x
%%
%%
%%          calls                   calls
%%      a (start) ----------> b (start) ---------> e
%%                            |
%%                            | calls (via atomic module name)
%%                            |
%%                            v
%%                            c -----------------> f
%%                              calls (via atom inside of literal)
%%
%%
%%      d  no one calls d :(
%%
%%
%%   x (start) ---> a (start)
%%

%%
%% Properties
%%

prop_simple_test() ->
    ?FORALL(
        ModulePaths,
        module_paths(),
        begin
            Modules = [Module || {Module, _Path} <- ModulePaths],
            Paths = [Path || {_Module, Path} <- ModulePaths],
            AVMFile = dest_dir("prop_simple_test.avm"),
            collect(
                Modules,
                begin
                    ok = packbeam_api:create(AVMFile, Paths),
                    ParsedFiles = packbeam_api:list(AVMFile),
                    modules_and_parsed_files_are_equivalent(Modules, ParsedFiles) andalso
                        all_beam_modules_are_properly_named(ParsedFiles) andalso
                        maybe_contains_start_beam(Modules, a, ParsedFiles) andalso
                        maybe_contains_start_beam(Modules, b, ParsedFiles)
                end
            )
        end
    ).

modules_and_parsed_files_are_equivalent(Modules, ParsedFiles) ->
    lists:sort(Modules) =:=
        lists:sort([get_module(ParsedFile) || ParsedFile <- get_beam_files(ParsedFiles)]).

all_beam_modules_are_properly_named(ParsedFiles) ->
    lists:all(
        fun(ParsedFile) ->
            Module = get_module(ParsedFile),
            get_module_name(ParsedFile) =:= (atom_to_list(Module) ++ ".beam")
        end,
        get_beam_files(ParsedFiles)
    ).

get_beam_files(ParsedFiles) ->
    [ParsedFile || ParsedFile <- ParsedFiles, is_beam(ParsedFile)].

maybe_contains_start_beam(Modules, Module, ParsedFiles) ->
    case lists:member(Module, Modules) of
        true ->
            is_start(find_parsed_file(Module, ParsedFiles));
        _ ->
            %% ignore
            true
    end.

find_parsed_file(Module, []) ->
    {parsed_file_not_found, Module};
find_parsed_file(Module, [ParsedFile | Rest]) ->
    case get_module(ParsedFile) of
        M when M =:= Module ->
            ParsedFile;
        _ ->
            find_parsed_file(Module, Rest)
    end.
%%
%% Helpers
%%

test_beam_path(Module) ->
    ?TEST_BEAM_DIR ++ atom_to_list(Module) ++ ".beam".

dest_dir(AVMFile) ->
    ?BUILD_DIR ++ AVMFile.

get_module(ParsedFile) ->
    packbeam_api:get_element_module(ParsedFile).

get_module_name(ParsedFile) ->
    packbeam_api:get_element_name(ParsedFile).

is_start(ParsedFile) ->
    packbeam_api:is_entrypoint(ParsedFile).

is_beam(ParsedFile) ->
    packbeam_api:is_beam(ParsedFile).

%%
%% Generators
%%

module_paths() ->
    ?LET(
        Modules,
        list(oneof([a, b, c, d, e, f])),
        remove_duplicates([{Module, test_beam_path(Module)} || Module <- Modules])
    ).

remove_duplicates([]) ->
    [];
remove_duplicates([H | T]) ->
    [H | [X || X <- remove_duplicates(T), X /= H]].
