%%
%% Copyright (c) 2020 dushin.net
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

%%-----------------------------------------------------------------------------
%% @doc An escript and OTP library used to generate an
%% <a href="http://github.com/bettio/AtomVM">AtomVM</a> AVM file from a set of
%% files (beam files, previously built AVM files, or even arbitrary data files).
%% @end
%%-----------------------------------------------------------------------------
-module(packbeam).

%% API exports (for backwards compatibility)
-export([create/2, create/4, list/1, delete/3]).
%% escript
-export([main/1]).

%%
%% Public API
%%

%%-----------------------------------------------------------------------------
%% @doc     Deprecated.  Use the packbeam_api module, instead.
%% @end
%%-----------------------------------------------------------------------------
create(OutputPath, InputPaths) ->
    io:format("WARNING.  packbeam:create/2 is deprecated.  Use packbeam_api::create/2 instead.~n"),
    packbeam_api:create(OutputPath, InputPaths).

%%-----------------------------------------------------------------------------
%% @doc     Deprecated.  Use the packbeam_api module, instead.
%% @end
%%-----------------------------------------------------------------------------
create(OutputPath, InputPaths, Prune, StartModule) ->
    io:format("WARNING.  packbeam:create/4 is deprecated.  Use packbeam_api::create/3 instead.~n"),
    packbeam_api:create(OutputPath, InputPaths, Prune, StartModule).

%%-----------------------------------------------------------------------------
%% @doc     Deprecated.  Use the packbeam_api module, instead.
%% @end
%%-----------------------------------------------------------------------------
list(InputPath) ->
    io:format("WARNING.  packbeam:list/1 is deprecated.  Use packbeam_api::list/1 instead.~n"),
    packbeam_api:list(InputPath).

%%-----------------------------------------------------------------------------
%% @doc     Deprecated.  Use the packbeam_api module, instead.
%% @end
%%-----------------------------------------------------------------------------
delete(OutputPath, InputPath, Names) ->
    io:format("WARNING.  packbeam:delete/3 is deprecated.  Use packbeam_api::delete/3 instead.~n"),
    packbeam_api:delete(OutputPath, InputPath, Names).

%%
%% escript entrypoint
%%

%% @hidden
main(Argv) ->
    {Opts, Args} = parse_args(Argv),
    case length(Args) of
        0 ->
            print_help(),
            erlang:halt(255);
        _ ->
            [Command | ArgsRest] = Args,
            try
                case Command of
                    "create" ->
                        erlang:halt(do_create(Opts, ArgsRest));
                    "list" ->
                        erlang:halt(do_list(Opts, ArgsRest));
                    "extract" ->
                        erlang:halt(do_extract(Opts, ArgsRest));
                    "delete" ->
                        erlang:halt(do_delete(Opts, ArgsRest));
                    "version" ->
                        io:format("~s~n", [get_version()]),
                        erlang:halt(0);
                    "help" ->
                        print_help(),
                        erlang:halt(0);
                    _ ->
                        io:format("packbeam: command must be one of create|list|delete|help~n"),
                        print_help(),
                        erlang:halt(255)
                end
            catch
                _:Exception:S ->
                    io:format("packbeam: caught exception: ~p~n", [Exception]),
                    io:format("Stacktrace: ~n~p~n", [S]),
                    print_help(),
                    erlang:halt(255)
            end
    end.

%%
%% escript internal functions
%%

%% @private
print_help() ->
    io:format(
        "~n"
        "packbeam version ~s~n"
        "~n"
        "Syntax:~n"
        "    packbeam <sub-command> <options> <args>~n"
        "~n"
        "The following sub-commands are supported:~n"
        "~n"
        "    create <options> <output-avm-file> [<input-file>]+~n"
        "        where:~n"
        "           <output-avm-file> is the output AVM file,~n"
        "           [<input-file>]+ is a list of one or more input files,~n"
        "           and <options> are among the following:~n"
        "              [--prune|-p]           Prune dependencies~n"
        "              [--start|-s <module>]  Start module~n"
        "              [--remove_lines|-r]    Remove line number information from AVM files~n"
        "~n"
        "    list <options> <avm-file>~n"
        "        where:~n"
        "           <avm-file> is an AVM file,~n"
        "           and <options> are among the following:~n"
        "               [--format|-f csv|bare|default]  Format output~n"
        "~n"
        "    extract <options> <avm-file> [<element>]*~n"
        "        where:~n"
        "           <avm-file> is an AVM file,~n"
        "           [<element>]+ is a list of one or more elements to extract~n"
        "               (if empty, then extract all elements)~n"
        "           and <options> are among the following:~n"
        "               [--out|-o <output-directory>]   Output directory into which to write elements~n"
        "               (if unspecified, use the current working directory)~n"
        "~n"
        "    delete <options> <avm-file> [<element>]+~n"
        "        where:~n"
        "           <avm-file> is an AVM file,~n"
        "           [<element>]+ is a list of one or more elements to delete,~n"
        "           and <options> are among the following:~n"
        "               [--out|-o <output-avm-file>]    Output AVM file~n"
        "~n"
        "    version~n"
        "        Print version and exit~n"
        "~n"
        "    help~n"
        "        Print this help~n"
        "~n",
        [get_version()]
    ).

%% @private
get_version() ->
    case application:load(atomvm_packbeam) of
        ok ->
            case lists:keyfind(atomvm_packbeam, 1, application:loaded_applications()) of
                {_, _, Version} ->
                    Version;
                false ->
                    "Error!  Unable to find atomvm_packbeam in loaded applications"
            end;
        {error, _Reason} ->
            "Error!  Unable to load atomvm_packbeam application"
    end.

%% @private
do_create(Opts, Args) ->
    validate_args(create, Opts, Args),
    [OutputFile | InputFiles] = Args,
    ok = packbeam_api:create(
        OutputFile,
        InputFiles,
        #{
            prune => maps:get(prune, Opts, false),
            start_module => maps:get(start_module, Opts, undefined),
            include_lines => not maps:get(remove_lines, Opts, false)
        }
    ),
    0.

%% @private
do_list(Opts, Args) ->
    validate_args(list, Opts, Args),
    [InputFile | _] = Args,
    Modules = packbeam_api:list(InputFile),
    print_modules(Modules, maps:get(format, Opts, undefined)),
    0.

%% @private
do_extract(Opts, Args) ->
    validate_args(extract, Opts, Args),
    [InputFile | Rest] = Args,
    OutputDir = maps:get(output, Opts, "."),
    ok = packbeam_api:extract(InputFile, Rest, OutputDir),
    0.

%% @private
do_delete(Opts, Args) ->
    validate_args(delete, Opts, Args),
    [InputFile | _] = Args,
    OutputFile = maps:get(output, Opts, InputFile),
    packbeam_api:delete(OutputFile, InputFile, Args),
    0.

%% @private
validate_args(create, _Opts, [OutputPath | _Rest] = _Args) ->
    case filelib:is_dir(OutputPath) of
        true ->
            throw(io_lib:format("Output file (~p) is a directory", [OutputPath]));
        _ ->
            ok
    end;
validate_args(create, _Opts, [] = _Args) ->
    throw("Missing output file option");
%%
validate_args(list, _Opts, [InputPath | _Rest] = _Args) ->
    case not filelib:is_file(InputPath) of
        true ->
            throw(io_lib:format("Input file (~p) does not exist", [InputPath]));
        _ ->
            ok
    end;
validate_args(list, _Opts, [] = _Args) ->
    throw("Missing input option");
%%
validate_args(extract, _Opts, [InputPath | _Rest] = _Args) ->
    case not filelib:is_file(InputPath) of
        true ->
            throw(io_lib:format("Input file (~p) does not exist", [InputPath]));
        _ ->
            ok
    end;
validate_args(extract, _Opts, [] = _Args) ->
    throw("Missing input option");
%%
validate_args(delete, _Opts, [InputPath | _Rest] = _Args) ->
    case not filelib:is_file(InputPath) of
        true ->
            throw(io_lib:format("Input file (~p) does not exist", [InputPath]));
        _ ->
            ok
    end;
validate_args(delete, _Opts, [] = _Args) ->
    throw("Missing input option").

%% @private
print_modules(Modules, "csv" = Format) ->
    io:format("MODULE_NAME,IS_BEAM,IS_ENTRYPOINT,SIZE_BYTES~n"),
    lists:foreach(
        fun(Module) -> print_module(Module, Format) end,
        Modules
    );
print_modules(Modules, Format) ->
    lists:foreach(
        fun(Module) -> print_module(Module, Format) end,
        Modules
    ).

%% @private
print_module(ParsedFile, undefined) ->
    print_module(ParsedFile, "default");
print_module(ParsedFile, "default") ->
    Name = packbeam_api:get_element_name(ParsedFile),
    Data = packbeam_api:get_element_data(ParsedFile),
    io:format(
        "~s~s [~p]~n", [
            Name,
            case packbeam_api:is_entrypoint(ParsedFile) of
                true -> " *";
                _ -> ""
            end,
            byte_size(Data)
        ]
    );
print_module(ParsedFile, "csv") ->
    Name = packbeam_api:get_element_name(ParsedFile),
    Data = packbeam_api:get_element_data(ParsedFile),
    io:format(
        "~s,~p,~p,~p~n", [
            Name,
            packbeam_api:is_beam(ParsedFile),
            packbeam_api:is_entrypoint(ParsedFile),
            byte_size(Data)
        ]
    );
print_module(ParsedFile, "bare") ->
    Name = packbeam_api:get_element_name(ParsedFile),
    io:format(
        "~s~n", [
            Name
        ]
    );
print_module(_ParsedFile, Format) ->
    throw({error, {unsupported_format, Format}}).

%% @private
parse_args(Argv) ->
    parse_args(Argv, {#{}, []}).

%% @private
parse_args([], {Opts, Args}) ->
    {Opts, lists:reverse(Args)};
parse_args(["-out", Path | T], {Opts, Args}) ->
    io:format("WARNING.  Deprecated option.  Use --out instead.~n"),
    parse_args(["--out", Path | T], {Opts, Args});
parse_args(["-o", Path | T], {Opts, Args}) ->
    parse_args(["--out", Path | T], {Opts, Args});
parse_args(["--out", Path | T], {Opts, Args}) ->
    parse_args(T, {Opts#{output => Path}, Args});
parse_args(["-prune" | T], {Opts, Args}) ->
    io:format("WARNING.  Deprecated option.  Use --prune instead.~n"),
    parse_args(["--prune" | T], {Opts, Args});
parse_args(["-p" | T], {Opts, Args}) ->
    parse_args(["--prune" | T], {Opts, Args});
parse_args(["--prune" | T], {Opts, Args}) ->
    parse_args(T, {Opts#{prune => true}, Args});
parse_args(["-start", Module | T], {Opts, Args}) ->
    io:format("WARNING.  Deprecated option.  Use --start instead.~n"),
    parse_args(["--start", Module | T], {Opts, Args});
parse_args(["-s", Module | T], {Opts, Args}) ->
    parse_args(["--start", Module | T], {Opts, Args});
parse_args(["--start", Module | T], {Opts, Args}) ->
    parse_args(T, {Opts#{start_module => list_to_atom(Module)}, Args});
parse_args(["-r" | T], {Opts, Args}) ->
    parse_args(["--remove_lines" | T], {Opts, Args});
parse_args(["--remove_lines" | T], {Opts, Args}) ->
    parse_args(T, {Opts#{remove_lines => true}, Args});
parse_args(["-format", Format | T], {Opts, Args}) ->
    io:format("WARNING.  Deprecated option.  Use --format instead.~n"),
    parse_args(["--format", Format | T], {Opts, Args});
parse_args(["-f", Format | T], {Opts, Args}) ->
    parse_args(["--format", Format | T], {Opts, Args});
parse_args(["--format", Format | T], {Opts, Args}) ->
    parse_args(T, {Opts#{format => Format}, Args});
parse_args([H | T], {Opts, Args}) ->
    parse_args(T, {Opts, [H | Args]}).
