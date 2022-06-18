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

%%-----------------------------------------------------------------------------
%% @doc An escript and OTP library used to generate an
%% <a href="http://github.com/bettio/AtomVM">AtomVM</a> AVM file from a set of
%% files (beam files, previously built AVM files, or even arbitrary data files).
%% @end
%%-----------------------------------------------------------------------------
-module(packbeam).

%% API exports
-export([create/4, create/2, list/1, delete/3]).
%% escript
-export([main/1]).

-define(AVM_HEADER,
    16#23, 16#21, 16#2f, 16#75,
    16#73, 16#72, 16#2f, 16#62,
    16#69, 16#6e, 16#2f, 16#65,
    16#6e, 16#76, 16#20, 16#41,
    16#74, 16#6f, 16#6d, 16#56,
    16#4d, 16#0a, 16#00, 16#00
).
-define(ALLOWED_CHUNKS, [
    "AtU8", "Code", "ExpT", "LocT", "ImpT", "LitU", "FunT", "StrT", "LitT"
]).
-define(BEAM_START_FLAG, 1).
-define(BEAM_CODE_FLAG, 2).
-define(NORMAL_FILE_FLAG, 4).

-type path() :: string().
-type parsed_file() :: [{string(), term()}].

%%
%% Public API
%%

%%-----------------------------------------------------------------------------
%% @param   OutputPath the path to write the AVM file
%% @param   InputPaths a list of paths of beam files, AVM files, or normal data files
%% @returns ok if the file was created.
%% @throws  Reason::string()
%% @doc     Create an AVM file.
%%
%%          Equivalent to create(OutputPath, InputPaths, false).
%% @end
%%-----------------------------------------------------------------------------
-spec create(path(), [path()]) -> ok | {error, Reason :: term()}.
create(OutputPath, InputPaths) ->
    create(OutputPath, InputPaths, false, undefined).

%%-----------------------------------------------------------------------------
%% @param   OutputPath the path to write the AVM file
%% @param   InputPaths a list of paths of beam files, AVM files, or normal data files
%% @param   Prune whether to prune the archive.  Without pruning, all found BEAM files are included
%%          in the output AVM file.  With pruning, then packbeam will attempt to
%%          determine which BEAM files are needed to run the application, depending
%%          on which modules are (transitively) referenced from the AVM entrypoint.
%% @returns ok if the file was created.
%% @throws  Reason::string()
%% @doc     Create an AVM file.
%%
%%          This function will create an AVM file at the location specified in
%%          OutputPath, using the input files specified in InputPaths.
%% @end
%%-----------------------------------------------------------------------------
-spec create(path(), [path()], Prune :: boolean(), StartModule :: module() | undefined) ->
    ok | {error, Reason :: term()}.
create(OutputPath, InputPaths, Prune, StartModule) ->
    ParsedFiles = parse_files(InputPaths, StartModule),
    write_packbeam(
        OutputPath,
        case Prune of
            true -> prune(ParsedFiles);
            _ -> ParsedFiles
        end
    ).

%%-----------------------------------------------------------------------------
%% @param   InputPath the AVM file from which to list elements
%% @returns list of parsed file data
%% @throws  Reason::string()
%% @doc     List the contents of an AVM file.
%%
%%          This function will list the contents of an AVM file at the
%%          location specified in InputPath.
%% @end
%%-----------------------------------------------------------------------------
-spec list(path()) -> [parsed_file()].
list(InputPath) ->
    case file_type(InputPath) of
        avm ->
            parse_file(InputPath);
        _ ->
            throw(io_lib:format("Expected AVM file: ~p", [InputPath]))
    end.

%%-----------------------------------------------------------------------------
%% @param   InputPath the AVM file from which to delete elements
%% @param   OutputPath the path to write the AVM file
%% @param   Names a list of elements from the source AVM file to delete
%% @returns ok if the file was created.
%% @throws  Reason::string()
%% @doc     Delete selected elements of an AVM file.
%%
%%          This function will delete elements of an AVM file at the location specified in
%%          InputPath, specified by the supplied list of names.  The output AVM file
%%          is written to OutputPath, which may be the same as InputPath.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(path(), path(), [path()]) -> ok | {error, Reason :: term()}.
delete(OutputPath, InputPath, Names) ->
    case file_type(InputPath) of
        avm ->
            ParsedFiles = parse_file(InputPath),
            write_packbeam(OutputPath, remove_names(Names, ParsedFiles));
        _ ->
            throw(io_lib:format("Expected AVM file: ~p", [InputPath]))
    end.

%%
%% Internal API functions
%%

%% @private
parse_files(InputPaths, StartModule) ->
    Files = lists:foldl(
        fun(InputPath, Accum) ->
            Accum ++ parse_file(InputPath)
        end,
        [],
        InputPaths
    ),
    case StartModule of
        undefined ->
            Files;
        _ ->
            reorder_start_module(StartModule, Files)
    end.

%% @private
parse_file(InputPath) ->
    parse_file(file_type(InputPath), InputPath, load_file(InputPath)).

%% @private
file_type(InputPath) ->
    case ends_with(InputPath, ".beam") of
        true ->
            beam;
        _ ->
            case ends_with(InputPath, ".avm") of
                true -> avm;
                _ -> normal
            end
    end.

%% @private
load_file(Path) ->
    case file:read_file(Path) of
        {ok, Data} ->
            Data;
        {error, Reason} ->
            throw(io_lib:format("Unable to load file ~s.  Reason: ~p", [Path, Reason]))
    end.

%% @private
prune(ParsedFiles) ->
    case find_entrypoint(ParsedFiles) of
        false ->
            throw("No input beam files contain start/0 entrypoint");
        {value, Entrypoint} ->
            BeamFiles = lists:filter(fun is_beam/1, ParsedFiles),
            Modules = closure(Entrypoint, BeamFiles, [get_module(Entrypoint)]),
            filter_modules(Modules, ParsedFiles)
    end.

%% @private
find_entrypoint(ParsedFiles) ->
    lists:search(fun is_entrypoint/1, ParsedFiles).

%% @private
is_entrypoint(Flags) when is_integer(Flags) ->
    Flags band ?BEAM_START_FLAG =:= ?BEAM_START_FLAG;
is_entrypoint(ParsedFile) ->
    is_entrypoint(proplists:get_value(flags, ParsedFile)).

%% @private
is_beam(Flags) when is_integer(Flags) ->
    Flags band ?BEAM_CODE_FLAG =:= ?BEAM_CODE_FLAG;
is_beam(ParsedFile) ->
    is_beam(proplists:get_value(flags, ParsedFile)).

%% @private
closure(_Current, [], Accum) ->
    lists:reverse(Accum);
closure(Current, Candidates, Accum) ->
    CandidateModules = get_modules(Candidates),
    CurrentsImports = get_imports(Current),
    CurrentsAtoms = get_atoms(Current),
    DepModules = intersection(CurrentsImports ++ CurrentsAtoms, CandidateModules) -- Accum,
    lists:foldl(
        fun(DepModule, A) ->
            case lists:member(DepModule, A) of
                true ->
                    A;
                _ ->
                    NewCandidates = remove(DepModule, Candidates),
                    closure(get_parsed_file(DepModule, Candidates), NewCandidates, [DepModule | A])
            end
        end,
        Accum,
        DepModules
    ).

%% @private
remove(Module, ParsedFiles) ->
    [P || P <- ParsedFiles, Module =/= proplists:get_value(module, P)].

%% @private
get_imports(ParsedFile) ->
    Imports = proplists:get_value(imports, proplists:get_value(chunk_refs, ParsedFile)),
    lists:usort([M || {M, _F, _A} <- Imports]).

%% @private
get_atoms(ParsedFile) ->
    AtomsT = [
        Atom || {_Index, Atom} <- proplists:get_value(atoms, proplists:get_value(chunk_refs, ParsedFile))
    ],
    AtomsFromLiterals = get_atom_literals(proplists:get_value(uncompressed_literals, ParsedFile)),
    lists:usort(AtomsT ++ AtomsFromLiterals).

%% @private
get_atom_literals(undefined) ->
    [];
get_atom_literals(UncompressedLiterals) ->
    <<NumLiterals:32, Rest/binary>> = UncompressedLiterals,
    get_atom_literals(NumLiterals, Rest, []).

%% @private
get_atom_literals(0, <<"">>, Accum) ->
    Accum;
get_atom_literals(I, Data, Accum) ->
    <<Length:32, StartData/binary>> = Data,
    <<EncodedLiteral:Length/binary, Rest/binary>> = StartData,
    Literal = binary_to_term(EncodedLiteral),
    ExtractedAtoms = extract_atoms(Literal, []),
    get_atom_literals(I - 1, Rest, ExtractedAtoms ++ Accum).

%% @private
extract_atoms(Term, Accum) when is_atom(Term) ->
    [Term|Accum];
extract_atoms(Term, Accum) when is_tuple(Term) ->
    extract_atoms(tuple_to_list(Term), Accum);
extract_atoms(Term, Accum) when is_map(Term) ->
    extract_atoms(maps:to_list(Term), Accum);
extract_atoms([H|T], Accum) ->
    HeadAtoms = extract_atoms(H, []),
    extract_atoms(T, HeadAtoms ++ Accum);
extract_atoms(_Term, Accum) ->
    Accum.

%% @private
get_modules(ParsedFiles) ->
    [get_module(ParsedFile) || ParsedFile <- ParsedFiles].

%% @private
get_module(ParsedFile) ->
    proplists:get_value(module, ParsedFile).

%% @private
get_parsed_file(Module, ParsedFiles) ->
    {value, V} = lists:search(
        fun(ParsedFile) ->
            proplists:get_value(module, ParsedFile) =:= Module
        end,
        ParsedFiles
    ),
    V.

%% @private
intersection(A, B) ->
    lists:filter(
        fun(E) ->
            lists:member(E, B)
        end,
        A
    ).

%% @private
filter_modules(Modules, ParsedFiles) ->
    lists:filter(
        fun(ParsedFile) ->
            case is_beam(ParsedFile) of
                true ->
                    lists:member(get_module(ParsedFile), Modules);
                _ ->
                    true
            end
        end,
        ParsedFiles
    ).

%% @private
parse_file(beam, _InputPath, Data) ->
    {ok, Module, Chunks} = beam_lib:all_chunks(Data),
    {UncompressedChunks, UncompressedLiterals} = uncompress_literals(Chunks),
    FilteredChunks = filter_chunks(UncompressedChunks),
    {ok, Binary} = beam_lib:build_module(FilteredChunks),
    {ok, {Module, ChunkRefs}} = beam_lib:chunks(Data, [imports, exports, atoms]),
    Exports = proplists:get_value(exports, ChunkRefs),
    Flags =
        case lists:member({start, 0}, Exports) of
            true ->
                ?BEAM_CODE_FLAG bor ?BEAM_START_FLAG;
            _ ->
                ?BEAM_CODE_FLAG
        end,
    [
        [
            {module, Module},
            {module_name, io_lib:format("~s.beam", [atom_to_list(Module)])},
            {flags, Flags},
            {data, Binary},
            {chunk_refs, ChunkRefs},
            {uncompressed_literals, UncompressedLiterals}
        ]
    ];
parse_file(avm, InputPath, Data) ->
    case Data of
        <<?AVM_HEADER, AVMData/binary>> ->
            parse_avm_data(AVMData);
        _ ->
            throw(io_lib:format("Invalid AVM header: ~p", [InputPath]))
    end;
parse_file(normal, InputPath, Data) ->
    DataSize = byte_size(Data),
    [[{module_name, InputPath}, {flags, ?NORMAL_FILE_FLAG}, {data, <<DataSize:32, Data/binary>>}]].

%% @private
reorder_start_module(StartModule, Files) ->
    {StartProps, Other} =
        lists:partition(
            fun(Props) ->
                % io:format("Props: ~w~n", [Props]),
                case proplists:get_value(module, Props) of
                    StartModule ->
                        Flags = proplists:get_value(flags, Props),
                        case is_entrypoint(Flags) of
                            true -> true;
                            _ -> throw({start_module_not_start_beam, StartModule})
                        end;
                    _ ->
                        false
                end
            end,
            Files
        ),
    case StartProps of
        [] ->
            throw({start_module_not_found, StartModule});
        _ ->
            StartProps ++ Other
    end.

%% @private
parse_avm_data(AVMData) ->
    parse_avm_data(AVMData, []).

%% @private
parse_avm_data(<<"">>, Accum) ->
    lists:reverse(Accum);
parse_avm_data(<<0:32/integer, _AVMRest/binary>>, Accum) ->
    lists:reverse(Accum);
parse_avm_data(<<Size:32/integer, AVMRest/binary>>, Accum) ->
    SizeWithoutSizeField = Size - 4,
    case SizeWithoutSizeField =< erlang:byte_size(AVMRest) of
        true ->
            <<AVMBeamData:SizeWithoutSizeField/binary, AVMNext/binary>> = AVMRest,
            Beam = parse_beam(AVMBeamData, [], in_header, []),
            parse_avm_data(AVMNext, [Beam | Accum]);
        _ ->
            throw(
                io_lib:format("Invalid AVM data size: ~p (AVMRest=~p)", [
                    Size, erlang:byte_size(AVMRest)
                ])
            )
    end.

%% @private
parse_beam(<<Flags:32, _Reserved:32, Rest/binary>>, [], in_header, Accum) ->
    parse_beam(Rest, [], in_module_name, [{flags, Flags} | Accum]);
parse_beam(<<0:8, Rest/binary>>, Tmp, in_module_name, Accum) ->
    parse_beam(Rest, [], eat_padding, [{module_name, lists:reverse(Tmp)} | Accum]);
parse_beam(<<C:8, Rest/binary>>, Tmp, in_module_name, Accum) ->
    parse_beam(Rest, [C | Tmp], in_module_name, Accum);
parse_beam(<<0:8, Rest/binary>>, Tmp, eat_padding, Accum) ->
    parse_beam(Rest, Tmp, eat_padding, Accum);
parse_beam(Data, _Tmp, eat_padding, Accum) ->
    Flags = proplists:get_value(flags, Accum),
    case is_beam(Flags) orelse is_entrypoint(Flags) of
        true ->
            {ok, {Module, ChunkRefs}} = beam_lib:chunks(Data, [imports, exports, atoms]),
            [{module, Module}, {chunk_refs, ChunkRefs}, {data, Data} | Accum];
        _ ->
            [{data, Data} | Accum]
    end.

%% @private
uncompress_literals(Chunks) ->
    case proplists:get_value("LitT", Chunks) of
        undefined ->
            {Chunks, undefined};
        <<_Header:4/binary, Data/binary>> ->
            UncompressedData = zlib:uncompress(Data),
            {
                lists:keyreplace(
                    "LitT",
                    1,
                    Chunks,
                    {"LitU", UncompressedData}
                ),
                UncompressedData
            }
    end.

%% @private
write_packbeam(OutputFilePath, ParsedFiles) ->
    PackedData =
        [<<?AVM_HEADER>> | [pack_data(ParsedFile) || ParsedFile <- ParsedFiles]] ++
            [create_header(0, 0, <<"end">>)],
    file:write_file(OutputFilePath, PackedData).

%% @private
pack_data(ParsedFile) ->
    ModuleName = list_to_binary(proplists:get_value(module_name, ParsedFile)),
    Flags = proplists:get_value(flags, ParsedFile),
    Data = proplists:get_value(data, ParsedFile),
    HeaderSize = header_size(ModuleName),
    HeaderPadding = create_padding(HeaderSize),
    DataSize = byte_size(Data),
    DataPadding = create_padding(DataSize),
    Size = HeaderSize + byte_size(HeaderPadding) + DataSize + byte_size(DataPadding),
    Header = create_header(Size, Flags, ModuleName),
    <<Header/binary, HeaderPadding/binary, Data/binary, DataPadding/binary>>.

%% @private
header_size(ModuleName) ->
    4 + 4 + 4 + byte_size(ModuleName) + 1.

%% @private
create_header(Size, Flags, ModuleName) ->
    <<Size:32, Flags:32, 0:32, ModuleName/binary, 0:8>>.

%% @private
create_padding(Size) ->
    case Size rem 4 of
        0 -> <<"">>;
        K -> list_to_binary(lists:duplicate(4 - K, 0))
    end.

%% @private
ends_with(String, Suffix) ->
    string:find(String, Suffix, trailing) =:= Suffix.

%% @private
filter_chunks(Chunks) ->
    lists:filter(
        fun({Tag, _Data}) -> lists:member(Tag, ?ALLOWED_CHUNKS) end,
        Chunks
    ).

%% @private
remove_names(Names, ParsedFiles) ->
    lists:filter(
        fun(ParsedFile) ->
            ModuleName = proplists:get_value(module_name, ParsedFile),
            not lists:member(ModuleName, Names)
        end,
        ParsedFiles
    ).

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
                    "delete" ->
                        erlang:halt(do_delete(Opts, ArgsRest));
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
        "Syntax:~n"
        "    packbeam <sub-command> <options>~n"
        "~n"
        "The following sub-commands are supported:"
        "~n"
        "    create [-prune] [-start <module>] <output-avm-file> [<input-file>]+~n"
        "    list <input-avm-file-path>~n"
        "    delete [-out <output-avm-file-path>] <input-avm-file-path> [<name>]+~n"
        "    help  print this help"
        "~n"
    ).

%% @private
do_create(Opts, Args) ->
    validate_args(create, Opts, Args),
    [OutputFile | InputFiles] = Args,
    ok = create(
        OutputFile, InputFiles, maps:get(prune, Opts, false), maps:get(start, Opts, undefined)
    ),
    0.

%% @private
do_list(Opts, Args) ->
    validate_args(list, Opts, Args),
    [InputFile | _] = Args,
    Modules = list(InputFile),
    print_modules(Modules),
    0.

%% @private
do_delete(Opts, Args) ->
    validate_args(delete, Opts, Args),
    [InputFile | _] = Args,
    OutputFile = maps:get(output, Opts, InputFile),
    delete(OutputFile, InputFile, Args),
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
print_modules(Modules) ->
    lists:foreach(
        fun print_module/1,
        Modules
    ).

%% @private
print_module(ParsedFile) ->
    ModuleName = proplists:get_value(module_name, ParsedFile),
    Flags = proplists:get_value(flags, ParsedFile),
    Data = proplists:get_value(data, ParsedFile),
    io:format(
        "~s~s [~p]~n", [
            ModuleName,
            case Flags band ?BEAM_START_FLAG of
                ?BEAM_START_FLAG -> " *";
                _ -> ""
            end,
            byte_size(Data)
        ]
    ).

%% @private
parse_args(Argv) ->
    parse_args(Argv, {#{}, []}).

%% @private
parse_args([], {Opts, Args}) ->
    {Opts, lists:reverse(Args)};
parse_args(["-out", Path | T], {Opts, Args}) ->
    parse_args(T, {Opts#{output => Path}, Args});
parse_args(["-in", Path | T], {Opts, Args}) ->
    parse_args(T, {Opts#{input => Path}, Args});
parse_args(["-prune" | T], {Opts, Args}) ->
    parse_args(T, {Opts#{prune => true}, Args});
parse_args(["-start", Module | T], {Opts, Args}) ->
    parse_args(T, {Opts#{start => list_to_atom(Module)}, Args});
parse_args([H | T], {Opts, Args}) ->
    parse_args(T, {Opts, [H | Args]}).
