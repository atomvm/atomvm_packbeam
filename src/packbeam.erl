%%
%% Copyright (c) dushin.net
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of dushin.net nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%

%%-----------------------------------------------------------------------------
%% @doc An escript and OTP library used to generate an
%% <a href="http://github.com/bettio/AtomVM">AtomVM</a> AVM file from a set of
%% files (beam files, previously built AVM files, or even arbitrary data files).
%% @end
%%-----------------------------------------------------------------------------
-module(packbeam).

%% API exports
-export([create/2, list/1, delete/3]).
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
%%          This function will create an AVM file at the location specified in
%%          OutputPath, using the input files specified in InputPaths.
%% @end
%%-----------------------------------------------------------------------------
-spec create(path(), [path()]) -> ok | {error, Reason::term()}.
create(OutputPath, InputPaths) ->
    ParsedFiles = parse_files(InputPaths),
    write_packbeam(OutputPath, ParsedFiles).

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
-spec delete(path(), path(), [path()]) -> ok | {error, Reason::term()}.
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
parse_files(InputPaths) ->
    lists:foldl(
        fun(InputPath, Accum) ->
            Accum ++ parse_file(InputPath)
        end,
        [],
        InputPaths
    ).

%% @private
parse_file(InputPath) ->
    parse_file(file_type(InputPath), InputPath, load_file(InputPath)).

%% @private
file_type(InputPath) ->
    case ends_with(InputPath, ".beam") of
        true -> beam;
        _ ->
            case ends_with(InputPath, ".avm") of
                true -> avm;
                _ ->    normal
            end
    end.

%% @private
load_file(Path) ->
    case file:read_file(Path) of
        {ok, Data} -> Data;
        {error, Reason} ->
            throw(io_lib:format("Unable to load file ~s.  Reason: ~p", [Path, Reason]))
    end.

%% @private
parse_file(beam, _InputPath, Data) ->
    {ok, Module, Chunks} = beam_lib:all_chunks(Data),
    UncompressedChunks = uncompress_literals(Chunks),
    FilteredChunks = filter_chunks(UncompressedChunks),
    {ok, Binary} = beam_lib:build_module(FilteredChunks),
    beam_lib:all_chunks(Binary),
    {ok, {Module, ExportChunks}} = beam_lib:chunks(Data, [exports]),
    Exports = proplists:get_value(exports, ExportChunks),
    Flags = case lists:member({start, 0}, Exports) of
        true ->
            ?BEAM_CODE_FLAG bor ?BEAM_START_FLAG;
        _ ->
            ?BEAM_CODE_FLAG
    end,
    [[
        {module_name, io_lib:format("~s.beam", [atom_to_list(Module)])},
        {flags, Flags},
        {data, Binary}
    ]];
parse_file(avm, InputPath, Data) ->
    case Data of
        <<?AVM_HEADER, AVMData/binary>> ->
            parse_avm_data(AVMData);
        _ ->
            throw(io_lib:format("Invalid AVM header: ~p", [InputPath]))
    end;
parse_file(normal, InputPath, Data) ->
    DataSize = byte_size(Data),
    [[{module_name, InputPath}, {flags, 0}, {data, <<DataSize:32, Data/binary>>}]].

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
            throw(io_lib:format("Invalid AVM data size: ~p (AVMRest=~p)", [Size, erlang:byte_size(AVMRest)]))
    end.

%% @private
parse_beam(<<Flags:32, _Reserved:32, Rest/binary>>, [], in_header, Accum) ->
    parse_beam(Rest, [], in_module_name, [{flags, Flags} | Accum]);
parse_beam(<<0:8, Rest/binary>>, Tmp, in_module_name, Accum) ->
    parse_beam(Rest, [], eat_padding, [{module_name, lists:reverse(Tmp)} | Accum]);
parse_beam(<<C:8, Rest/binary>>, Tmp, in_module_name, Accum) ->
    parse_beam(Rest, [C|Tmp], in_module_name, Accum);
parse_beam(<<0:8, Rest/binary>>, Tmp, eat_padding, Accum) ->
    parse_beam(Rest, Tmp, eat_padding, Accum);
parse_beam(Data, _Tmp, eat_padding, Accum) ->
    %{ok, _Module, _Chunks} = beam_lib:all_chunks(Data),
    [{data, Data} | Accum].

%% @private
uncompress_literals(Chunks) ->
    case proplists:get_value("LitT", Chunks) of
        undefined ->
            Chunks;
        <<_Header:4/binary, Data/binary>> ->
            UncompressedData = zlib:uncompress(Data),
            lists:keyreplace(
                "LitT", 1, Chunks,
                {"LitT", UncompressedData}
            )
    end.

%% @private
write_packbeam(OutputFilePath, ParsedFiles) ->
    PackedData = [<<?AVM_HEADER>> | [pack_data(ParsedFile) || ParsedFile <- ParsedFiles]]
                    ++ [create_header(0, 0, <<"end">>)],
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
            [Command|ArgsRest] = Args,
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
                _:Exception:_S ->
                    io:format("packbeam: exception: ~s~n", [Exception]),
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
        "    * create -out <output-avm-file-path> [<input-file>]+~n"
        "    * list -in <input-avm-file-path>~n"
        "    * delete -in <input-avm-file-path> [-out <output-avm-file-path>] [<name>]+~n"
        "    * help  print this help"
        "~n"
    ).

%% @private
do_create(Opts, Args) ->
    validate_args(create, Opts, Args),
    ok = create(maps:get(output, Opts), Args),
    0.

%% @private
do_list(Opts, Args) ->
    validate_args(list, Opts, Args),
    Modules = list(maps:get(input, Opts)),
    print_modules(Modules),
    0.

%% @private
do_delete(Opts, Args) ->
    validate_args(delete, Opts, Args),
    delete(maps:get(output, Opts), maps:get(input, Opts, maps:get(output, Opts)), Args),
    0.

%% @private
validate_args(create, Opts, _Args) ->
    case maps:find(output, Opts) of
        error ->
            throw(io_lib:format("Missing output option"));
        {ok, OutputPath} ->
            case filelib:is_dir(OutputPath) of
                true ->
                    throw(io_lib:format("Output file (~p) is a directory", [OutputPath]));
                _ -> ok
            end
    end;
validate_args(list, Opts, _Args) ->
    case maps:find(input, Opts) of
        error ->
            throw(io_lib:format("Missing input option"));
        {ok, InputPath} ->
            case not filelib:is_file(InputPath) of
                true ->
                    throw(io_lib:format("Input file (~p) does not exist", [InputPath]));
                _ -> ok
            end
    end;
validate_args(delete, Opts, _Args) ->
    case maps:find(input, Opts) of
        error ->
            throw(io_lib:format("Missing input option"));
        {ok, InputPath} ->
            case not filelib:is_file(InputPath) of
                true ->
                    throw(io_lib:format("Input file (~p) does not exist", [InputPath]));
                _ -> ok
            end
    end.

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
            case Flags band ?BEAM_START_FLAG of ?BEAM_START_FLAG -> " *"; _ -> "" end,
            byte_size(Data)
        ]
    ).

%% @private
parse_args(Argv) ->
    parse_args(Argv, {#{}, []}).

%% @private
parse_args([], {Opts, Args}) ->
    {Opts, lists:reverse(Args)};
parse_args(["-out", Path|T], {Opts, Args}) ->
    parse_args(T, {Opts#{output => Path}, Args});
parse_args(["-in", Path|T], {Opts, Args}) ->
    parse_args(T, {Opts#{input => Path}, Args});
parse_args([H|T], {Opts, Args}) ->
    parse_args(T, {Opts, [H|Args]}).
