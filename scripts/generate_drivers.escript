#!/usr/bin/env escript
%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% Generates src/packbeam_drivers.hrl from an AtomVM checkout:
%%
%%     scripts/generate_drivers.escript /path/to/AtomVM
%%
%% Each platform registers its NIF collections and port drivers with
%% REGISTER_NIF_COLLECTION and REGISTER_PORT_DRIVER. A registration guarded by
%% a build option (a Kconfig entry on ESP32, a CMake option elsewhere) can be
%% switched off when the application never uses it.
-mode(compile).

platforms() ->
    [
        {"ESP32", ["src/platforms/esp32/components/avm_builtins"], kconfig,
            "src/platforms/esp32/components/avm_builtins/Kconfig"},
        {"STM32", ["src/platforms/stm32/src/lib"], cmake, "src/platforms/stm32/CMakeLists.txt"},
        {"RP2", ["src/platforms/rp2/src/lib"], cmake, "src/platforms/rp2/CMakeLists.txt"}
    ].

main([AtomVM]) ->
    main([
        AtomVM,
        filename:join([filename:dirname(escript:script_name()), "..", "src", "packbeam_drivers.hrl"])
    ]);
main([AtomVM, Out]) ->
    Common = sources(AtomVM, ["src/libAtomVM"]),
    Entries = lists:append([
        platform(AtomVM, Platform, Dirs, Toggles(AtomVM, Config), Common)
     || {Platform, Dirs, Kind, Config} <- platforms(),
        Toggles <- [toggles(Kind)]
    ]),
    ok = file:write_file(Out, render(AtomVM, lists:usort(Entries))),
    io:format("~s: ~b registrations~n", [Out, length(Entries)]);
main(_) ->
    io:format(
        standard_error, "usage: generate_drivers.escript <AtomVM checkout> [<output .hrl>]~n", []
    ),
    halt(1).

%% The build options, as the macro the C code tests mapped to how to set the
%% option when that macro must be undefined or defined.
toggles(kconfig) ->
    fun(AtomVM, Kconfig) ->
        {ok, B} = file:read_file(filename:join(AtomVM, Kconfig)),
        maps:from_list([
            {"CONFIG_" ++ N, {"CONFIG_" ++ N ++ "=n", "CONFIG_" ++ N ++ "=y"}}
         || [N] <- matches(B, "(?m)^\\s*config\\s+(\\w+)")
        ])
    end;
toggles(cmake) ->
    fun(AtomVM, CMakeLists) ->
        {ok, B} = file:read_file(filename:join(AtomVM, CMakeLists)),
        maps:from_list([
            {N, {N ++ "=OFF", N ++ "=ON"}}
         || [N] <- matches(B, "(?m)^\\s*option\\((\\w+)")
        ])
    end.

sources(AtomVM, Dirs) ->
    lists:append([
        [
            {F, element(2, file:read_file(F))}
         || F <- filelib:wildcard(filename:join([AtomVM, D, "*.c"]))
        ]
     || D <- Dirs
    ]).

platform(AtomVM, Platform, Dirs, Toggles, Common) ->
    Own = sources(AtomVM, Dirs),
    lists:append([registrations(Platform, Toggles, Src, Own ++ Common) || {_, Src} <- Own]).

registrations(Platform, Toggles, Src, Sources) ->
    Lines = string:split(binary_to_list(Src), "\n", all),
    walk(Lines, [], Platform, Toggles, Sources, []).

%% The preprocessor conditions enclosing each line decide which option guards
%% a registration: the innermost one naming a toggle.
walk([], _, _, _, _, Acc) ->
    lists:reverse(Acc);
walk([Line | Rest], Stack, Platform, Toggles, Sources, Acc) ->
    Trimmed = string:trim(Line),
    case directive(Trimmed) of
        {push, Cond} ->
            walk(Rest, [{Cond, false} | Stack], Platform, Toggles, Sources, Acc);
        pop ->
            walk(Rest, tl(Stack), Platform, Toggles, Sources, Acc);
        flip ->
            [{Cond, Negated} | Outer] = Stack,
            walk(Rest, [{Cond, not Negated} | Outer], Platform, Toggles, Sources, Acc);
        {elif, Cond} ->
            walk(Rest, [{Cond, false} | tl(Stack)], Platform, Toggles, Sources, Acc);
        none ->
            case registration(Trimmed) of
                {Kind, Name, Resolver} ->
                    Toggle = guard(Stack, Toggles),
                    MFAs =
                        case Kind of
                            nifs -> nif_names(Resolver, Sources);
                            port -> []
                        end,
                    Entry = {Platform, Kind, Name, Toggle, MFAs},
                    walk(Rest, Stack, Platform, Toggles, Sources, [Entry | Acc]);
                none ->
                    walk(Rest, Stack, Platform, Toggles, Sources, Acc)
            end
    end.

directive("#if" ++ _ = L) -> {push, L};
directive("#elif" ++ _ = L) -> {elif, L};
directive("#else" ++ _) -> flip;
directive("#endif" ++ _) -> pop;
directive(_) -> none.

registration(L) ->
    case
        re:run(
            L, "^REGISTER_(NIF_COLLECTION|PORT_DRIVER)\\((\\w+),\\s*\\w+,\\s*\\w+,\\s*(\\w+)\\)", [
                {capture, all_but_first, list}
            ]
        )
    of
        {match, ["NIF_COLLECTION", Name, Resolver]} -> {nifs, Name, Resolver};
        {match, ["PORT_DRIVER", Name, _]} -> {port, Name, none};
        nomatch -> none
    end.

guard([], _) ->
    none;
guard([{Cond, Negated} | Outer], Toggles) ->
    case [M || [M] <- matches(Cond, "\\b([A-Z][A-Z0-9_]+)\\b"), maps:is_key(M, Toggles)] of
        [M | _] ->
            {Off, On} = maps:get(M, Toggles),
            Inverted =
                re:run(Cond, "#ifndef\\s+" ++ M ++ "|!\\s*defined\\s*\\(?\\s*" ++ M) =/= nomatch,
            case Inverted =/= Negated of
                false -> Off;
                true -> On
            end;
        [] ->
            guard(Outer, Toggles)
    end.

%% A resolver compares the requested name with "module:function/arity"
%% literals, or matches "module:" first and then "function/arity".
nif_names(Resolver, Sources) ->
    Definition =
        "\\*\\s*" ++ Resolver ++ "\\s*\\(\\s*const\\s+char\\s*\\*\\s*\\w+\\s*\\)\\s*\\{(.*?)\\n\\}",
    Bodies = [
        Body
     || {_, Src} <- Sources,
        {match, [Body]} <- [re:run(Src, Definition, [dotall, {capture, all_but_first, list}])]
    ],
    case Bodies of
        [Body | _] ->
            Literals = [L || [L] <- matches(Body, "\"([^\"]*)\"")],
            lists:usort(names(Literals, none));
        [] ->
            error({resolver_not_found, Resolver})
    end.
matches(S, RE) ->
    case re:run(S, RE, [global, {capture, all_but_first, list}]) of
        {match, Ms} -> Ms;
        nomatch -> []
    end.

names([], _) ->
    [];
names([L | Ls], Prefix) ->
    case re:run(L, "^([^:/ ]+):(?:([^:/ ]+)/([0-9]+))?$", [{capture, all_but_first, list}]) of
        {match, [M]} ->
            names(Ls, M);
        {match, [M, F, A]} ->
            [{list_to_atom(M), list_to_atom(F), list_to_integer(A)} | names(Ls, Prefix)];
        nomatch when Prefix =/= none ->
            case re:run(L, "^([^:/ ]+)/([0-9]+)$", [{capture, all_but_first, list}]) of
                {match, [F, A]} ->
                    [
                        {list_to_atom(Prefix), list_to_atom(F), list_to_integer(A)}
                        | names(Ls, Prefix)
                    ];
                nomatch ->
                    names(Ls, Prefix)
            end;
        nomatch ->
            names(Ls, Prefix)
    end.

render(AtomVM, Entries) ->
    Revision = string:trim(os:cmd("git -C " ++ AtomVM ++ " describe --always")),
    [
        "%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>\n"
        "%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later\n"
        "\n"
        "%% Generated by scripts/generate_drivers.escript from AtomVM ",
        Revision,
        ".\n"
        "%% Do not edit: regenerate it from the AtomVM sources instead.\n"
        "%%\n"
        "%% {Platform, nifs | port, Name, SettingThatDisablesIt | none, NIFs}\n"
        "-define(DRIVER_REGISTRATIONS, [\n",
        lists:join(",\n", [entry(E) || E <- Entries]),
        "\n]).\n"
    ].

%% The layout erlfmt keeps, so the generated file passes the format check.
entry({Platform, Kind, Name, Setting, []}) ->
    io_lib:format("    {~p, ~p, ~p, ~p, []}", [Platform, Kind, Name, Setting]);
entry({Platform, Kind, Name, Setting, MFAs}) ->
    [
        io_lib:format("    {~p, ~p, ~p, ~p, [~n", [Platform, Kind, Name, Setting]),
        lists:join(",\n", [io_lib:format("        {~p, ~p, ~p}", [M, F, A]) || {M, F, A} <- MFAs]),
        "\n    ]}"
    ].
