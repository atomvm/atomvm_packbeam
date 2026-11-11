%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% AtomVM's NIF collections and port drivers, from packbeam_drivers.hrl.
%% CI checks that file matches the latest AtomVM release; regenerate it with
%% scripts/check_drivers.sh --update.
-module(packbeam_drivers).
-export([nifs/0, is_nif/1, suggestions/2]).

-include("packbeam_drivers.hrl").

%% @doc Whether AtomVM implements this function as a NIF on some platform.
-spec is_nif(mfa()) -> boolean().
is_nif(MFA) -> lists:member(MFA, nifs()).
%% @doc Every function AtomVM implements as a NIF on some platform.
-spec nifs() -> [mfa()].
nifs() -> lists:usort(lists:append([MFAs || {_, nifs, _, _, MFAs} <- ?DRIVER_REGISTRATIONS])).

%% @doc One line per build option that disables a port driver or NIF
%% collection that none of `Ports' or `Nifs' needs, by the platforms sharing it.
-spec suggestions(Ports :: [string()], Nifs :: [mfa()]) -> [io_lib:chars()].
suggestions(Ports, Nifs) ->
    Unused = [
        {{Setting, Kind, Name}, Platform}
     || {Platform, Kind, Name, Setting, MFAs} <- ?DRIVER_REGISTRATIONS,
        Setting =/= none,
        unused(Kind, Name, MFAs, Ports, Nifs)
    ],
    %% Platforms sharing an option share one line.
    Grouped = lists:foldl(
        fun({Key, Platform}, Acc) ->
            case lists:keyfind(Key, 1, Acc) of
                {Key, Platforms} -> lists:keyreplace(Key, 1, Acc, {Key, Platforms ++ [Platform]});
                false -> Acc ++ [{Key, [Platform]}]
            end
        end,
        [],
        Unused
    ),
    [
        io_lib:format("~s: ~s (no reachable ~s ~s)", [
            lists:join("/", Platforms), Setting, Name, what(Kind)
        ])
     || {{Setting, Kind, Name}, Platforms} <- Grouped
    ].

unused(port, Name, _, Ports, _) -> not lists:member(Name, Ports);
unused(nifs, _, MFAs, _, Nifs) -> not lists:any(fun(MFA) -> lists:member(MFA, Nifs) end, MFAs).

what(port) -> "port";
what(nifs) -> "NIFs".
