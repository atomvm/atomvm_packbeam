%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(prop_function_prune).
-include_lib("proper/include/proper.hrl").

%% Compare observable execution before and after rewriting, with generated
%% call-chain lengths and literal payloads (including arbitrary binary data).
prop_execution_equivalence() ->
    ?FORALL(
        {N, Value, Payload},
        {range(1, 8), integer(), binary()},
        begin
            Names = [list_to_atom("f" ++ integer_to_list(I)) || I <- lists:seq(1, N)],
            Bodies = [
                {function, 1, F, 0, [
                    {clause, 1, [], [], [
                        case I of
                            N -> erl_parse:abstract({Value, Payload});
                            _ -> {call, 1, {atom, 1, lists:nth(I + 1, Names)}, []}
                        end
                    ]}
                ]}
             || {I, F} <- lists:zip(lists:seq(1, N), Names)
            ],
            Forms = [
                {attribute, 1, module, pf_property},
                {attribute, 1, export, [{start, 0}, {dead, 0}]},
                {function, 1, start, 0, [
                    {clause, 1, [], [], [{call, 1, {atom, 1, hd(Names)}, []}]}
                ]},
                {function, 1, dead, 0, [
                    {clause, 1, [], [], [
                        erl_parse:abstract({discarded, lists:duplicate(64, dead)})
                    ]}
                ]}
                | Bodies
            ],
            {ok, pf_property, B} = compile:forms(Forms, [binary, no_line_info]),
            {[Trimmed], Report} = packbeam_prune:run([B], [], [{pf_property, start, 0}], #{}),
            {ok, {pf_property, [{exports, Exports}]}} = beam_lib:chunks(Trimmed, [exports]),
            Before = execute(B),
            After = execute(Trimmed),
            Before =:= After andalso After =:= {Value, Payload} andalso
                Exports =:= [{start, 0}] andalso maps:get(warnings, Report) =:= []
        end
    ).
execute(B) ->
    {module, pf_property} = code:load_binary(pf_property, "generated.beam", B),
    try
        pf_property:start()
    after
        code:delete(pf_property),
        code:purge(pf_property)
    end.
