#!/usr/bin/env escript

main([]) ->
    code:add_path("ebin"),
    {T1, _} = timer:tc(fun() -> punycode_encode_tests() end),
    io:format("punycode_encode_tests run in ~p ms~n", [T1]),
    {T2, _} = timer:tc(fun() -> idna_to_ascii_tests() end),
    io:format("idna_to_ascii run in ~p ms~n", [T2]).

punycode_encode_tests() ->
    test_each("test/punycode_*", fun (Test) ->
                {proplists:get_value(punycode, Test), punycode:encode(proplists:get_value(unicode, Test))}
        end).

idna_to_ascii_tests() ->
    test_each("test/idna_*", fun (Test) ->
                {proplists:get_value(output, Test), idna:to_ascii(idna_ucs:from_utf8(proplists:get_value(input, Test)))}
        end).

test_each(FilePattern, Fun) ->
    test_each(filelib:wildcard(FilePattern), Fun, 0).

test_each([], _, N) ->
    io:format("~p tests passed~n", [N]);
test_each([File | Files], Fun, N) ->
    {ok, Test} = file:consult(File),
    {Output, ReturnValue} = Fun(Test),
    case ReturnValue of
        Output ->
            test_each(Files, Fun, N + 1);
        _ ->
            io:format("~s~n", [File]),
            io:format("returned: ~p~n", [ReturnValue]),
            io:format("expected: ~p~n", [Output]),
            halt(1)
    end.
