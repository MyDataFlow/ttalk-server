%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc A set of benchmarks comparing expat_sax_parser, erlsom and exml.
%%%
%%% @end
%%% Created : 15 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(benchmarks).

-export([run/2,
         run_exml/3,
         run_erlsom/3,
         run_xmerl/3]).

run(Path, N) ->
    application:start(exml),
    {ok, XML} = file:read_file(Path),
    [apply(?MODULE, F, [XML, N, []]) || F <- [run_exml, run_erlsom, run_xmerl]],
    application:stop(exml).

run_exml(_, 0, Acc) ->
    stats(exml, Acc);
run_exml(XML, N, Acc) ->
    T = now(),
    exml:parse(XML),
    Diff = timer:now_diff(now(), T),
    run_exml(XML, N-1, [Diff | Acc]).


run_xmerl(_, 0, Acc) ->
    stats(xmerl, Acc);
run_xmerl(XML, N, Acc) ->
    T = now(),
    {ok, EventState, _Rest} = xmerl_sax_parser:stream(XML, [{event_fun, fun(E, _, XAcc) -> [E | XAcc] end},
                                                            {event_state, []}]),
    lists:reverse(EventState),
    Diff = timer:now_diff(now(), T),
    run_xmerl(XML, N-1, [Diff | Acc]).


run_erlsom(_, 0, Acc) ->
    stats(erlsom, Acc);
run_erlsom(XML, N, Acc) ->
    T = now(),
    {ok, Events, _} = erlsom:parse_sax(XML, [], fun(E, XAcc) -> [E | XAcc] end),
    lists:reverse(Events),
    Diff = timer:now_diff(now(), T),
    run_erlsom(XML, N-1, [Diff | Acc]).


stats(Type, Acc) ->
    Avg = avg(Acc),
    io:format("~w~nTotal: ~w~n"
              "Avg: ~w~n"
              "Std dev.: ~w~n",
              [Type, total(Acc), Avg, std_dev(Acc, Avg)]).

total(Times) ->
    lists:sum(Times).

avg(Times) ->
    total(Times)/length(Times).

std_dev(Times, Avg) ->
    Sums = lists:foldl(
             fun(V, Acc) -> D = V - Avg, Acc + (D * D) end,
             0, Times),
    math:sqrt(Sums / (length(Times) - 1)).
