%%%-------------------------------------------------------------------
%%% @author  Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Tests for alarms_folsom_handler
%%%
%%% @end
%%% Created : 16 May 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(alarms_folsom_handler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("alarms.hrl").

-import(test_utils, [init/0, fake_alarm/2]).

suite() ->
    [].

all() ->
    [test_metrics,
     test_unexpected].

init_per_suite(Config) ->
    init(),
    application:load(alarms),
    alarms_utils:set_cfg(handlers, [alarms_folsom_handler]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    alarms:start(),
    Config.

end_per_testcase(_TC, _Config) ->
    alarms:stop(),
    folsom:stop(),
    ok.

test_metrics(_Config) ->
    Before = alarms_utils:epoch_usec_to_local_time(
               folsom_utils:now_epoch_micro()),
    fake_alarm(long_gc, gc1),
    timer:sleep(100),
    fake_alarm(long_gc, gc2),
    timer:sleep(100),
    fake_alarm(large_heap, lh),
    After = alarms_utils:epoch_usec_to_local_time(
              folsom_utils:now_epoch_micro()),

    {2, 2, [GC2, GC1]} = alarms_folsom_handler:get_alarm(long_gc),
    {1, 1, [LH]} = alarms_folsom_handler:get_alarm(large_heap),
    {0, 0, []} = alarms_folsom_handler:get_alarm(mnesia_fatal),

    {GC_TS1, [{event, gc1}]} = GC1,
    {GC_TS2, [{event, gc2}]} = GC2,
    {LH_TS, [{event, lh}]} = LH,

    true = Before =< GC_TS1 andalso GC_TS1 =< GC_TS2 andalso GC_TS2 =< LH_TS
        andalso LH_TS =< After.

test_unexpected(_Config) ->
    fake_alarm(unexpected, alarm),
    [] = alarms_folsom_handler:get_alarms(),
    true.
