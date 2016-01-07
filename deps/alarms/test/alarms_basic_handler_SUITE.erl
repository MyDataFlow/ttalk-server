%%%-------------------------------------------------------------------
%%% @author  Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Tests for alarms_basic_handler
%%%
%%% @end
%%% Created : 16 May 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(alarms_basic_handler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("alarms.hrl").

-import(test_utils, [init/0, fake_alarm/2]).

suite() ->
    [].

all() ->
    [test_summary,
     test_logging,
     test_unexpected].

init_per_suite(Config) ->
    init(),
    application:load(alarms),
    alarms_utils:set_cfg(handlers, [alarms_basic_handler]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(test_logging, Config) ->
    alarms:start(),
    Interval = alarms_utils:get_cfg({alarms_basic_handler, min_log_interval}),
    alarms_utils:set_cfg({alarms_basic_handler, min_log_interval}, 1),
    error_logger:add_report_handler(test_report_handler),
    [{min_log_interval, Interval}|Config];
init_per_testcase(test_unexpected, Config) ->
    alarms:start(),
    error_logger:add_report_handler(test_report_handler),
    Config;
init_per_testcase(_TC, Config) ->
    alarms:start(),
    Config.

end_per_testcase(test_logging, Config) ->
    error_logger:delete_report_handler(test_report_handler),
    alarms_utils:set_cfg(min_log_interval, ?config(min_log_interval, Config)),
    alarms:stop(),
    ok;
end_per_testcase(test_unexpected, Config) ->
    error_logger:delete_report_handler(test_report_handler),
    alarms:stop(),
    Config;
end_per_testcase(_TC, _Config) ->
    alarms:stop(),
    ok.

test_summary(_Config) ->
    fake_alarm(long_gc, gc1),
    Before = calendar:local_time(),
    fake_alarm(long_gc, gc2),
    fake_alarm(large_heap, lh),
    After = calendar:local_time(),

    {ok, LH} = alarms_basic_handler:get_alarm(large_heap),
    {1, TS1, lh} = LH,
    {ok, GC} = alarms_basic_handler:get_alarm(long_gc),
    {2, TS2, gc2} = GC,
    error = alarms_basic_handler:get_alarm(mnesia_fatal),
    [{large_heap, LH}, {long_gc, GC}] = alarms_basic_handler:get_alarms(),
    true = Before =< TS2 andalso TS2 =< TS1 andalso TS1 =< After.

test_logging(_Config) ->
    fake_alarm(long_gc, gc1),
    fake_alarm(long_gc, gc2),
    fake_alarm(large_heap, lh),
    fake_alarm(mnesia_error, {"error", []}), % should not be logged

    %% Wait until events are processed
    sys:get_status(alarm_handler),
    timer:sleep(1000),
    fake_alarm(long_gc, gc3),

    %% Wait until events are processed
    sys:get_status(alarm_handler),
    [GC1, LH, GC3] = test_report_handler:get_logs(),
    {info_report, _, {_, std_info,
                      [{alarm, long_gc}, {count,1}, {details, gc1}]}} = GC1,
    {info_report, _, {_, std_info,
                      [{alarm, large_heap}, {count,1}, {details, lh}]}} = LH,
    {info_report, _, {_, std_info,
                      [{alarm, long_gc}, {count,3}, {details, gc3}]}} = GC3,
    true.

test_unexpected(_Config) ->
    fake_alarm(unexpected, alarm),
    [] = alarms_basic_handler:get_alarms(),
    [Log] = test_report_handler:get_logs(),
    {error, _, {_, "Unexpected alarm ~p: ~p~n", [unexpected, alarm]}} = Log,
    true.
