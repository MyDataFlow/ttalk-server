%%%-------------------------------------------------------------------
%%% @author  Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Tests for alarms_server
%%%
%%% @end
%%% Created : 15 May 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(alarms_server_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-import(test_utils, [init/0, fake_event/1]).

suite() ->
    [].

all() ->
    [test_monitor,
     test_mnesia,
     test_alarm_handling].

init_per_suite(Config) ->
    init(),
    application:load(alarms),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    alarms_server:start(),
    Config.

end_per_testcase(_TC, _Config) ->
    alarms_server:stop(),
    ok.

test_monitor(_Config) ->
    Options = alarms_server:get_monitor_options(),
    true = proplists:get_value(busy_dist_port, Options),
    true = proplists:get_value(busy_port, Options),
    LH = proplists:get_value(large_heap, Options),
    LongGC = proplists:get_value(long_gc, Options),
    {ok, LH} = application:get_env(alarms, large_heap),
    {ok, LongGC} = application:get_env(alarms, long_gc),

    NewOptions = lists:keyreplace(
                   large_heap, 1, Options, {large_heap, 1234567}),
    Options = alarms_server:set_monitor_options(NewOptions),
    NewOptions = alarms_server:get_monitor_options(),
    true.

test_mnesia(_Config) ->
    Subscribers = mnesia:system_info(subscribers),
    ServerPid = whereis(alarms_server),
    true = lists:member(ServerPid, Subscribers).

test_alarm_handling(_Config) ->
    Pid = self(),

    fake_event({monitor, Pid, long_gc, gc_info}),
    [{long_gc, {Pid, gc_info}}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(long_gc),

    fake_event({monitor, Pid, large_heap, lh_info}),
    [{large_heap, {Pid, lh_info}}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(large_heap),

    fake_event({monitor, Pid, busy_port, port}),
    [{busy_port, {Pid, port}}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(busy_port),

    fake_event({monitor, Pid, busy_dist_port, distport}),
    [{busy_dist_port, {Pid, distport}}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(busy_dist_port),

    fake_event({mnesia_overload, overload_info}),
    [{mnesia_overload, overload_info}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(mnesia_overload),

    fake_event({inconsistent_database, context, info}),
    [{inconsistent_database, {context, info}}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(inconsistent_database),

    fake_event({mnesia_fatal, "fatal", [], <<>>}),
    [{mnesia_fatal, {"fatal", []}}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(mnesia_fatal),

    fake_event({mnesia_error, "error", []}),
    [{mnesia_error, {"error", []}}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(mnesia_error),

    fake_event({mnesia_up, node}),
    [{mnesia_up, node}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(mnesia_up),

    fake_event({mnesia_down, node}),
    [{mnesia_down, node}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(mnesia_down),

    fake_event({nodeup, upnode, []}),
    [{nodeup, {upnode, []}}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(nodeup),

    fake_event({nodedown, downnode, []}),
    [{nodedown, {downnode, []}}] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(nodedown),
    true.
