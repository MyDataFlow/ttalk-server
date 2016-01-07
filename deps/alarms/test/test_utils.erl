%%%-------------------------------------------------------------------
%%% @author  Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Test utilities
%%%
%%% @end
%%% Created : 16 May 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(test_utils).

-export([init/0, fake_alarm/2, fake_event/1]).

init() ->
    application:start(sasl),
    application:start(mnesia).

fake_alarm(AlarmType, Details) ->
    alarm_handler:set_alarm({AlarmType, Details}).

fake_event(Event) ->
    alarms_server ! Event,
    %% Now wait until event is processed
    sys:get_status(alarms_server).
