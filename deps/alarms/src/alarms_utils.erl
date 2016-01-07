%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Alarm service utils
%%%
%%% @end
%%% Created : 8 May 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(alarms_utils).

-export([get_cfg/1, set_cfg/2, alarm_types/0, event_manager/0,
         now_epoch_usec/0, epoch_usec_to_local_time/1]).

-include("alarms.hrl").

-define(EPOCH_SEC, 62167219200).

get_cfg(Key) ->
    {ok, Val} = application:get_env(alarms, Key),
    Val.

set_cfg(Key, Val) ->
    application:set_env(alarms, Key, Val).

alarm_types() ->
    ?ALARM_TYPES.

event_manager() ->
    ?EVENT_MANAGER.

now_epoch_usec() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.

epoch_usec_to_local_time(USec) ->
    calendar:universal_time_to_local_time(
      calendar:gregorian_seconds_to_datetime(?EPOCH_SEC + USec div 1000000)).
