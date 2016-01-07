%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Macros used by alarms
%%%
%%% @end
%%% Created : 8 May 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------

-define(EVENT_MANAGER, alarm_handler).

-define(ALARM_TYPES,
        (?SYSTEM_MONITOR_ALARM_TYPES ++
             ?MNESIA_ALARM_TYPES ++
             ?NET_KERNEL_ALARM_TYPES)).

-define(SYSTEM_MONITOR_ALARM_TYPES,
        [long_gc,
         large_heap,
         busy_port,
         busy_dist_port]).

-define(MNESIA_ALARM_TYPES,
        [mnesia_overload,
         inconsistent_database,
         mnesia_fatal,
         mnesia_error,
         mnesia_up,
         mnesia_down]).

-define(NET_KERNEL_ALARM_TYPES,
        [nodeup,
         nodedown]).
