%%%-------------------------------------------------------------------
%%% @author  Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc A simple system monitoring utility
%%%      for applications using mnesia.
%%%
%%% @end
%%% Created : 16 Apr 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(alarms_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("alarms.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    gen_event:delete_handler(?EVENT_MANAGER, alarm_handler, []),
    [ok = gen_event:add_handler(?EVENT_MANAGER, Handler, [])
     || Handler <- alarms_utils:get_cfg(handlers)],
    alarms_sup:start_link().

stop(_State) ->
    [gen_event:delete_handler(?EVENT_MANAGER, Handler, [])
     || Handler <- alarms_utils:get_cfg(handlers)],
    gen_event:add_handler(?EVENT_MANAGER, alarm_handler, []).
