%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Alarm handler storing alarm summary in ets tables
%%%
%%% @end
%%% Created : 8 May 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(alarms_basic_handler).

-behaviour(gen_event).

%% API
-export([add_handler/0, swap_handler/0, get_alarms/0, get_alarm/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-include("alarms.hrl").

-record(state, {alarms, log_ts}).

-type alarm() :: {alarm_type(), alarm_data()}.
-type alarm_type() :: term().
-type alarm_data() :: {Count :: pos_integer(),
                       LastTimestamp :: calendar:datetime(),
                       LastDetails :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds this event handler to alarm_handler
%% @end
%%--------------------------------------------------------------------
-spec add_handler() ->  ok | {'EXIT', term()} | term().
add_handler() ->
    gen_event:add_handler(?EVENT_MANAGER, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Replaces default SASL alarm_handler with this one
%% @end
%%--------------------------------------------------------------------
-spec swap_handler() -> ok | {error, term()}.
swap_handler() ->
    gen_event:swap_handler(?EVENT_MANAGER,
                           {alarm_handler, swap},
                           {?MODULE, []}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all alarms
%% @end
%%--------------------------------------------------------------------
-spec get_alarms() -> [alarm()].
get_alarms() ->
    gen_event:call(?EVENT_MANAGER, ?MODULE, get_alarms).

%%--------------------------------------------------------------------
%% @doc
%% Returns the summary of alarms of the specified type.
%% @end
%%--------------------------------------------------------------------
-spec get_alarm(alarm_type()) -> {ok, alarm_data()} | error.
get_alarm(AlarmType) ->
    gen_event:call(?EVENT_MANAGER, ?MODULE, {get_alarm, AlarmType}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(_) ->
    {ok, #state{alarms = orddict:new(), log_ts = orddict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({set_alarm, {AlarmType, Details}},
             State = #state{alarms = Alarms0, log_ts = LogTS0}) ->
    case lists:member(AlarmType, alarms_utils:alarm_types()) of
        true ->
            {Alarms, LogTS} = handle_alarm(AlarmType, Details, Alarms0, LogTS0),
            {ok, State#state{alarms = Alarms, log_ts = LogTS}};
        false ->
            error_logger:error_msg("Unexpected alarm ~p: ~p~n",
                                   [AlarmType, Details]),
            {ok, State}
    end;
handle_event({clear_alarm, AlarmType},
             State = #state{alarms = Alarms, log_ts = LogTS}) ->
    orddict:erase(AlarmType, Alarms),
    {ok, State#state{alarms = orddict:erase(AlarmType, Alarms),
                     log_ts = orddict:erase(AlarmType, LogTS)}};
handle_event(_, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(get_alarms, State = #state{alarms = Alarms}) ->
    {ok, orddict:to_list(Alarms), State};
handle_call({get_alarm, AlarmType}, State = #state{alarms = Alarms}) ->
    Reply = orddict:find(AlarmType, Alarms),
    {ok, Reply, State};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_alarm(AlarmType, Details, Alarms0, LogTS0) ->
    Count = case orddict:find(AlarmType, Alarms0) of
                {ok, {Cnt, _Last, _Details}} -> Cnt + 1;
                error                        -> 1
            end,
    TS = alarms_utils:epoch_usec_to_local_time(alarms_utils:now_epoch_usec()),
    Alarms = orddict:store(AlarmType, {Count, TS, Details}, Alarms0),
    ShouldLog = case orddict:find(AlarmType, LogTS0) of
                    {ok, T} ->
                        calendar:datetime_to_gregorian_seconds(TS) -
                            calendar:datetime_to_gregorian_seconds(T) >=
                            alarms_utils:get_cfg({?MODULE, min_log_interval});
                    error ->
                        true
                end,
    LogTS = case ShouldLog andalso maybe_log(AlarmType, Count, Details) of
                true  -> orddict:store(AlarmType, TS, LogTS0);
                false -> LogTS0
            end,
    {Alarms, LogTS}.

maybe_log(AlarmType, Count, Details) ->
    case lists:member(
           AlarmType,
           [mnesia_up, mnesia_down |
            ?SYSTEM_MONITOR_ALARM_TYPES ++ ?NET_KERNEL_ALARM_TYPES]) of
        true ->
            Report = alarms_utils:get_cfg({?MODULE, report}),
            error_logger:Report([{alarm, AlarmType},
                                 {count, Count},
                                 {details, Details}]),
            true;
        false ->
            false
    end.
