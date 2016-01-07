%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Alarm handler storing alarm summary in folsom metrics
%%%
%%% @end
%%% Created : 8 May 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(alarms_folsom_handler).

-behaviour(gen_event).

%% API
-export([add_handler/0, get_alarms/0, get_alarm/1, get_alarm/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-include("alarms.hrl").

-record(state, {}).

-type alarm() :: {alarm_type(), alarm_data()}.
-type alarm_type() :: term().
-type alarm_data() :: {Count :: pos_integer(),
                       CurCount :: pos_integer(),
                       history()}.

-type history() :: [{Timestamp :: calendar:datetime(),
                     [{event, Details :: term()}]}].

-define(DEFAULT_LIMIT, 5).

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
    gen_event:call(?EVENT_MANAGER, ?MODULE,
                   {get_alarm, AlarmType, ?DEFAULT_LIMIT}).

%%--------------------------------------------------------------------
%% @doc
%% Returns the summary of alarms of the specified type.
%% @end
%%--------------------------------------------------------------------
-spec get_alarm(alarm_type(), pos_integer()) -> {ok, alarm_data()} | error.
get_alarm(AlarmType, Limit) ->
    gen_event:call(?EVENT_MANAGER, ?MODULE, {get_alarm, AlarmType, Limit}).

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
    folsom:start(),
    HistorySize = alarms_utils:get_cfg({?MODULE, history_size}),
    lists:foreach(
      fun(AlarmType) ->
              folsom_metrics:new_spiral({alarm, AlarmType, summary}),
              folsom_metrics:tag_metric({alarm, AlarmType, summary}, alarm),
              folsom_metrics:new_history({alarm, AlarmType, history},
                                         HistorySize),
              folsom_metrics:tag_metric({alarm, AlarmType, history}, alarm)
      end, alarms_utils:alarm_types()),
    {ok, #state{}}.

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
handle_event({set_alarm, {AlarmType, Details}}, State) ->
    case lists:member(AlarmType, alarms_utils:alarm_types()) of
        true ->
            folsom_metrics:notify({{alarm, AlarmType, summary}, 1}),
            folsom_metrics:notify({{alarm, AlarmType, history}, Details});
        false ->
            ok
    end,
    {ok, State};
handle_event({clear_alarm, _AlarmType}, State) ->
    {ok, State}; % Not supported by folsom
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
handle_call(get_alarms, State) ->
    Reply = lists:flatmap(
              fun(AlarmType) ->
                      case do_get_alarm(AlarmType, ?DEFAULT_LIMIT) of
                          {0, _, _} -> [];
                          AlarmData -> [{AlarmType, AlarmData}]
                      end
              end, alarms_utils:alarm_types()),
    {ok, Reply, State};
handle_call({get_alarm, AlarmType, Limit}, State) ->
    Reply = do_get_alarm(AlarmType, Limit),
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
do_get_alarm(AlarmType, Limit) ->
    [{count, Count}, {one, CurCount}] =
        folsom_metrics:get_metric_value({alarm, AlarmType, summary}),
    History0 = folsom_metrics_history:get_events(
                 {alarm, AlarmType, history}, Limit),
    History = [{alarms_utils:epoch_usec_to_local_time(TS), Events}
               || {TS, Events} <- History0],
    {Count, CurCount, History}.
