%%%-------------------------------------------------------------------
%%% @author  Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Main server for alarms application.
%%%
%%% @end
%%% Created : 16 Apr 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(alarms_server).

-behaviour(gen_server).

%% API
-export([start/0, stop/0, start_link/0,
         get_monitor_options/0, set_monitor_options/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_monitor_options() ->
    gen_server:call(?SERVER, get_monitor_options).

set_monitor_options(Settings) ->
    gen_server:call(?SERVER, {set_monitor_options, Settings}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    erlang:system_monitor(
      self(), [{long_gc, alarms_utils:get_cfg(long_gc)},
               {large_heap, alarms_utils:get_cfg(large_heap)},
               busy_port,
               busy_dist_port]),
    {ok, _} = mnesia:subscribe(system),
    ok = net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_monitor_options, _From, State) ->
    Self = self(),
    {Self, Options} = erlang:system_monitor(),
    {reply, Options, State};
handle_call({set_monitor_options, Settings}, _From, State) ->
    Self = self(),
    {Self, Options} = erlang:system_monitor(self(), Settings),
    {reply, Options, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    case msg_to_alarm(Info) of
        false ->
            ok;
        {AlarmType, Details} ->
            set_alarm(AlarmType, Details)
    end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{}) ->
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

%% from erlang:system_monitor/2
msg_to_alarm({monitor, GcPid, long_gc, Info}) ->
    {long_gc, {GcPid, Info}};
msg_to_alarm({monitor, GcPid, large_heap, Info}) ->
    {large_heap, {GcPid, Info}};
msg_to_alarm({monitor, SusPid, busy_port, Port}) ->
    {busy_port, {SusPid, Port}};
msg_to_alarm({monitor, SusPid, busy_dist_port, Port}) ->
    {busy_dist_port, {SusPid, Port}};

%% from mnesia:subscribe/1
msg_to_alarm({mnesia_overload, Details}) ->
    {mnesia_overload, Details};
msg_to_alarm({inconsistent_database, Context, Node}) ->
    {inconsistent_database, {Context, Node}};
msg_to_alarm({mnesia_fatal, Format, Args, _BinaryCore}) ->
    {mnesia_fatal, {Format, Args}};
msg_to_alarm({mnesia_error, Format, Args}) ->
    {mnesia_error, {Format, Args}};
msg_to_alarm({mnesia_up, Node}) ->
    {mnesia_up, Node};
msg_to_alarm({mnesia_down, Node}) ->
    {mnesia_down, Node};

%% from net_kernel:monitor_nodes/2
msg_to_alarm({nodeup, Node, InfoList}) ->
    {nodeup, {Node, InfoList}};
msg_to_alarm({nodedown, Node, InfoList}) ->
    {nodedown, {Node, InfoList}};

msg_to_alarm(_) ->
    false.

set_alarm(AlarmType, Info) ->
    alarm_handler:set_alarm({AlarmType, Info}).
