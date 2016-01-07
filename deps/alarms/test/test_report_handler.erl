%%%-------------------------------------------------------------------
%%% @author  Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc A simple report handler accumulating all logs in a list
%%%
%%% @end
%%% Created : 16 May 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(test_report_handler).

-behaviour(gen_event).

-export([get_logs/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

get_logs() ->
    gen_event:call(error_logger, ?MODULE, get_logs).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
    {ok, []}.

handle_event(Event, State) ->
    {ok, [Event|State]}.

handle_call(get_logs, State) ->
    Reply = lists:reverse(State),
    {ok, Reply, State};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
