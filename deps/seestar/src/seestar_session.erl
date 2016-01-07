%%% Copyright 2014 Aleksey Yeschenko
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(seestar_session).

-behaviour(gen_server).

-include("seestar_messages.hrl").
-include("builtin_types.hrl").

%% API exports.
-export([start_link/2, start_link/3, start_link/4, stop/1]).
-export([perform/3, perform_async/3]).
-export([prepare/2, execute/5, execute_async/5]).

%% gen_server exports.
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3]).

-type credentials() :: [{string() | binary(), string() | binary()}].
-type events() :: [topology_change | status_change | schema_change].
-type client_option() :: {keyspace, string() | binary()}
                       | {credentials, credentials()}
                       | {events, events()}.

-type connect_option() :: gen_tcp:connect_option() | {connect_timeout, timeout()}.

-type 'query'() :: binary() | string().
-type query_id() :: binary().

-define(b2l(Term), case is_binary(Term) of true -> binary_to_list(Term); false -> Term end).
-define(l2b(Term), case is_list(Term) of true -> list_to_binary(Term); false -> Term end).

-record(req,
        {op :: seestar_frame:opcode(),
         body :: binary(),
         from :: {pid(), reference()},
         sync = true :: boolean()}).

-record(st,
        {host :: inet:hostname(),
         sock :: inet:socket(),
         buffer :: seestar_buffer:buffer(),
         free_ids :: [seestar_frame:stream_id()],
         backlog = queue:new() :: queue_t(),
         reqs :: ets:tid()}).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

%% @equiv start_link(Host, Post, [])
-spec start_link(inet:hostname(), inet:port_number()) ->
    any().
start_link(Host, Port) ->
    start_link(Host, Port, []).

%% @equiv start_link(Host, Post, ClientOptions, [])
-spec start_link(inet:hostname(), inet:port_number(), [client_option()]) ->
    any().
start_link(Host, Port, ClientOptions) ->
    start_link(Host, Port, ClientOptions, []).

-spec start_link(inet:hostname(), inet:port_number(), [client_option()], [connect_option()]) ->
    any().
start_link(Host, Port, ClientOptions, ConnectOptions) ->
     case gen_server:start_link(?MODULE, [Host, Port, ConnectOptions], []) of
        {ok, Pid} ->
            case setup(Pid, ClientOptions) of
                ok    -> {ok, Pid};
                Error -> stop(Pid), Error
            end;
        Error ->
            Error
    end.

setup(Pid, Options) ->
    case authenticate(Pid, Options) of
        false ->
            {error, invalid_credentials};
        true ->
            case set_keyspace(Pid, Options) of
                false ->
                    {error, invalid_keyspace};
                true ->
                    case subscribe(Pid, Options) of
                        false -> {error, invalid_events};
                        true  -> ok
                    end
            end
    end.

authenticate(Pid, Options) ->
    Credentials = proplists:get_value(credentials, Options),
    case request(Pid, #startup{}, true) of
        #ready{} ->
            true;
        #authenticate{} when Credentials =:= undefined ->
            false;
        #authenticate{} ->
            KVPairs = [ {?l2b(K), ?l2b(V)} || {K, V} <- Credentials ],
            case request(Pid, #credentials{credentials = KVPairs}, true) of
                #ready{} -> true;
                #error{} -> false
            end
    end.

set_keyspace(Pid, Options) ->
    case proplists:get_value(keyspace, Options) of
        undefined ->
            true;
        Keyspace ->
            case perform(Pid, "USE " ++ ?b2l(Keyspace), one) of
                {ok, _Result}    -> true;
                {error, _Reason} -> false
            end
    end.

subscribe(Pid, Options) ->
    case proplists:get_value(events, Options, []) of
        [] ->
            true;
        Events ->
            case request(Pid, #register{event_types = Events}, true) of
                #ready{} -> true;
                #error{} -> false
            end
    end.

%% @doc Stop the client.
%% Closes the socket and terminates the process normally.
-spec stop(pid()) -> ok.
stop(Client) ->
    gen_server:cast(Client, stop).

%% @doc Synchoronously perform a CQL query using the specified consistency level.
%% Returns a result of an appropriate type (void, rows, set_keyspace, schema_change).
%% Use {@link seestar_result} module functions to work with the result.
-spec perform(pid(), 'query'(), seestar:consistency()) ->
    {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()}.
perform(Client, Query, Consistency) ->
    case request(Client, #'query'{'query' = ?l2b(Query), consistency = Consistency}, true) of
        #result{result = Result} ->
            {ok, Result};
        #error{} = Error ->
            {error, Error}
    end.

%% TODO doc
%% @doc Asynchronously perform a CQL query using the specified consistency level.
-spec perform_async(pid(), 'query'(), seestar:consistency()) -> ok.
perform_async(Client, Query, Consistency) ->
    Req = #'query'{'query' = ?l2b(Query), consistency = Consistency},
    request(Client, Req, false).

%% @doc Prepare a query for later execution. The response will contain the prepared
%% query id and column metadata for all the variables (if any).
%% @see execute/3.
%% @see execute/4.
-spec prepare(pid(), 'query'()) ->
    {ok, Result :: seestar_result:prepared_result()} | {error, Error :: seestar_error:error()}.
prepare(Client, Query) ->
    case request(Client, #prepare{'query' = ?l2b(Query)}, true) of
        #result{result = Result} ->
            {ok, Result};
        #error{} = Error ->
            {error, Error}
    end.

%% @doc Synchronously execute a prepared query using the specified consistency level.
%% Use {@link seestar_result} module functions to work with the result.
%% @see prepare/2.
%% @see perform/3.
-spec execute(pid(),
              query_id(),
              [seestar_cqltypes:type()], [seestar_cqltypes:value()],
              seestar:consistency()) ->
        {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()}.
execute(Client, QueryID, Types, Values, Consistency) ->
    Req = #execute{id = QueryID, types = Types, values = Values, consistency = Consistency},
    case request(Client, Req, true) of
        #result{result = Result} ->
            {ok, Result};
        #error{} = Error ->
            {error, Error}
    end.

execute_async(Client, QueryID, Types, Values, Consistency) ->
    Req = #execute{id = QueryID, types = Types, values = Values, consistency = Consistency},
    request(Client, Req, false).

request(Client, Request, Sync) ->
    {ReqOp, ReqBody} = seestar_messages:encode(Request),
    case gen_server:call(Client, {request, ReqOp, ReqBody, Sync}, infinity) of
        {RespOp, RespBody} ->
            seestar_messages:decode(RespOp, RespBody);
        Ref ->
            Ref
    end.

%% -------------------------------------------------------------------------
%% gen_server callback functions
%% -------------------------------------------------------------------------

%% @private
init([Host, Port, ConnectOptions]) ->
    Timeout = proplists:get_value(connect_timeout, ConnectOptions, infinity),
    SockOpts = proplists:delete(connect_timeout, ConnectOptions),
    case gen_tcp:connect(Host, Port, SockOpts, Timeout) of
        {ok, Sock} ->
            ok = inet:setopts(Sock, [binary, {packet, 0}, {active, true}]),
            {ok, #st{host = Host, sock = Sock, buffer = seestar_buffer:new(),
                     free_ids = lists:seq(0, 127), reqs = ets:new(seestar_reqs, [])}};
        {error, Reason} ->
            {stop, {connection_error, Reason}}
    end.

%% @private
terminate(_Reason, _St) ->
    ok.

%% @private
handle_call({request, Op, Body, Sync}, From, #st{free_ids = []} = St) ->
    Req = #req{op = Op, body = Body, from = From, sync = Sync},
    {noreply, St#st{backlog = queue:in(Req, St#st.backlog)}};

handle_call({request, Op, Body, Sync}, {_Pid, Ref} = From, St) ->
    case Sync of
        true  -> ok;
        false -> gen_server:reply(From, Ref)
    end,
    case send_request(#req{op = Op, body = Body, from = From, sync = Sync}, St) of
        {ok, St1}       -> {noreply, St1};
        {error, Reason} -> {stop, {socket_error, Reason}, St}
    end;

handle_call(Request, _From, St) ->
    {stop, {unexpected_call, Request}, St}.

send_request(#req{op = Op, body = Body, from = From, sync = Sync}, St) ->
    ID = hd(St#st.free_ids),
    Frame = seestar_frame:new(ID, [], Op, Body),
    case gen_tcp:send(St#st.sock, seestar_frame:encode(Frame)) of
        ok ->
            ets:insert(St#st.reqs, {ID, From, Sync}),
            {ok, St#st{free_ids = tl(St#st.free_ids)}};
        {error, _Reason} = Error ->
            Error
    end.

%% @private
handle_cast(stop, St) ->
    gen_tcp:close(St#st.sock),
    {stop, normal, St};

handle_cast(Request, St) ->
    {stop, {unexpected_cast, Request}, St}.

%% @private
handle_info({tcp, Sock, Data}, #st{sock = Sock} = St) ->
    {Frames, Buffer} = seestar_buffer:decode(St#st.buffer, Data),
    {noreply, process_frames(Frames, St#st{buffer = Buffer})};

handle_info({tcp_closed, Sock}, #st{sock = Sock} = St) ->
    {stop, socket_closed, St};

handle_info({tcp_error, Sock, Reason}, #st{sock = Sock} = St) ->
    {stop, {socket_error, Reason}, St};

handle_info(Info, St) ->
    {stop, {unexpected_info, Info}, St}.

process_frames([Frame|Frames], St) ->
    process_frames(Frames,
                   case seestar_frame:id(Frame) of
                       -1 -> handle_event(Frame, St);
                       _  -> handle_response(Frame, St)
                   end);
process_frames([], St) ->
    process_backlog(St).

handle_event(_Frame, St) ->
    St.

handle_response(Frame, St) ->
    ID = seestar_frame:id(Frame),
    [{ID, From, Sync}] = ets:lookup(St#st.reqs, ID),
    ets:delete(St#st.reqs, ID),
    Op = seestar_frame:opcode(Frame),
    Body = seestar_frame:body(Frame),
    case Sync of
        true  -> gen_server:reply(From, {Op, Body});
        false -> reply_async(From, Op, Body)
    end,
    St#st{free_ids = [ID|St#st.free_ids]}.

reply_async({Pid, Ref}, Op, Body) ->
    F = fun() ->
            case seestar_messages:decode(Op, Body) of
                #result{result = Result} ->
                    {ok, Result};
                #error{} = Error ->
                    {error, Error}
            end
        end,
    Pid ! {seestar_response, Ref, F}.

process_backlog(#st{backlog = Backlog, free_ids = FreeIDs} = St) ->
    case queue:is_empty(Backlog) orelse FreeIDs =:= [] of
        true ->
            St;
        false ->
            {{value, Req}, Backlog1} = queue:out(Backlog),
            #req{from = {_Pid, Ref} = From, sync = Sync} = Req,
            case Sync of
                true  -> ok;
                false -> gen_server:reply(From, Ref)
            end,
            case send_request(Req, St#st{backlog = Backlog1}) of
                {ok, St1}       -> process_backlog(St1);
                {error, Reason} -> {stop, {socket_error, Reason}, St}
            end
    end.

%% @private
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
