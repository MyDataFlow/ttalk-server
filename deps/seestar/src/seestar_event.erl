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

-module(seestar_event).

-include("seestar_messages.hrl").

-export([type/1, change/1, ip/1, port/1, keyspace/1, table/1]).

-type topology_change_event() :: #topology_change{}.
-type status_change_event() :: #status_change{}.
-type schema_change_event() :: #schema_change{}.
-opaque event() :: topology_change_event()
                 | status_change_event()
                 | schema_change_event().
-export_type([event/0]).

-type type() :: topology_change | status_change | schema_change.
-type topology_change() :: new_node | removed_node.
-type status_change() :: up | down.
-type schema_change() :: created | updated | dropped.

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec type(Event :: event()) -> type().
type(#topology_change{}) ->
    topology_change;
type(#status_change{}) ->
    status_change;
type(#schema_change{}) ->
    schema_change.

-spec change(Event :: event()) -> topology_change() | status_change() | schema_change().
change(#topology_change{change = Change}) ->
    Change;
change(#status_change{change = Change}) ->
    Change;
change(#schema_change{change = Change}) ->
    Change.

-spec ip(Event :: topology_change_event() | status_change_event()) -> inet:ip_address().
ip(#topology_change{ip = IP}) ->
    IP;
ip(#status_change{ip = IP}) ->
    IP.

-spec port(Event :: topology_change_event() | status_change_event()) -> inet:port_number().
port(#topology_change{port = Port}) ->
    Port;
port(#status_change{port = Port}) ->
    Port.

-spec keyspace(Event :: schema_change_event()) -> binary().
keyspace(#schema_change{keyspace = Keyspace}) ->
    Keyspace.

-spec table(Event :: schema_change_event()) -> binary() | undefined.
table(#schema_change{table = Table}) ->
    Table.
