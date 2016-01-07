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

-module(seestar_result).

-include("seestar_messages.hrl").

-export([type/1, rows/1, names/1, types/1, type/2, keyspace/1, table/1, query_id/1,
         change/1]).

-type rows_result() :: #rows{}.
-type set_keyspace_result() :: #set_keyspace{}.
-type prepared_result() :: #prepared{}.
-type schema_change_result() :: #schema_change{}.
-opaque result() :: void
                | rows_result()
                | set_keyspace_result()
                | prepared_result()
                | schema_change_result().
-export_type([result/0, prepared_result/0]).

-type type() :: void | rows | set_keyspace | prepared | schema_change.
-type change() :: created | updated | dropped.

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec type(Result :: result()) -> type().
type(void) ->
    void;
type(#rows{}) ->
    rows;
type(#set_keyspace{}) ->
    set_keyspace;
type(#prepared{}) ->
    prepared;
type(#schema_change{}) ->
    schema_change.

-spec rows(Rows :: rows_result()) -> [[seestar_cqltypes:value()]].
rows(#rows{rows = Rows}) ->
    Rows.

-spec names(Result :: rows_result() | prepared_result()) -> [binary()].
names(#rows{metadata = Columns}) ->
    [ C#column.name || C <- Columns ];
names(#prepared{metadata = Columns}) ->
    [ C#column.name || C <- Columns ].

-spec types(Result :: rows_result() | prepared_result()) -> [seestar_cqltypes:type()].
types(#rows{metadata = Columns}) ->
    [ C#column.type || C <- Columns ];
types(#prepared{metadata = Columns}) ->
    [ C#column.type || C <- Columns ].

-spec type(Result :: rows_result() | prepared_result(), Name :: binary()) -> seestar_cqltypes:type().
type(#rows{metadata = Columns}, Name) ->
    hd([ C#column.type || C <- Columns, C#column.name =:= Name ]);
type(#prepared{metadata = Columns}, Name) ->
    hd([ C#column.type || C <- Columns, C#column.name =:= Name ]).

-spec keyspace(Result :: set_keyspace_result() | schema_change_result()) -> binary().
keyspace(#set_keyspace{keyspace = Keyspace}) ->
    Keyspace;
keyspace(#schema_change{keyspace = Keyspace}) ->
    Keyspace.

-spec table(Result :: schema_change_result()) -> binary() | undefined.
table(#schema_change{table = Table}) ->
    Table.

-spec query_id(Result :: prepared_result()) -> binary().
query_id(#prepared{id = ID}) ->
    ID.

-spec change(Result :: schema_change_result()) -> change().
change(#schema_change{change = Change}) ->
    Change.
