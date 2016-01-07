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

-module(seestar_error).

-include("seestar_messages.hrl").

-export([code/1, message/1, consistency/1, required/1, alive/1, received/1,
         write_type/1, data_present/1, keyspace/1, table/1, query_id/1]).

-opaque error() :: #error{}.
-export_type([error/0]).

-type write_type() :: simple | batch | unlogged_batch | counter | batch_log.

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec code(Error :: error()) -> integer().
code(#error{code = Code}) ->
    Code.

-spec message(Error :: error()) -> binary().
message(#error{message = Message}) ->
    Message.

-spec consistency(Error :: error()) -> seestar:consistency().
consistency(#error{details = #unavailable{consistency = Consistency}}) ->
    Consistency;
consistency(#error{details = #write_timeout{consistency = Consistency}}) ->
    Consistency;
consistency(#error{details = #read_timeout{consistency = Consistency}}) ->
    Consistency.

-spec required(Error :: error()) -> integer().
required(#error{details = #unavailable{required = Required}}) ->
    Required;
required(#error{details = #write_timeout{required = Required}}) ->
    Required;
required(#error{details = #read_timeout{required = Required}}) ->
    Required.

-spec alive(Error :: error()) -> integer().
alive(#error{details = #unavailable{alive = Alive}}) ->
    Alive.

-spec received(Error :: error()) -> integer().
received(#error{details = #write_timeout{received = Received}}) ->
    Received;
received(#error{details = #read_timeout{received = Received}}) ->
    Received.

-spec write_type(Error :: error()) -> write_type().
write_type(#error{details = #write_timeout{write_type = WriteType}}) ->
    WriteType.

-spec data_present(Error :: error()) -> boolean().
data_present(#error{details = #read_timeout{data_present = DataPresent}}) ->
    DataPresent.

-spec keyspace(Error :: error()) -> binary().
keyspace(#error{details = #already_exists{keyspace = Keyspace}}) ->
    Keyspace.

-spec table(Error :: error()) -> binary() | undefined.
table(#error{details = #already_exists{table = Table}}) ->
    Table.

-spec query_id(Error :: error()) -> binary().
query_id(#error{details = #unprepared{id = ID}}) ->
    ID.
