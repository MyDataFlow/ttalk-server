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

%% requests.
-record(startup,
        {version = <<"3.0.0">> :: binary(),
         compression :: undefined | binary()}).

-record(credentials,
        {credentials :: [{binary(), binary()}]}).

-record(options,
        {}).

-record('query',
        {'query' :: binary(),
         consistency = one :: atom()}).

-record(prepare,
        {'query' :: binary()}).

-record(execute,
        {id :: binary(),
         types = [] :: [seestar_cqltypes:type()],
         values = [] :: [seestar_cqltypes:value()],
         variables :: [binary()],
         consistency = one :: atom()}).

-record(register,
        {event_types = [] :: [topology_change | status_change | schema_change]}).

%% responses.
-record(ready,
        {}).

-record(authenticate,
        {class :: binary()}).

-record(supported,
        {versions :: [binary()],
         compression :: [binary()]}).

%% error and various details
-record(unavailable,
        {consistency :: atom(),
         required :: integer(),
         alive :: integer()}).

-record(write_timeout,
        {consistency :: atom(),
         received :: integer(),
         required :: integer(),
         write_type :: atom()}).

-record(read_timeout,
        {consistency :: atom(),
         received :: integer(),
         required :: integer(),
         data_present :: boolean()}).

-record(already_exists,
        {keyspace :: binary(),
         table :: binary() | undefined}).

-record(unprepared,
        {id :: binary()}).

-record(error,
        {code :: non_neg_integer(),
         message :: binary(),
         details :: undefined
                  | #unavailable{}
                  | #write_timeout{}
                  | #read_timeout{}
                  | #already_exists{}
                  | #unprepared{}}).

%% result and various result sub-types.
-record(column,
        {keyspace :: binary(),
         table :: binary(),
         name :: binary(),
         type :: seestar_cqltypes:type()}).

-record(rows,
        {metadata :: [#column{}],
         rows :: [[seestar_cqltypes:value()]]}).

-record(set_keyspace,
        {keyspace :: binary()}).

-record(prepared,
        {id :: binary(),
         metadata :: [#column{}]}).

%% also an event.
-record(schema_change,
        {change :: created | updated | dropped,
         keyspace :: binary(),
         table :: binary() | undefined}).

-record(result,
        {result :: void
                 | #rows{}
                 | #set_keyspace{}
                 | #prepared{}
                 | #schema_change{}}).

%% event.
-record(topology_change,
        {change :: new_node | removed_node,
         ip :: inet:ip_address(),
         port :: inet:port_number()}).

-record(status_change,
        {change :: up | down,
         ip :: inet:ip_address(),
         port :: inet:port_number()}).

-record(event,
        {event :: #topology_change{}
                | #status_change{}
                | #schema_change{}}).
