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

%%% @private
-module(seestar_messages).

-export([encode/1, decode/2]).

-include("constants.hrl").
-include("seestar_messages.hrl").

%% requests.
-define(STARTUP, 16#01).
-define(CREDENTIALS, 16#04).
-define(OPTIONS, 16#05).
-define(QUERY, 16#07).
-define(PREPARE, 16#09).
-define(EXECUTE, 16#0A).
-define(REGISTER, 16#0B).
%% responses.
-define(ERROR, 16#00).
-define(READY, 16#02).
-define(AUTHENTICATE, 16#03).
-define(SUPPORTED, 16#06).
-define(RESULT, 16#08).
%% event.
-define(EVENT, 16#0C).

-type outgoing() :: #startup{}
                  | #credentials{}
                  | #options{}
                  | #'query'{}
                  | #prepare{}
                  | #execute{}
                  | #register{}.

-type incoming() :: #error{}
                  | #ready{}
                  | #authenticate{}
                  | #supported{}
                  | #result{}
                  | #event{}.

-define(VERSION, <<"CQL_VERSION">>).
-define(COMPRESSION, <<"COMPRESSION">>).

%% -------------------------------------------------------------------------
%% encoding functions
%% -------------------------------------------------------------------------

-spec encode(outgoing()) -> {seestar_frame:opcode(), binary()}.
encode(#startup{version = Version, compression = Compression}) ->
    KVPairs =
        case Compression of
            undefined ->
                [{?VERSION, Version}];
            Value when is_binary(Value) ->
                [{?VERSION, Version}, {?COMPRESSION, Value}]
        end,
    {?STARTUP, seestar_types:encode_string_map(KVPairs)};

encode(#credentials{credentials = KVPairs}) ->
    {?CREDENTIALS, seestar_types:encode_string_map(KVPairs)};

encode(#options{}) ->
    {?OPTIONS, <<>>};

encode(#'query'{'query' = Query, consistency = Consistency}) ->
    {?QUERY, <<(seestar_types:encode_long_string(Query))/binary,
               (seestar_types:encode_consistency(Consistency))/binary>>};

encode(#prepare{'query' = Query}) ->
    {?PREPARE, seestar_types:encode_long_string(Query)};

encode(#execute{id = ID, types = Types, values = Values, consistency = Consistency}) ->
    Variables = [ seestar_cqltypes:encode_value_with_size(Type, Value) ||
                  {Type, Value} <- lists:zip(Types, Values) ],
    {?EXECUTE, list_to_binary([seestar_types:encode_short_bytes(ID),
                               seestar_types:encode_short(length(Variables)),
                               Variables,
                               seestar_types:encode_consistency(Consistency)])};

encode(#register{event_types = Types}) ->
    % assert validity of event types.
    Unique = lists:usort(Types),
    [] = Unique -- [topology_change, status_change, schema_change],
    Encoded = [ list_to_binary(string:to_upper(atom_to_list(Type))) || Type <- Types ],
    {?REGISTER, seestar_types:encode_string_list(Encoded)}.

%% -------------------------------------------------------------------------
%% decoding functions
%% -------------------------------------------------------------------------

-spec decode(seestar_frame:opcode(), binary()) -> incoming().
decode(?ERROR, Body) ->
    {Code, Rest0} = seestar_types:decode_int(Body),
    {Message, Rest1} = seestar_types:decode_string(Rest0),
    #error{code = Code,
           message = Message,
           details = case Code of
                         ?UNAVAILABLE    -> decode_unavailable(Rest1);
                         ?WRITE_TIMEOUT  -> decode_write_timeout(Rest1);
                         ?READ_TIMEOUT   -> decode_read_timeout(Rest1);
                         ?ALREADY_EXISTS -> decode_already_exists(Rest1);
                         ?UNPREPARED     -> decode_unprepared(Rest1);
                         _               -> undefined
                     end};

decode(?READY, _Body) ->
    #ready{};

decode(?AUTHENTICATE, Body) ->
    {Class, _} = seestar_types:decode_string(Body),
    #authenticate{class = Class};

decode(?SUPPORTED, Body) ->
    {KVPairs, _} = seestar_types:decode_string_multimap(Body),
    #supported{versions = proplists:get_value(?VERSION, KVPairs),
               compression = proplists:get_value(?COMPRESSION, KVPairs)};

decode(?RESULT, Body) ->
    {Kind, Rest} = seestar_types:decode_int(Body),
    #result{result = case Kind of
                         16#01 -> void;
                         16#02 -> decode_rows(Rest);
                         16#03 -> decode_set_keyspace(Rest);
                         16#04 -> decode_prepared(Rest);
                         16#05 -> decode_schema_change(Rest)
                     end}.

%% -------------------------------------------------------------------------
%% error details
%% -------------------------------------------------------------------------

decode_unavailable(Data) ->
    {Consistency, Rest0} = seestar_types:decode_consistency(Data),
    {Required, Rest1} = seestar_types:decode_int(Rest0),
    {Alive, _} = seestar_types:decode_int(Rest1),
    #unavailable{consistency = Consistency, required = Required, alive = Alive}.

decode_write_timeout(Data) ->
    {Consistency, Rest0} = seestar_types:decode_consistency(Data),
    {Received, Rest1} = seestar_types:decode_int(Rest0),
    {Required, Rest2} = seestar_types:decode_int(Rest1),
    {WriteType, _} = seestar_types:decode_string(Rest2),
    #write_timeout{consistency = Consistency,
                   received = Received,
                   required = Required,
                   write_type = list_to_atom(string:to_lower(binary_to_list(WriteType)))}.

decode_read_timeout(Data) ->
    {Consistency, Rest0} = seestar_types:decode_consistency(Data),
    {Received, Rest1} = seestar_types:decode_int(Rest0),
    {Required, Rest2} = seestar_types:decode_int(Rest1),
    <<DataPresent, _/binary>> = Rest2,
    #read_timeout{consistency = Consistency,
                  received = Received,
                  required = Required,
                  data_present = DataPresent =/= 0}.

decode_already_exists(Data) ->
    {{Keyspace, Table}, _} = decode_table_spec(Data),
    #already_exists{keyspace = Keyspace,
                    table = case Table of
                                 <<>> -> undefined;
                                 _    -> Table
                            end}.

decode_unprepared(Data) ->
    {ID, _} = seestar_types:decode_short_bytes(Data),
    #unprepared{id = ID}.

%% -------------------------------------------------------------------------
%% different result types
%% -------------------------------------------------------------------------

decode_rows(Body) ->
    {Meta, Rest0} = decode_metadata(Body),
    {Count, Rest1} = seestar_types:decode_int(Rest0),
    #rows{metadata = Meta, rows = decode_rows(Meta, Rest1, Count)}.

decode_rows(Meta, Data, Count) ->
    decode_rows(Meta, Data, Count, []).

decode_rows(_, _, 0, Acc) ->
    lists:reverse(Acc);
decode_rows(Meta, Data, Count, Acc) ->
    {Row, Rest} = decode_row(Meta, Data),
    decode_rows(Meta, Rest, Count - 1, [Row|Acc]).

decode_row(Meta, Data) ->
    decode_row(Meta, Data, []).

decode_row([], Data, Row) ->
    {lists:reverse(Row), Data};
decode_row([#column{type = Type}|Meta], Data, Row) ->
    {Value, Rest} = seestar_cqltypes:decode_value_with_size(Type, Data),
    decode_row(Meta, Rest, [Value|Row]).

decode_metadata(Data) ->
    {Flags, Rest0} = seestar_types:decode_int(Data),
    {Count, Rest1} = seestar_types:decode_int(Rest0),
    {TableSpec, Rest2} = case Flags of
                             16#00 -> {undefined, Rest1};
                             16#01 -> decode_table_spec(Rest1)
                         end,
    decode_column_specs(TableSpec, Rest2, Count).

decode_column_specs(TableSpec, Data, Count) ->
    decode_column_specs(TableSpec, Data, Count, []).

decode_column_specs(_, Data, 0, Meta) ->
    {lists:reverse(Meta), Data};
decode_column_specs(TableSpec, Data, Count, Meta) ->
    {Column, Rest} = decode_column_spec(TableSpec, Data),
    decode_column_specs(TableSpec, Rest, Count - 1, [Column|Meta]).

decode_column_spec(undefined, Data) ->
    {TableSpec, Rest} = decode_table_spec(Data),
    decode_column_spec(TableSpec, Rest);
decode_column_spec({Keyspace, Table}, Data) ->
    {Name, Rest0} = seestar_types:decode_string(Data),
    {Type, Rest1} = seestar_cqltypes:decode_type(Rest0),
    {#column{keyspace = Keyspace, table = Table, name = Name, type = Type}, Rest1}.

decode_table_spec(Data) ->
    {Keyspace, Rest0} = seestar_types:decode_string(Data),
    {Table, Rest1} = seestar_types:decode_string(Rest0),
    {{Keyspace, Table}, Rest1}.

decode_set_keyspace(Body) ->
    {Keyspace, _} = seestar_types:decode_string(Body),
    #set_keyspace{keyspace = Keyspace}.

decode_prepared(Body) ->
    {ID, Rest} = seestar_types:decode_short_bytes(Body),
    {Meta, _} = decode_metadata(Rest),
    #prepared{id = ID, metadata = Meta}.

decode_schema_change(Body) ->
    {Change, Rest} = seestar_types:decode_string(Body),
    {{Keyspace, Table}, _} = decode_table_spec(Rest),
    #schema_change{change = list_to_atom(string:to_lower(binary_to_list(Change))),
                   keyspace = Keyspace,
                   table = case Table of
                               <<>> -> undefined;
                               _    -> Table
                           end}.
