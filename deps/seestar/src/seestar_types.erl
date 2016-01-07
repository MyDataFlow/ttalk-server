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
-module(seestar_types).

-export([encode_short/1, encode_long_string/1, encode_string_list/1, encode_bytes/1,
         encode_short_bytes/1, encode_consistency/1, encode_string_map/1]).
-export([decode_int/1, decode_short/1, decode_string/1, decode_uuid/1, decode_bytes/1,
         decode_short_bytes/1, decode_consistency/1, decode_string_multimap/1]).

%% -------------------------------------------------------------------------
%% encoding functions
%% -------------------------------------------------------------------------

encode_int(Value) ->
    <<Value:32>>.

encode_short(Value) ->
    <<Value:16>>.

encode_string(Value) ->
    <<(encode_short(size(Value)))/binary, Value/binary>>.

encode_long_string(Value) ->
    <<(encode_int(size(Value)))/binary, Value/binary>>.

encode_string_list(Values) ->
    list_to_binary([encode_short(length(Values)),
                    [ encode_string(Value) || Value <- Values ]]).

encode_bytes(Value) ->
    <<(encode_int(size(Value)))/binary, Value/binary>>.

encode_short_bytes(Value) ->
    <<(encode_short(size(Value)))/binary, Value/binary>>.

encode_consistency(Value) ->
    encode_short(case Value of
                     any          -> 16#00;
                     one          -> 16#01;
                     two          -> 16#02;
                     three        -> 16#03;
                     quorum       -> 16#04;
                     all          -> 16#05;
                     local_quorum -> 16#06;
                     each_quorum  -> 16#07
                 end).

encode_string_map(KVPairs) ->
    encode_string_map(KVPairs, []).

encode_string_map([], Acc) ->
    list_to_binary([encode_short(length(Acc)), lists:reverse(Acc)]);
encode_string_map([{K, V}|Rest], Acc) ->
    encode_string_map(Rest, [[encode_string(K), encode_string(V)]|Acc]).

%% -------------------------------------------------------------------------
%% decoding functions
%% -------------------------------------------------------------------------

decode_int(Data) ->
    <<Value:32, Rest/binary>> = Data,
    {Value, Rest}.

decode_short(Data) ->
    <<Value:16, Rest/binary>> = Data,
    {Value, Rest}.

decode_string(Data) ->
    {Length, Rest} = decode_short(Data),
    split_binary(Rest, Length).

decode_uuid(Data) ->
    split_binary(Data, 16).

decode_string_list(Data) ->
    {Count, Rest} = decode_short(Data),
    decode_string_list(Rest, Count, []).

decode_string_list(Data, 0, Acc) ->
    {lists:reverse(Acc), Data};
decode_string_list(Data, Count, Acc) ->
    {String, Rest} = decode_string(Data),
    decode_string_list(Rest, Count - 1, [String|Acc]).

decode_bytes(Data) ->
    {Size, Rest} = decode_int(Data),
    split_binary(Rest, Size).

decode_short_bytes(Data) ->
    {Size, Rest} = decode_short(Data),
    split_binary(Rest, Size).

decode_consistency(Data) ->
    {Consistency, Rest} = decode_short(Data),
    {case Consistency of
         16#00 -> any;
         16#01 -> one;
         16#02 -> two;
         16#03 -> three;
         16#04 -> quorum;
         16#05 -> all;
         16#06 -> local_quorum;
         16#07 -> each_quorum
      end, Rest}.

decode_string_multimap(Data) ->
    {Count, Rest} = decode_short(Data),
    decode_string_multimap(Rest, Count, []).

decode_string_multimap(Data, 0, Acc) ->
    {lists:reverse(Acc), Data};
decode_string_multimap(Data, Count, Acc) ->
    {Key, Rest0} = decode_string(Data),
    {Values, Rest1} = decode_string_list(Rest0),
    decode_string_multimap(Rest1, Count - 1, [{Key, Values}|Acc]).
