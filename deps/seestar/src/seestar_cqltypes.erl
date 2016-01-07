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

-module(seestar_cqltypes).

-include("builtin_types.hrl").

-export([decode_value_with_size/2, encode_value_with_size/2, decode_type/1]).

-type type() :: native() | {list | set, native()} | {map, native(), native()} | {custom, string()}.
-type native() :: ascii | bigint | blob | boolean | counter | decimal | double |
                  float | int | text | timestamp | uuid | varchar | varint |
                  timeuuid | inet.
-type decimal() :: {Unscaled :: integer(), Scale :: integer()}.
-type value() :: null | integer() | binary() | boolean() | float() | inet:ip_address() |
                 decimal() | list() | dict_t() | set_t().
-export_type([type/0, value/0]).

%% -------------------------------------------------------------------------
%% decoding values
%% -------------------------------------------------------------------------

%% @private
-spec decode_value_with_size(type(), binary()) -> {value(), binary()}.
decode_value_with_size(_, <<16#FF, 16#FF, 16#FF, 16#FF, Rest/binary>>) ->
    {null, Rest};
decode_value_with_size(Type, Data) ->
    {Bytes, Rest} = seestar_types:decode_bytes(Data),
    {decode_value(Type, Bytes), Rest}.

-spec decode_value(type(), binary()) -> value().
decode_value(bigint, <<Value:64/signed>>) ->
    Value;

decode_value(boolean, <<0>>) ->
    false;

decode_value(boolean, <<1>>) ->
    true;

decode_value(counter, <<Value:64/signed>>) ->
    Value;

decode_value(decimal, Bytes) ->
    % don't convert to float - will loose precision.
    <<Scale:32/signed, Rest/binary>> = Bytes,
    {decode_value(varint, Rest), Scale};

decode_value(double, <<Value:64/float>>) ->
    Value;

decode_value(float, <<Value:32/float>>) ->
    Value;

decode_value(int, <<Value:32/signed>>) ->
    Value;

decode_value(timestamp, <<Millis:64/signed>>) ->
    {Millis div 1000000000,
     (Millis div 1000) rem 1000000,
     (Millis * 1000) rem 1000000};

decode_value(varint, Bytes) ->
    Size = size(Bytes),
    <<Value:Size/signed-unit:8>> = Bytes,
    Value;

decode_value(inet, Bytes) when size(Bytes) =:= 4 ->
    list_to_tuple(binary_to_list(Bytes));

decode_value(inet, Bytes) when size(Bytes) =:= 16 ->
    <<S1:16, S2:16, S3:16, S4:16, S5:16, S6:16, S7:16, S8:16>> = Bytes,
    {S1, S2, S3, S4, S5, S6, S7, S8};

decode_value({list, Type}, Bytes) ->
    decode_list(Type, Bytes);

decode_value({map, KeyType, ValueType}, Bytes) ->
    decode_map(KeyType, ValueType, Bytes);

decode_value({set, Type}, Bytes) ->
    decode_set(Type, Bytes);

decode_value(_Type, Bytes) ->
    % Don't perform any transformations on values of other types.
    Bytes.

%% -------------------------------------------------------------------------
%% decoding collections
%% -------------------------------------------------------------------------

decode_list(Type, Data) ->
    {Length, Rest} = seestar_types:decode_short(Data),
    decode_list(Type, Length, Rest, []).

decode_list(_, 0, _, List) ->
    lists:reverse(List);
decode_list(Type, Length, Data, List) ->
    {Bytes, Rest} = seestar_types:decode_short_bytes(Data),
    decode_list(Type, Length - 1, Rest, [decode_value(Type, Bytes)|List]).

decode_map(KeyType, ValueType, Data) ->
    {Size, Rest} = seestar_types:decode_short(Data),
    decode_map(KeyType, ValueType, Size, Rest, dict:new()).

decode_map(_, _, 0, _, Dict) ->
    Dict;
decode_map(KeyType, ValueType, Size, Data, Dict) ->
    {KeyBytes, Rest0} = seestar_types:decode_short_bytes(Data),
    {ValueBytes, Rest1} = seestar_types:decode_short_bytes(Rest0),
    Key = decode_value(KeyType, KeyBytes),
    Value = decode_value(ValueType, ValueBytes),
    decode_map(KeyType, ValueType, Size - 1, Rest1, dict:store(Key, Value, Dict)).

decode_set(Type, Data) ->
    {Size, Rest} = seestar_types:decode_short(Data),
    decode_set(Type, Size, Rest, sets:new()).

decode_set(_, 0, _, Set) ->
    Set;
decode_set(Type, Size, Data, Set) ->
    {Bytes, Rest} = seestar_types:decode_short_bytes(Data),
    decode_set(Type, Size - 1, Rest, sets:add_element(decode_value(Type, Bytes), Set)).

%% -------------------------------------------------------------------------
%% encoding values
%% -------------------------------------------------------------------------

%% @private
-spec encode_value_with_size(type(), value()) -> binary().
encode_value_with_size(_, null) ->
    <<16#FF, 16#FF, 16#FF, 16#FF>>;
encode_value_with_size(Type, Value) ->
    seestar_types:encode_bytes(encode_value(Type, Value)).

-spec encode_value(type(), value()) -> binary().
encode_value(bigint, Value) ->
    <<Value:64/signed>>;

encode_value(boolean, false) ->
    <<0>>;

encode_value(boolean, true) ->
    <<1>>;

encode_value(counter, Value) ->
    <<Value:64/signed>>;

encode_value(decimal, {Unscaled, Scale}) ->
    %% don't convert to float - will loose precision.
    <<Scale:32/signed, (encode_value(varint, Unscaled))/binary>>;

encode_value(double, Value) ->
    <<Value:64/float>>;

encode_value(float, Value) ->
    <<Value:32/float>>;

encode_value(int, Value) ->
    <<Value:32/signed>>;

encode_value(timestamp, {MegaSeconds, Seconds, MicroSeconds}) ->
    <<((MegaSeconds * 1000000 + Seconds) * 1000 + MicroSeconds div 1000):64/signed>>;

encode_value(varint, Value) when Value >= 0 ->
    encode_varint_pos(Value, []);
encode_value(varint, Value) ->
    encode_varint_neg(Value, []);

encode_value(inet, {O1, O2, O3, O4}) ->
    <<O1, O2, O3, O4>>;

encode_value(inet, {S1, S2, S3, S4, S5, S6, S7, S8}) ->
    <<S1:16, S2:16, S3:16, S4:16, S5:16, S6:16, S7:16, S8:16>>;

encode_value({list, Type}, List) ->
    encode_list(Type, List);

encode_value({map, KeyType, ValueType}, Dict) ->
    encode_map(KeyType, ValueType, Dict);

encode_value({set, Type}, Set) ->
    encode_set(Type, Set);

encode_value(_Type, Value) ->
    % Don't perform any transformations on values of other types.
    Value.

encode_varint_pos(0, Ds = [_|_]) ->
    list_to_binary(Ds);
encode_varint_pos(X, Ds) ->
    encode_varint_pos(X bsr 8, [(X band 255)|Ds]).

encode_varint_neg(-1, Ds=[MSB|_]) when MSB >= 16#80 ->
    list_to_binary(Ds);
encode_varint_neg(X, Ds) ->
    encode_varint_neg(X bsr 8, [(X band 255)|Ds]).

%% -------------------------------------------------------------------------
%% encoding collections
%% -------------------------------------------------------------------------

encode_list(Type, List) ->
    Encoded = [ seestar_types:encode_short_bytes(encode_value(Type, V)) || V <- List ],
    <<(seestar_types:encode_short(length(List)))/binary,
      (list_to_binary(Encoded))/binary>>.

encode_set(Type, Set) ->
    Encoded = sets:fold(fun(V, Acc) ->
                            [seestar_types:encode_short_bytes(encode_value(Type, V))|Acc]
                        end, [], Set),
    <<(seestar_types:encode_short(sets:size(Set)))/binary,
      (list_to_binary(Encoded))/binary>>.

encode_map(KeyType, ValueType, Dict) ->
    F = fun(K, V, Acc) ->
            [<<(seestar_types:encode_short_bytes(encode_value(KeyType, K)))/binary,
               (seestar_types:encode_short_bytes(encode_value(ValueType, V)))/binary>>|Acc]
         end,
    <<(seestar_types:encode_short(dict:size(Dict)))/binary,
      (list_to_binary(dict:fold(F, [], Dict)))/binary>>.

%% -------------------------------------------------------------------------
%% decoding the type itself
%% -------------------------------------------------------------------------

%% @private
-spec decode_type(binary()) -> {type(), binary()}.
decode_type(Data) ->
    {Code, Rest0} = seestar_types:decode_short(Data),
    case Code of
        16#00 ->
            {Class, Rest1} = seestar_types:decode_string(Rest0),
            {{custom, binary_to_list(Class)}, Rest1};
        _ when Code >= 16#01, Code =< 16#10 ->
            {code_to_type(Code), Rest0};
        16#20 ->
            {Subtype, Rest1} = decode_type(Rest0),
            {{list, Subtype}, Rest1};
        16#21 ->
            {KeyType, Rest1} = decode_type(Rest0),
            {ValueType, Rest2} = decode_type(Rest1),
            {{map, KeyType, ValueType}, Rest2};
        16#22 ->
            {Subtype, Rest1} = decode_type(Rest0),
            {{set, Subtype}, Rest1}
    end.

code_to_type(16#01) -> ascii;
code_to_type(16#02) -> bigint;
code_to_type(16#03) -> blob;
code_to_type(16#04) -> boolean;
code_to_type(16#05) -> counter;
code_to_type(16#06) -> decimal;
code_to_type(16#07) -> double;
code_to_type(16#08) -> float;
code_to_type(16#09) -> int;
code_to_type(16#0A) -> text; % never actually occurs.
code_to_type(16#0B) -> timestamp;
code_to_type(16#0C) -> uuid;
code_to_type(16#0D) -> varchar;
code_to_type(16#0E) -> varint;
code_to_type(16#0F) -> timeuuid;
code_to_type(16#10) -> inet.
