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
-module(seestar_frame).

-export([new/4, id/1, flags/1, has_flag/2, opcode/1,
         body/1, encode/1, pending_size/1, decode/1]).

-type stream_id() :: -1..127.
-type flag() :: compression | tracing.
-type opcode() :: 16#00..16#0C.
-export_type([stream_id/0, flag/0, opcode/0, frame/0]).

-define(COMPRESSION, 16#01).
-define(TRACING, 16#02).

-record(frame, {id :: stream_id(),
                flags = [] :: [flag()],
                opcode :: opcode(),
                body :: binary()}).
-opaque frame() :: #frame{}.

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec new(stream_id(), [flag()], opcode(), binary()) -> frame().
new(ID, Flags, Op, Body) ->
    #frame{id = ID, flags = Flags, opcode = Op, body = Body}.

-spec id(frame()) -> stream_id().
id(Frame) ->
    Frame#frame.id.

-spec flags(frame()) -> [flag()].
flags(Frame) ->
    Frame#frame.flags.

-spec has_flag(frame(), flag()) -> boolean().
has_flag(Frame, Flag) ->
    lists:member(Flag, flags(Frame)).

-spec opcode(frame()) -> opcode().
opcode(Frame) ->
    Frame#frame.opcode.

-spec body(frame()) -> binary().
body(Frame) ->
    Frame#frame.body.

-spec encode(frame()) -> binary().
encode(#frame{id = ID, flags = Flags, opcode = Op, body = Body}) ->
    <<16#01, (encode_flags(Flags)), ID/signed, Op, (size(Body)):32, Body/binary>>.

encode_flags(Flags) ->
    lists:foldl(fun(Flag, Byte) -> encode_flag(Flag) bor Byte end, 0, Flags).

encode_flag(compression) -> ?COMPRESSION;
encode_flag(tracing)     -> ?TRACING.

-spec pending_size(binary()) -> pos_integer().
pending_size(<<16#81, _Flags, _ID/signed, _Op, Size:32, _/binary>>) ->
    Size + 8;
pending_size(_) ->
    undefined.

-spec decode(binary()) -> {[frame()], binary()}.
decode(Stream) ->
    decode(Stream, []).
decode(<<16#81, Flags, ID/signed, Op, Size:32, Body:Size/binary, Rest/binary>>, Acc) ->
    Frame = #frame{id = ID, flags = decode_flags(Flags), opcode = Op, body = Body},
    decode(Rest, [Frame|Acc]);
decode(Stream, Acc) ->
    {lists:reverse(Acc), Stream}.

decode_flags(Byte) ->
    F = fun(Mask, Flags) when Byte band Mask =:= Mask ->
            [decode_flag(Mask)|Flags];
           (_Mask, Flags) ->
            Flags
        end,
    lists:foldl(F, [], [?COMPRESSION, ?TRACING]).

decode_flag(?COMPRESSION) -> compression;
decode_flag(?TRACING)     -> tracing.
