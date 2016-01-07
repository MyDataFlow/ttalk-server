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
-module(seestar_buffer).

-export([new/0, decode/2]).
-export_type([buffer/0]).

-record(buffer, {buffered :: iolist(),
                 current_size :: non_neg_integer(),
                 pending_size :: non_neg_integer() | undefined}).
-opaque buffer() :: #buffer{}.

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec new() -> buffer().
new() ->
    #buffer{buffered = [], current_size = 0, pending_size = undefined}.

-spec decode(buffer(), binary()) -> {[seestar_frame:frame()], buffer()}.
decode(#buffer{current_size = Current, pending_size = Pending} = Buffer, NewData)
        when is_integer(Pending) andalso Current + size(NewData) < Pending ->
    {[], Buffer#buffer{buffered = [NewData|Buffer#buffer.buffered],
                       current_size = Current + size(NewData)}};

decode(Buffer, NewData) ->
    Data = list_to_binary(lists:reverse([NewData|Buffer#buffer.buffered])),
    {Frames, Rest} = seestar_frame:decode(Data),
    {Frames, #buffer{buffered = [Rest],
                     current_size = size(Rest),
                     pending_size = seestar_frame:pending_size(Rest)}}.
