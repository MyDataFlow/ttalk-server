% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ecoveralls_utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% API
-export([
  source_file/1,
  filename_with_path/1,
  send_data/2,
  merge_options/2
]).

% API

-spec source_file(module()) -> {ok, string()} | {error, term()}.
source_file(Mod) ->
  try Mod:module_info(compile) of
    Info ->
      Source = proplists:get_value(source, Info),
      {ok, Source}
  catch
    error:undef -> {error, source_not_found}
  end.

-spec filename_with_path(string()) -> binary().
filename_with_path(SrcFile) ->
  {ok, Cwd} = file:get_cwd(),
  Cwd2 = re:replace(Cwd, "/(logs|\.eunit)/.+$", "", [{return, list}]),
  Path = string:substr(SrcFile, length(Cwd2) + 2),
  unicode:characters_to_binary(Path).

-spec send_data(string(), jsx:json_term()) -> ok | {error, string()}.
send_data(Url,  Data) ->
  Boundary = "----------ecoveralls",
  Type = "multipart/form-data; boundary=" ++ Boundary,
  Body = generate_body(Data, Boundary),
  case httpc:request(post, {Url, [], Type, Body}, [], []) of
    {ok, {{_Version, 200, _HttpMsg}, _RespHeaders, _RespBody}} -> ok;
    {ok, {{_Version, _Code, _HttpMsg}, _RespHeaders, RespBody}} -> {error, RespBody}
  end.

-spec merge_options(ecoveralls:options(), ecoveralls:options()) -> ecoveralls:options().
merge_options(ListA, ListB) ->
  DictA = orddict:from_list(ListA),
  DictB = orddict:from_list(ListB),
  MergedDict = orddict:merge(fun(_Key, _ValueA, ValueB) -> ValueB end, DictA, DictB),
  orddict:to_list(MergedDict).

% Private

-spec generate_body(jsx:json_term(), string()) -> binary().
generate_body(Data, Boundary) ->
  Boundary2 = unicode:characters_to_binary(Boundary),
  Payload = jsx:encode(Data),
  <<"--", Boundary2/binary, "\r\n",
    "Content-Disposition: form-data; name=\"json_file\"; filename=\"json_file.json\"\r\n",
    "Content-Type: application/octet-stream\r\n\r\n",
    Payload/binary, "\r\n",
    "--", Boundary2/binary, "--\r\n">>.

% Tests (private functions)

-ifdef(TEST).
generate_body_test() ->
  ?assertEqual(<<"--test\r\n",
                 "Content-Disposition: form-data; name=\"json_file\"; filename=\"json_file.json\"\r\n",
                 "Content-Type: application/octet-stream\r\n\r\n",
                 "[]\r\n",
                 "--test--\r\n">>, generate_body([], "test")),
  ?assertEqual(<<"--test\r\n",
                 "Content-Disposition: form-data; name=\"json_file\"; filename=\"json_file.json\"\r\n",
                 "Content-Type: application/octet-stream\r\n\r\n",
                 "{\"test\":[1]}\r\n",
                 "--test--\r\n">>, generate_body([{test, [1]}], "test")).
-endif.
