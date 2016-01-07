% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cover_SUITE).

-include_lib("common_test/include/ct.hrl").

% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

% Tests
-export([
  analyse/1,
  report/1
]).

% Common Test

all() ->
  [
   analyse,
   report
  ].

init_per_suite(Config) ->
  ok = ecoveralls:start(),
  Config.

end_per_suite(_Config) ->
  ok = ecoveralls:stop(),
  ok.

% Tests

analyse(Config) ->
  DataDir = ?config(data_dir, Config),
  CoverData = filename:join(DataDir, "test.coverdata"),
  CoverageReport = ecoveralls:analyse(CoverData, []),
  {<<"service_job_id">>, null} = lists:keyfind(<<"service_job_id">>, 1, CoverageReport),
  {<<"service_name">>, null} = lists:keyfind(<<"service_name">>, 1, CoverageReport),
  {<<"source_files">>, Files} = lists:keyfind(<<"source_files">>, 1, CoverageReport),
  true = lists:any(fun(File) ->
    {<<"name">>, Name} = lists:keyfind(<<"name">>, 1, File),
    {<<"source">>, Source} = lists:keyfind(<<"source">>, 1, File),
    {<<"coverage">>, Coverage} = lists:keyfind(<<"coverage">>, 1, File),
    Source2 = binary:split(Source, <<"\n">>, [global]),
    Name =:= <<"test/cover_SUITE_data/cover_test.erl">> andalso length(Source2) =:= length(Coverage)
  end, Files).

report(Config) ->
  DataDir = ?config(data_dir, Config),
  CoverData = filename:join(DataDir, "test.coverdata"),
  % This will cause a warning, because the .coverdata has already been imported ...
  ok = ecoveralls:report(CoverData, [{url, "http://example.com"}]).
