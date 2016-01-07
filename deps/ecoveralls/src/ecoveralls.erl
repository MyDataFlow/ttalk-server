% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ecoveralls).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(COVERALLS_URL, "https://coveralls.io/api/v1/jobs").

% Types

-type option() :: {service_job_id, binary()} | {service_name, binary()} | {url, string()}.
-type line_coverage() :: non_neg_integer() | null.

-type options() :: [option()].
-type file_coverage() :: [{binary(), binary() | [line_coverage()]}].

-export_type([
  options/0,
  file_coverage/0
]).

% Services
-export([
  travis_ci/1,
  travis_ci/2
]).

% API
-export([
  start/0,
  stop/0,
  analyse/2,
  report/2
]).

% Services

-spec travis_ci(string()) -> ok.
travis_ci(CoverData) ->
  travis_ci(CoverData, []).

-spec travis_ci(string(), options()) -> ok.
travis_ci(CoverData, Options) ->
  ok = start(),
  JobId = unicode:characters_to_binary(os:getenv("TRAVIS_JOB_ID")),
  Options2 = ecoveralls_utils:merge_options([{service_name, <<"travis-ci">>}, {service_job_id, JobId}], Options),
  ok = ecoveralls:report(CoverData, Options2),
  ok = stop().

% API

-spec start() -> ok.
start() ->
  ok = application:start(asn1),
  ok = application:start(crypto),
  ok = application:start(public_key),
  ok = application:start(ssl),
  ok = application:start(inets),
  ok = application:start(jsx),
  ok = application:start(ecoveralls),
  ok.

-spec stop() -> ok.
stop() ->
  ok = application:stop(ecoveralls),
  ok = application:stop(jsx),
  ok = application:stop(inets),
  ok = application:stop(ssl),
  ok = application:stop(public_key),
  ok = application:stop(crypto),
  ok = application:stop(asn1),
  ok.

-spec analyse(string(), options()) -> jsx:json_term().
analyse(CoverData, Options) ->
  ok = cover:import(CoverData),
  ServiceJobId = proplists:get_value(service_job_id, Options, null),
  ServiceName = proplists:get_value(service_name, Options, null),
  FileCoverage = coverage_report(cover:imported_modules()),
  [{<<"service_job_id">>, ServiceJobId}, {<<"service_name">>, ServiceName}, {<<"source_files">>, FileCoverage}].

-spec report(string(), options()) -> ok.
report(CoverData, Options) ->
  case analyse(CoverData, Options) of
    Data ->
      Url = proplists:get_value(url, Options, ?COVERALLS_URL),
      case ecoveralls_utils:send_data(Url, Data) of
        ok -> ok;
        {error, Msg} ->
          io:format(user, "~p~n", [Msg]),
          ok
      end
  end.

% Private

-spec coverage_report([module()]) -> [file_coverage()].
coverage_report(Modules) ->
  coverage_report(Modules, []).

-spec coverage_report([module()], [file_coverage()]) -> [file_coverage()].
coverage_report([], Acc) ->
  Acc;
coverage_report([Mod|Rest], Acc) ->
  case file_coverage(Mod) of
    {ok, FileCoverage} -> coverage_report(Rest, [FileCoverage | Acc]);
    {error, _Reason} -> coverage_report(Rest, Acc)
  end.

-spec file_coverage(module()) -> {ok, file_coverage()} | {error, term()}.
file_coverage(Mod) ->
  case ecoveralls_utils:source_file(Mod) of
    {ok, SourceFile} ->
      {ok, Source} = file:read_file(SourceFile),
      SourceLines = binary:split(Source, <<"\n">>, [global]),
      {ok, Coverage} = cover:analyse(Mod, calls, line),
      CoverageLines = line_coverage(1, length(SourceLines), Coverage, []),
      FileCoverage = [{<<"name">>, ecoveralls_utils:filename_with_path(SourceFile)}, {<<"source">>, Source}, {<<"coverage">>, CoverageLines}],
      {ok, FileCoverage};
    {error, _Reason}=E -> E
  end.

-spec line_coverage(pos_integer(), pos_integer(), list(), [line_coverage()]) -> [line_coverage()].
line_coverage(Line, EndLine, _Coverage, Acc) when Line > EndLine ->
  lists:reverse(Acc);
line_coverage(Line, EndLine, [], Acc) ->
  line_coverage(Line + 1, EndLine, [], [null | Acc]);
line_coverage(1, EndLine, [{{_Mod, 0}, _Calls}|Rest], Acc) ->
  line_coverage(1, EndLine, Rest, Acc);
line_coverage(Line, EndLine, [{{_Mod, Line}, Calls}|Rest], Acc) ->
  line_coverage(Line + 1, EndLine, Rest, [Calls | Acc]);
line_coverage(Line, EndLine, Coverage, Acc) ->
  line_coverage(Line + 1, EndLine, Coverage, [null | Acc]).

% Tests (private functions)

-ifdef(TEST).
coverage_report_test() ->
  ?assertEqual([], coverage_report([nothing], [])).

line_coverage_test() ->
  ?assertEqual([null, null], line_coverage(1, 2, [{{test, 0}, 9001}], [])),
  ?assertEqual([], line_coverage(3, 2, [{{test, 0}, 9001}], [])).
-endif.
