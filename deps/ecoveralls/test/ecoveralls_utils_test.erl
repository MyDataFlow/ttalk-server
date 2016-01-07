% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ecoveralls_utils_test).

-include_lib("eunit/include/eunit.hrl").

source_file_test() ->
  {ok, SrcFile} = ecoveralls_utils:source_file(?MODULE),
  ?assertEqual(<<"test/ecoveralls_utils_test.erl">>, ecoveralls_utils:filename_with_path(SrcFile)),
  ?assertEqual({error, source_not_found}, ecoveralls_utils:source_file(this_does_not_exist)).

merge_options_test() ->
  ?assertEqual([{service_name, <<"test">>}], ecoveralls_utils:merge_options([], [{service_name, <<"test">>}])),
  ?assertEqual([{service_name, <<"test">>}], ecoveralls_utils:merge_options([{service_name, <<"foo">>}], [{service_name, <<"test">>}])),
  ?assertEqual([{service_job_id, <<"123">>}, {service_name, <<"test">>}], ecoveralls_utils:merge_options([{service_name, <<"test">>}], [{service_job_id, <<"123">>}])).
