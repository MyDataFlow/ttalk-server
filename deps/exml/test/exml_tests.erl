%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Unit tests for exml application
%%%
%%% @end
%%% Created : 12 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------

-module(exml_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-compile(export_all).

application_test() ->
    ?assertEqual(ok, application:start(exml)),
    ?assertEqual(ok, application:stop(exml)).

size_of_normal_xml_test() ->
    Raw = <<"<a attr=\"urn:test:0\"><b>grzegrzółka</b><c>&amp;</c></a>"/utf8>>,
    ?assertEqual(iolist_size(Raw), exml:xml_size(parse(Raw))).

size_of_escaped_characters_test() ->
    Raw = <<"<a>&amp;</a>">>,
    ?assertEqual(iolist_size(Raw), exml:xml_size(parse(Raw))).

size_of_exml_with_cdata_test() ->
    Raw = <<"<a><![CDATA[ Within this Character Data block I can
            use double dashes as much as I want (along with <, &, ', and \")]]></a>">>,
    ?assertEqual(iolist_size(exml:to_binary(parse(Raw))), exml:xml_size(parse(Raw))).

throws_error_when_record_is_invalid_test() ->
    BadExml = #xmlel{name = <<"pp">>, attrs = 1},
    ?assertError({badxml, BadExml, _}, exml:to_binary(BadExml)).

parse(Doc) ->
    case exml:parse(Doc) of
        {ok, X} -> X;
        {error, E} -> throw(E)
    end.
