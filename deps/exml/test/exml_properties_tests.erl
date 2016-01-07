-module(exml_properties_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml.hrl").
-compile(export_all).

p(Name, Property) ->
    ?assert(proper:quickcheck
              (proper:conjunction([{Name, Property}]),
               [100, long_result, {to_file, user}])).

parse_test() ->
    p("All valid xml cdata can be parsed",
      ?FORALL(Doc, utf8_doc(),
              is_parseable(Doc))).

serialize_test() ->
    p("All valid xml cdata can be serialized",
      ?FORALL(Doc, utf8_doc(),
              is_binary(exml:to_binary(parse(Doc))))).

inverse_test() ->
    p("exml:parse can parse the output of exml:to_binary",
      ?FORALL(Doc, utf8_doc(),
              ok == element(1, exml:parse(exml:to_binary(parse(Doc)))))).

size_test() ->
    p("exml:size equals actual size of output xml string",
      ?FORALL(Doc, utf8_doc(),
              iolist_size(exml:to_binary(parse(Doc))) == exml:xml_size(parse(Doc)))).

is_parseable(Doc) ->
    case exml:parse(Doc) of
        {ok, _} -> true;
        _ -> false
    end.

parse(Doc) ->
    case exml:parse(Doc) of
        {ok, X} -> X;
        {error, E} -> throw(E)
    end.

%%
%%  Generators
%%

utf8_doc() ->
    ?LET({{ElOpen,ElClose}, Cdata},
         {xml_open_close(), xml_cdata()},
         unicode:characters_to_binary
           (ElOpen ++ Cdata ++ ElClose)).

xml_open_close() ->
    ?LET(TagName, tagname_text(),
         {lists:flatten("<" ++ TagName ++ ">"),
          lists:flatten("</" ++ TagName ++ ">")}).

tagname_text() ->
    non_empty(list(choose($a, $z))).

utf8_char() ->
    %% see: https://en.wikipedia.org/wiki/Valid_characters_in_XML#XML_1.0
    oneof([xml_escaped_entity(),
           xml_c0_control(),
           xml_utf8_bmp_char()]).

xml_c0_control() ->
    elements([16#0009, 16#000A, 16#000D]).

xml_utf8_bmp_char() ->
    ?SUCHTHAT(C, oneof([choose(16#0020,16#D7FF),
                   choose(16#E000, 16#FFFD)]),
              not lists:member(C, [$<,$>,$&])).

xml_escaped_entity() ->
    oneof(["&amp;", "&lt;", "&gt;"]).
utf8_text() ->
    non_empty(list(utf8_char())).

xml_cdata() ->
    utf8_text().
