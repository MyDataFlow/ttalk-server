-module(seestar_session_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("seestar/include/constants.hrl").

session_test_() ->
    {foreach,
     fun() ->
         seestar_ccm:create(),
         seestar_ccm:start(),
         timer:sleep(500),
         {ok, Pid} = seestar_session:start_link("localhost", 9042),
         unlink(Pid),
         Pid
     end,
     fun(Pid) ->
         seestar_session:stop(Pid),
         seestar_ccm:remove()
     end,
     [fun(Pid) -> {with, Pid, [fun test_schema_queries/1]} end,
      fun(Pid) -> {with, Pid, [fun test_native_types/1]} end,
      fun(Pid) -> {with, Pid, [fun test_collection_types/1]} end,
      fun(Pid) -> {with, Pid, [fun test_counter_type/1]} end]}.

test_schema_queries(Pid) ->
    Qry0 = "CREATE KEYSPACE seestar "
           "WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': 1}",
    {ok, Res0} = seestar_session:perform(Pid, Qry0, one),
    ?assertEqual(schema_change, seestar_result:type(Res0)),
    ?assertEqual(<<"seestar">>, seestar_result:keyspace(Res0)),
    ?assertEqual(undefined, seestar_result:table(Res0)),

    {error, Err0} = seestar_session:perform(Pid, Qry0, one),
    ?assertEqual(?ALREADY_EXISTS, seestar_error:code(Err0)),
    ?assertEqual(<<"seestar">>, seestar_error:keyspace(Err0)),
    ?assertEqual(undefined, seestar_error:table(Err0)),

    Qry1 = "USE seestar",
    {ok, Res1} = seestar_session:perform(Pid, Qry1, one),
    ?assertEqual(set_keyspace, seestar_result:type(Res1)),
    ?assertEqual(<<"seestar">>, seestar_result:keyspace(Res1)),

    Qry2 = "CREATE TABLE seestar_test_table (id int primary key, value text)",
    {ok, Res2} = seestar_session:perform(Pid, Qry2, one),
    ?assertEqual(schema_change, seestar_result:type(Res2)),
    ?assertEqual(<<"seestar">>, seestar_result:keyspace(Res2)),
    ?assertEqual(<<"seestar_test_table">>, seestar_result:table(Res2)),

    {error, Err1} = seestar_session:perform(Pid, Qry2, one),
    ?assertEqual(?ALREADY_EXISTS, seestar_error:code(Err1)),
    ?assertEqual(<<"seestar">>, seestar_error:keyspace(Err1)),
    ?assertEqual(<<"seestar_test_table">>, seestar_error:table(Err1)).

test_native_types(Pid) ->
    create_keyspace(Pid, "seestar", 1),
    Qry0 = "CREATE TABLE seestar.has_all_types (
                asciicol ascii,
                bigintcol bigint,
                blobcol blob,
                booleancol boolean,
                decimalcol decimal,
                doublecol double,
                floatcol float,
                inetcol inet,
                intcol int,
                textcol text,
                timestampcol timestamp,
                timeuuidcol timeuuid,
                uuidcol uuid,
                varcharcol varchar,
                varintcol varint,
                PRIMARY KEY(asciicol)
            )",
    {ok, _} = seestar_session:perform(Pid, Qry0, one),
    % test serialization.
    Qry1 = "INSERT INTO seestar.has_all_types (
               asciicol, bigintcol, blobcol, booleancol, decimalcol, doublecol, floatcol,
               inetcol, intcol, textcol, timestampcol, timeuuidcol, uuidcol, varcharcol, varintcol)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    {ok, Res1} = seestar_session:prepare(Pid, Qry1),
    QryID = seestar_result:query_id(Res1),
    Types = seestar_result:types(Res1),
    ?assertEqual([ascii, bigint, blob, boolean, decimal, double, float,
                  inet, int, varchar, timestamp, timeuuid, uuid, varchar, varint],
                 Types),
    Row0 = [<<"abcd">>, 1234567890123456789, <<4,2>>, true, {1995211882, 5},
            9999999.999, 99999.9921875, {127,0,0,1}, 100, <<"Voilá!">>, {1368,199874,337000},
            <<146,135,233,168,39,16,17,187,131,194,96,197,71,12,191,14>>,
            <<113,68,80,223,85,99,74,129,188,158,84,49,50,156,40,232>>,
            <<>>, 10000000000000000000000000],
    Row1 = [<<"cdef">>, 1234567890123456789, <<2,4>>, false, {1995211882, 6}, 9999999.999,
            99999.9921875, {255,255,255,255,255,255,255,255}, 200, <<"текст">>, {1368,199874,337000},
            <<135,99,103,104,40,81,17,187,181,58,96,197,71,12,191,14>>,
            <<148,125,144,228,220,27,68,12,148,158,178,154,25,169,42,113>>,
            <<>>, 100000000000000000000000000],
    {ok, _} = seestar_session:execute(Pid, QryID, Types, Row0, one),
    {ok, _} = seestar_session:execute(Pid, QryID, Types, Row1, one),
    % test deserialization.
    Qry2 = "SELECT asciicol, bigintcol, blobcol, booleancol, decimalcol, doublecol, floatcol,
                   inetcol, intcol, textcol, timestampcol, timeuuidcol, uuidcol, varcharcol, varintcol
            FROM seestar.has_all_types",
    {ok, Res2} = seestar_session:perform(Pid, Qry2, one),
    ?assertEqual(Types, seestar_result:types(Res2)),
    ?assertEqual([Row0, Row1], seestar_result:rows(Res2)).

test_counter_type(Pid) ->
    create_keyspace(Pid, "seestar", 1),
    Qry0 = "CREATE TABLE seestar.has_counter_type (id int PRIMARY KEY, counter counter)",
    {ok, _} = seestar_session:perform(Pid, Qry0, one),
    Qry1 = "UPDATE seestar.has_counter_type SET counter = counter + ? WHERE id = ?",
    {ok, Res1} = seestar_session:prepare(Pid, Qry1),
    QryID = seestar_result:query_id(Res1),
    Types = seestar_result:types(Res1),
    [ {ok, _} = seestar_session:execute(Pid, QryID, Types, [C, 0], one) || C <- [ 1, -2, 3 ] ],
    Qry2 = "SELECT id, counter FROM seestar.has_counter_type WHERE id = 0",
    {ok, Res2} = seestar_session:perform(Pid, Qry2, one),
    ?assertEqual([[0, 2]], seestar_result:rows(Res2)).

test_collection_types(Pid) ->
    create_keyspace(Pid, "seestar", 1),
    Qry0 = "CREATE TABLE seestar.has_collection_types (
                id int,
                mapcol map<text,blob>,
                setcol set<int>,
                listcol list<boolean>,
                PRIMARY KEY(id)
            )",
    {ok, _} = seestar_session:perform(Pid, Qry0, one),
    Qry1 = "INSERT INTO seestar.has_collection_types (id, mapcol, setcol, listcol) VALUES (?, ?, ?, ?)",
    {ok, Res1} = seestar_session:prepare(Pid, Qry1),
    QryID = seestar_result:query_id(Res1),
    Types = seestar_result:types(Res1),
    Row0 = [0, null, null, null],
    Row1 = [1, dict:from_list([{<<"k1">>, <<"v1">>}]), sets:from_list([1]), [true]],
    Row2 = [2, dict:from_list([{<<"k1">>, <<"v1">>}, {<<"k2">>, <<"v2">>}]), sets:from_list([1,2]), [true, false]],
    [ {ok, _} = seestar_session:execute(Pid, QryID, Types, R, one) || R <- [Row0, Row1, Row2] ],
    Qry2 = "SELECT id, mapcol, setcol, listcol FROM seestar.has_collection_types",
    {ok, Res2} = seestar_session:perform(Pid, Qry2, one),
    ?assertEqual([Row1, Row0, Row2], seestar_result:rows(Res2)).

create_keyspace(Pid, Name, RF) ->
    Qry = "CREATE KEYSPACE ~s WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': ~w}",
    {ok, _} = seestar_session:perform(Pid, lists:flatten(io_lib:format(Qry, [Name, RF])), one).
