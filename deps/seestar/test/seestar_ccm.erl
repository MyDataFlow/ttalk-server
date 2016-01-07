-module(seestar_ccm).

-export([create/0, create/1, remove/0, start/0, stop/0]).

create() ->
    create(1).

create(Nodes) ->
    cmd("create seestar_unit_test_cluster -n ~w -b --cassandra-dir ~s",
        [Nodes, getenv("CASSANDRA_DIR", "./")]).

remove() ->
    stop(),
    cmd("remove").

start() ->
    cmd("start").

stop() ->
    cmd("stop").

cmd(Cmd) ->
    os:cmd("ccm " ++ Cmd).

cmd(Pattern, Variables) ->
    cmd(lists:flatten(io_lib:format(Pattern, Variables))).

getenv(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        Value -> Value
    end.
