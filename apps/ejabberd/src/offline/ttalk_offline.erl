-module(ttalk_offline).
-type backend() :: ttalk_offline_riak | ttalk_offline_pg.

-spec offline_backend(backend()) -> string().
offline_backend(Backend) ->
    lists:flatten(
      ["-module(ttalk_offline_backend).
        -export([backend/0]).

        -spec backend() -> atom().
        backend() ->
            ttalk_offline_",
       atom_to_list(Backend),
       ".\n"]).