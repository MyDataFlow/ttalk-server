-module(ttalk_storage).
-type backend() :: ttalk_storage_riak | ttalk_storage_pg.

-spec storage_backend(backend()) -> string().
storage_backend(Backend) ->
    lists:flatten(
      ["-module(ttalk_storage_backend).
        -export([backend/0]).

        -spec backend() -> atom().
        backend() ->
            ttalk_storage_",
       atom_to_list(Backend),
       ".\n"]).