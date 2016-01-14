-ifndef(RPBERRORRESP_PB_H).
-define(RPBERRORRESP_PB_H, true).
-record(rpberrorresp, {
    errmsg = erlang:error({required, errmsg}),
    errcode = erlang:error({required, errcode})
}).
-endif.

-ifndef(RPBGETSERVERINFORESP_PB_H).
-define(RPBGETSERVERINFORESP_PB_H, true).
-record(rpbgetserverinforesp, {
    node,
    server_version
}).
-endif.

-ifndef(RPBPAIR_PB_H).
-define(RPBPAIR_PB_H, true).
-record(rpbpair, {
    key = erlang:error({required, key}),
    value
}).
-endif.

-ifndef(RPBGETBUCKETREQ_PB_H).
-define(RPBGETBUCKETREQ_PB_H, true).
-record(rpbgetbucketreq, {
    bucket = erlang:error({required, bucket}),
    type
}).
-endif.

-ifndef(RPBGETBUCKETRESP_PB_H).
-define(RPBGETBUCKETRESP_PB_H, true).
-record(rpbgetbucketresp, {
    props = erlang:error({required, props})
}).
-endif.

-ifndef(RPBSETBUCKETREQ_PB_H).
-define(RPBSETBUCKETREQ_PB_H, true).
-record(rpbsetbucketreq, {
    bucket = erlang:error({required, bucket}),
    props = erlang:error({required, props}),
    type
}).
-endif.

-ifndef(RPBRESETBUCKETREQ_PB_H).
-define(RPBRESETBUCKETREQ_PB_H, true).
-record(rpbresetbucketreq, {
    bucket = erlang:error({required, bucket}),
    type
}).
-endif.

-ifndef(RPBGETBUCKETTYPEREQ_PB_H).
-define(RPBGETBUCKETTYPEREQ_PB_H, true).
-record(rpbgetbuckettypereq, {
    type = erlang:error({required, type})
}).
-endif.

-ifndef(RPBSETBUCKETTYPEREQ_PB_H).
-define(RPBSETBUCKETTYPEREQ_PB_H, true).
-record(rpbsetbuckettypereq, {
    type = erlang:error({required, type}),
    props = erlang:error({required, props})
}).
-endif.

-ifndef(RPBMODFUN_PB_H).
-define(RPBMODFUN_PB_H, true).
-record(rpbmodfun, {
    module = erlang:error({required, module}),
    function = erlang:error({required, function})
}).
-endif.

-ifndef(RPBCOMMITHOOK_PB_H).
-define(RPBCOMMITHOOK_PB_H, true).
-record(rpbcommithook, {
    modfun,
    name
}).
-endif.

-ifndef(RPBBUCKETPROPS_PB_H).
-define(RPBBUCKETPROPS_PB_H, true).
-record(rpbbucketprops, {
    n_val,
    allow_mult,
    last_write_wins,
    precommit = [],
    has_precommit = false,
    postcommit = [],
    has_postcommit = false,
    chash_keyfun,
    linkfun,
    old_vclock,
    young_vclock,
    big_vclock,
    small_vclock,
    pr,
    r,
    w,
    pw,
    dw,
    rw,
    basic_quorum,
    notfound_ok,
    backend,
    search,
    repl,
    search_index,
    datatype,
    consistent,
    write_once
}).
-endif.

-ifndef(RPBAUTHREQ_PB_H).
-define(RPBAUTHREQ_PB_H, true).
-record(rpbauthreq, {
    user = erlang:error({required, user}),
    password = erlang:error({required, password})
}).
-endif.

-ifndef(RPBSEARCHDOC_PB_H).
-define(RPBSEARCHDOC_PB_H, true).
-record(rpbsearchdoc, {
    fields = []
}).
-endif.

-ifndef(RPBSEARCHQUERYREQ_PB_H).
-define(RPBSEARCHQUERYREQ_PB_H, true).
-record(rpbsearchqueryreq, {
    q = erlang:error({required, q}),
    index = erlang:error({required, index}),
    rows,
    start,
    sort,
    filter,
    df,
    op,
    fl = [],
    presort
}).
-endif.

-ifndef(RPBSEARCHQUERYRESP_PB_H).
-define(RPBSEARCHQUERYRESP_PB_H, true).
-record(rpbsearchqueryresp, {
    docs = [],
    max_score,
    num_found
}).
-endif.

