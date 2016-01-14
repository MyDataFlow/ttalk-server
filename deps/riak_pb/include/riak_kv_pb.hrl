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

-ifndef(RPBGETCLIENTIDRESP_PB_H).
-define(RPBGETCLIENTIDRESP_PB_H, true).
-record(rpbgetclientidresp, {
    client_id = erlang:error({required, client_id})
}).
-endif.

-ifndef(RPBSETCLIENTIDREQ_PB_H).
-define(RPBSETCLIENTIDREQ_PB_H, true).
-record(rpbsetclientidreq, {
    client_id = erlang:error({required, client_id})
}).
-endif.

-ifndef(RPBGETREQ_PB_H).
-define(RPBGETREQ_PB_H, true).
-record(rpbgetreq, {
    bucket = erlang:error({required, bucket}),
    key = erlang:error({required, key}),
    r,
    pr,
    basic_quorum,
    notfound_ok,
    if_modified,
    head,
    deletedvclock,
    timeout,
    sloppy_quorum,
    n_val,
    type
}).
-endif.

-ifndef(RPBGETRESP_PB_H).
-define(RPBGETRESP_PB_H, true).
-record(rpbgetresp, {
    content = [],
    vclock,
    unchanged
}).
-endif.

-ifndef(RPBPUTREQ_PB_H).
-define(RPBPUTREQ_PB_H, true).
-record(rpbputreq, {
    bucket = erlang:error({required, bucket}),
    key,
    vclock,
    content = erlang:error({required, content}),
    w,
    dw,
    return_body,
    pw,
    if_not_modified,
    if_none_match,
    return_head,
    timeout,
    asis,
    sloppy_quorum,
    n_val,
    type
}).
-endif.

-ifndef(RPBPUTRESP_PB_H).
-define(RPBPUTRESP_PB_H, true).
-record(rpbputresp, {
    content = [],
    vclock,
    key
}).
-endif.

-ifndef(RPBDELREQ_PB_H).
-define(RPBDELREQ_PB_H, true).
-record(rpbdelreq, {
    bucket = erlang:error({required, bucket}),
    key = erlang:error({required, key}),
    rw,
    vclock,
    r,
    w,
    pr,
    pw,
    dw,
    timeout,
    sloppy_quorum,
    n_val,
    type
}).
-endif.

-ifndef(RPBLISTBUCKETSREQ_PB_H).
-define(RPBLISTBUCKETSREQ_PB_H, true).
-record(rpblistbucketsreq, {
    timeout,
    stream,
    type
}).
-endif.

-ifndef(RPBLISTBUCKETSRESP_PB_H).
-define(RPBLISTBUCKETSRESP_PB_H, true).
-record(rpblistbucketsresp, {
    buckets = [],
    done
}).
-endif.

-ifndef(RPBLISTKEYSREQ_PB_H).
-define(RPBLISTKEYSREQ_PB_H, true).
-record(rpblistkeysreq, {
    bucket = erlang:error({required, bucket}),
    timeout,
    type
}).
-endif.

-ifndef(RPBLISTKEYSRESP_PB_H).
-define(RPBLISTKEYSRESP_PB_H, true).
-record(rpblistkeysresp, {
    keys = [],
    done
}).
-endif.

-ifndef(RPBMAPREDREQ_PB_H).
-define(RPBMAPREDREQ_PB_H, true).
-record(rpbmapredreq, {
    request = erlang:error({required, request}),
    content_type = erlang:error({required, content_type})
}).
-endif.

-ifndef(RPBMAPREDRESP_PB_H).
-define(RPBMAPREDRESP_PB_H, true).
-record(rpbmapredresp, {
    phase,
    response,
    done
}).
-endif.

-ifndef(RPBINDEXREQ_PB_H).
-define(RPBINDEXREQ_PB_H, true).
-record(rpbindexreq, {
    bucket = erlang:error({required, bucket}),
    index = erlang:error({required, index}),
    qtype = erlang:error({required, qtype}),
    key,
    range_min,
    range_max,
    return_terms,
    stream,
    max_results,
    continuation,
    timeout,
    type,
    term_regex,
    pagination_sort
}).
-endif.

-ifndef(RPBINDEXRESP_PB_H).
-define(RPBINDEXRESP_PB_H, true).
-record(rpbindexresp, {
    keys = [],
    results = [],
    continuation,
    done
}).
-endif.

-ifndef(RPBCSBUCKETREQ_PB_H).
-define(RPBCSBUCKETREQ_PB_H, true).
-record(rpbcsbucketreq, {
    bucket = erlang:error({required, bucket}),
    start_key = erlang:error({required, start_key}),
    end_key,
    start_incl = true,
    end_incl = false,
    continuation,
    max_results,
    timeout,
    type
}).
-endif.

-ifndef(RPBCSBUCKETRESP_PB_H).
-define(RPBCSBUCKETRESP_PB_H, true).
-record(rpbcsbucketresp, {
    objects = [],
    continuation,
    done
}).
-endif.

-ifndef(RPBINDEXOBJECT_PB_H).
-define(RPBINDEXOBJECT_PB_H, true).
-record(rpbindexobject, {
    key = erlang:error({required, key}),
    object = erlang:error({required, object})
}).
-endif.

-ifndef(RPBCONTENT_PB_H).
-define(RPBCONTENT_PB_H, true).
-record(rpbcontent, {
    value = erlang:error({required, value}),
    content_type,
    charset,
    content_encoding,
    vtag,
    links = [],
    last_mod,
    last_mod_usecs,
    usermeta = [],
    indexes = [],
    deleted
}).
-endif.

-ifndef(RPBLINK_PB_H).
-define(RPBLINK_PB_H, true).
-record(rpblink, {
    bucket,
    key,
    tag
}).
-endif.

-ifndef(RPBCOUNTERUPDATEREQ_PB_H).
-define(RPBCOUNTERUPDATEREQ_PB_H, true).
-record(rpbcounterupdatereq, {
    bucket = erlang:error({required, bucket}),
    key = erlang:error({required, key}),
    amount = erlang:error({required, amount}),
    w,
    dw,
    pw,
    returnvalue
}).
-endif.

-ifndef(RPBCOUNTERUPDATERESP_PB_H).
-define(RPBCOUNTERUPDATERESP_PB_H, true).
-record(rpbcounterupdateresp, {
    value
}).
-endif.

-ifndef(RPBCOUNTERGETREQ_PB_H).
-define(RPBCOUNTERGETREQ_PB_H, true).
-record(rpbcountergetreq, {
    bucket = erlang:error({required, bucket}),
    key = erlang:error({required, key}),
    r,
    pr,
    basic_quorum,
    notfound_ok
}).
-endif.

-ifndef(RPBCOUNTERGETRESP_PB_H).
-define(RPBCOUNTERGETRESP_PB_H, true).
-record(rpbcountergetresp, {
    value
}).
-endif.

-ifndef(RPBGETBUCKETKEYPREFLISTREQ_PB_H).
-define(RPBGETBUCKETKEYPREFLISTREQ_PB_H, true).
-record(rpbgetbucketkeypreflistreq, {
    bucket = erlang:error({required, bucket}),
    key = erlang:error({required, key}),
    type
}).
-endif.

-ifndef(RPBGETBUCKETKEYPREFLISTRESP_PB_H).
-define(RPBGETBUCKETKEYPREFLISTRESP_PB_H, true).
-record(rpbgetbucketkeypreflistresp, {
    preflist = []
}).
-endif.

-ifndef(RPBBUCKETKEYPREFLISTITEM_PB_H).
-define(RPBBUCKETKEYPREFLISTITEM_PB_H, true).
-record(rpbbucketkeypreflistitem, {
    partition = erlang:error({required, partition}),
    node = erlang:error({required, node}),
    primary = erlang:error({required, primary})
}).
-endif.

