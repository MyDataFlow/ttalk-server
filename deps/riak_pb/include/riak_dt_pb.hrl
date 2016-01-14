-ifndef(MAPFIELD_PB_H).
-define(MAPFIELD_PB_H, true).
-record(mapfield, {
    name = erlang:error({required, name}),
    type = erlang:error({required, type})
}).
-endif.

-ifndef(MAPENTRY_PB_H).
-define(MAPENTRY_PB_H, true).
-record(mapentry, {
    field = erlang:error({required, field}),
    counter_value,
    set_value = [],
    register_value,
    flag_value,
    map_value = []
}).
-endif.

-ifndef(DTFETCHREQ_PB_H).
-define(DTFETCHREQ_PB_H, true).
-record(dtfetchreq, {
    bucket = erlang:error({required, bucket}),
    key = erlang:error({required, key}),
    type = erlang:error({required, type}),
    r,
    pr,
    basic_quorum,
    notfound_ok,
    timeout,
    sloppy_quorum,
    n_val,
    include_context = true
}).
-endif.

-ifndef(DTVALUE_PB_H).
-define(DTVALUE_PB_H, true).
-record(dtvalue, {
    counter_value,
    set_value = [],
    map_value = []
}).
-endif.

-ifndef(DTFETCHRESP_PB_H).
-define(DTFETCHRESP_PB_H, true).
-record(dtfetchresp, {
    context,
    type = erlang:error({required, type}),
    value
}).
-endif.

-ifndef(COUNTEROP_PB_H).
-define(COUNTEROP_PB_H, true).
-record(counterop, {
    increment
}).
-endif.

-ifndef(SETOP_PB_H).
-define(SETOP_PB_H, true).
-record(setop, {
    adds = [],
    removes = []
}).
-endif.

-ifndef(MAPUPDATE_PB_H).
-define(MAPUPDATE_PB_H, true).
-record(mapupdate, {
    field = erlang:error({required, field}),
    counter_op,
    set_op,
    register_op,
    flag_op,
    map_op
}).
-endif.

-ifndef(MAPOP_PB_H).
-define(MAPOP_PB_H, true).
-record(mapop, {
    removes = [],
    updates = []
}).
-endif.

-ifndef(DTOP_PB_H).
-define(DTOP_PB_H, true).
-record(dtop, {
    counter_op,
    set_op,
    map_op
}).
-endif.

-ifndef(DTUPDATEREQ_PB_H).
-define(DTUPDATEREQ_PB_H, true).
-record(dtupdatereq, {
    bucket = erlang:error({required, bucket}),
    key,
    type = erlang:error({required, type}),
    context,
    op = erlang:error({required, op}),
    w,
    dw,
    pw,
    return_body = false,
    timeout,
    sloppy_quorum,
    n_val,
    include_context = true
}).
-endif.

-ifndef(DTUPDATERESP_PB_H).
-define(DTUPDATERESP_PB_H, true).
-record(dtupdateresp, {
    key,
    context,
    counter_value,
    set_value = [],
    map_value = []
}).
-endif.

