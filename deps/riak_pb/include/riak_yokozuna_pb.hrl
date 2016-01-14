-ifndef(RPBYOKOZUNAINDEX_PB_H).
-define(RPBYOKOZUNAINDEX_PB_H, true).
-record(rpbyokozunaindex, {
    name = erlang:error({required, name}),
    schema,
    n_val
}).
-endif.

-ifndef(RPBYOKOZUNAINDEXGETREQ_PB_H).
-define(RPBYOKOZUNAINDEXGETREQ_PB_H, true).
-record(rpbyokozunaindexgetreq, {
    name
}).
-endif.

-ifndef(RPBYOKOZUNAINDEXGETRESP_PB_H).
-define(RPBYOKOZUNAINDEXGETRESP_PB_H, true).
-record(rpbyokozunaindexgetresp, {
    index = []
}).
-endif.

-ifndef(RPBYOKOZUNAINDEXPUTREQ_PB_H).
-define(RPBYOKOZUNAINDEXPUTREQ_PB_H, true).
-record(rpbyokozunaindexputreq, {
    index = erlang:error({required, index}),
    timeout
}).
-endif.

-ifndef(RPBYOKOZUNAINDEXDELETEREQ_PB_H).
-define(RPBYOKOZUNAINDEXDELETEREQ_PB_H, true).
-record(rpbyokozunaindexdeletereq, {
    name = erlang:error({required, name})
}).
-endif.

-ifndef(RPBYOKOZUNASCHEMA_PB_H).
-define(RPBYOKOZUNASCHEMA_PB_H, true).
-record(rpbyokozunaschema, {
    name = erlang:error({required, name}),
    content
}).
-endif.

-ifndef(RPBYOKOZUNASCHEMAPUTREQ_PB_H).
-define(RPBYOKOZUNASCHEMAPUTREQ_PB_H, true).
-record(rpbyokozunaschemaputreq, {
    schema = erlang:error({required, schema})
}).
-endif.

-ifndef(RPBYOKOZUNASCHEMAGETREQ_PB_H).
-define(RPBYOKOZUNASCHEMAGETREQ_PB_H, true).
-record(rpbyokozunaschemagetreq, {
    name = erlang:error({required, name})
}).
-endif.

-ifndef(RPBYOKOZUNASCHEMAGETRESP_PB_H).
-define(RPBYOKOZUNASCHEMAGETRESP_PB_H, true).
-record(rpbyokozunaschemagetresp, {
    schema = erlang:error({required, schema})
}).
-endif.

