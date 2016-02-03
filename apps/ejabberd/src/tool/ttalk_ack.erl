-module(ttalk_ack).
send_ack(From,ID,StoreID) ->
  Server = #jid{user = <<"">>, server = From#jid.lserver,
  resource = <<"">>, luser = <<"">>, lserver = From#jid.lserver, lresource = <<"">>},
  Timestamp = ttalk_time:millisecond(),
  Ack = #xmlel{
          name = <<"message">>,
          attrs = [
              {<<"xmlns:s">>, ?NS_TTALK_SERVER},
              {<<"from">>, jlib:jid_to_binary(Server)},
              {<<"id">>,ID},
              {<<"to">>, jlib:jid_to_binary(From)},
              {<<"type">>, <<"ack">>},
              {<<"s:timestamp">>,erlang:integer_to_binary(Timestamp)},
              {<<"s:id">>,erlang:integer_to_binary(StoreID)}
            ]},
	ejabberd_router:route(Server,From,Ack).