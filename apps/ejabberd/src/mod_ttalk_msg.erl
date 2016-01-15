-module(mod_ttalk_msg).
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([start/2, stop/1]).
start(Host, Opts) ->
    ejabberd_hooks:add(user_send_packet, Host,
		       ?MODULE, user_send_packet, 99),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host,
			  ?MODULE, user_send_packet, 99),
    ok.

user_send_packet(From,To,Packet)->
	ok.

%%<message 
%%  from='example.com'
%%  id='ktx72v49'
%%  to='juliet@example.com'
%%  type='ack'
%%  s:timestamp='20160112160432267'
%%  s:id='gid_ktx72v49'
%%  xml:lang='en'>
%%</message>
send_ack(From, To, Packet = #xmlel{name = <<"message">>,
	 attrs = Attrs},StoreID) ->
  Type = xml:get_attr_s(<<"type">>, Attrs),
  ID = xml:get_attr_s(<<"id">>,Attrs),
  case {Type,From#jid.luser} of
  	{_ , <<"">>}->
  		ok;
  	{<<"chat">>,_}->
  	    Server = #jid{user = <<"">>, server = From#jid.lserver,
  	     resource = <<"">>, luser = <<"">>, 
  	     lserver = From#jid.lserver, lresource = <<"">>},
  	    Timestamp = ttalk_time:millisecond(),
  		Ack = #xmlel{name = <<"message">>,
            attrs = [
            	{<<"from">>, jlib:jid_to_string(Server)},
            	{<<"id">>,ID},
           		{<<"to">>, jlib:jid_to_string(From)},
              	{<<"type">>, <<"ack">>},
              	{<<"s:timestamp">>,erlang:integer_to_binary(Timestamp)},
              	{<<"s:id">>,StoreID}
           	]},
        ejabberd_router:route(Server,From,Ack)
    end.


