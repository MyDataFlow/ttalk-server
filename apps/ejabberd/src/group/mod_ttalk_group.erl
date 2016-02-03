-module(mod_ttalk_group).
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ttalk.hrl").

-export([start/2, stop/1]).
-export([user_send_packet/3]).

start(Host, Opts) ->
    ejabberd_hooks:add(user_send_packet, Host,
		       ?MODULE, user_send_packet, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host,
			  ?MODULE, user_send_packet, 50),
    ok.

user_send_packet(From,To,Packet)->
  ok.
