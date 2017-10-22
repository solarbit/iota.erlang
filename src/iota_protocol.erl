% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_protocol).

-include("iota.hrl").

-export([codec/0, init/2, terminate/2, on_send/2]).

-export([connected/2]).

codec() ->
	{codec, fun encode/1, fun decode/1}.


encode(Message) ->
	{ok, Message, fun encode/1}.


decode(Bin) ->
	Messages = text:split(Bin, <<" ">>),
	{ok, Messages, fun decode/1}.


init(_Opts, _Hosts) ->
	State = #{},
	{noreply, connected, State}.


connected(Message, State) ->
	?TTY({received, Message}),
	{noreply, connected, State}.


terminate(_Reason, _State) ->
	ok.


on_send(Message, State) ->
	?TTY({sending, Message}),
	{ok, State}.
