% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_replicator).

-include("iota.hrl").

-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(TRANSACTION_PACKET_SIZE, 1650).
-define(QUEUE_SIZE, 1000).
-define(RECV_QUEUE_SIZE, 1000).
-define(REPLY_QUEUE_SIZE, 1000).
-define(PAUSE_BETWEEN_TRANSACTIONS, 1).
-define(REQUEST_HASH_SIZE, 46).

-define(NUM_ACCEPTORS, 32). % NUM_THREADS in IRI
-define(CRC32_BYTES, 16).
-define(PORT_BYTES, 10).


start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
	gen_server:cast(?MODULE, stop).


init(_Args) ->
	% net_peer:start_link(#{transport => udp, port => ?}),
	State = #{},
	{ok, State}.


handle_call(Message, _From, State) ->
	{reply, {error, Message}, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Message, State) ->
    {noreply, State}.


handle_info(_Message, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


terminate(_Reason, _State) ->
	ok.
