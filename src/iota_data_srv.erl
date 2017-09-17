% Copyright (c) 2017 Mastercard. All rights reserved.

-module(iota_data_srv).

-include("iota.hrl").

-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-compile(export_all).


start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
	gen_server:cast(?MODULE, stop).


status() ->
	{ok, Status} = gen_server:call(?MODULE, status),
	io:format(Status, []).


put(K, V) ->
	gen_server:call(?MODULE, {put, K, V}).


get(K) ->
	gen_server:call(?MODULE, {get, K}).


init(_Args) ->
	Path = code:priv_dir(?APPLICATION) ++ "/iota.db",
	{ok, Ref} = erocksdb:open(Path, [{create_if_missing, true}], []),
	{ok, Ref}.


handle_call(status, _From, Ref) ->
	Reply = erocksdb:status(Ref),
	{reply, Reply, Ref};
handle_call({get, K}, _From, Ref) ->
	Reply = erocksdb:get(Ref, K, []),
	{reply, Reply, Ref};
handle_call({put, K, V}, _From, Ref) ->
	Reply = erocksdb:put(Ref, K, V, []),
	{reply, Reply, Ref};
handle_call(Message, _From, Ref) ->
	{reply, {error, Message}, Ref}.


handle_cast(stop, Ref) ->
    {stop, normal, Ref};
handle_cast(_Message, Ref) ->
    {noreply, Ref}.


handle_info(_Message, Ref) ->
    {noreply, Ref}.


code_change(_OldVsn, Ref, _Extra) ->
	{ok, Ref}.


terminate(_Reason, Ref) ->
	erocksdb:close(Ref),
	ok.
