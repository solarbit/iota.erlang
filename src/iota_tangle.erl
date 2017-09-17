% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_tangle).

-include("iota.hrl").

-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-compile(export_all).


start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
	gen_server:cast(?MODULE, stop).


info() ->
	gen_server:call(?MODULE, info).



put(K, V) ->
	gen_server:call(?MODULE, {put, K, V}).


get(K) ->
	gen_server:call(?MODULE, {get, K}).


init(_Args) ->
	{ok, iota_db:connect()}.


handle_call(info, _From, Ref) ->
	Reply = iota_db:info(Ref),
	{reply, Reply, Ref};
handle_call({get, K}, _From, Ref) ->
	Reply = iota_db:get(Ref, K),
	{reply, Reply, Ref};
handle_call({put, K, V}, _From, Ref) ->
	Reply = iota_db:put(Ref, K, V),
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
	iota_db:release(Ref),
	ok.
