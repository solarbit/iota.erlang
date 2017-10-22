% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_tangle).

-include("iota.hrl").

-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([info/0, get/1, get/2, put/2, put/3, load/2, save/1, attach/4, cancel_attach/1, revalidate/0]).


start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
	gen_server:cast(?MODULE, stop).


info() ->
	gen_server:call(?MODULE, info).


put(Key, Value) ->
	put(default, Key, Value).

put(Bucket, Key, Value) ->
	gen_server:call(?MODULE, {put, Bucket, Key, Value}).


get(Key) ->
	get(default, Key).

get(Bucket, Key) ->
	gen_server:call(?MODULE, {get, Bucket, Key}).


load(tx, Hash) ->
	gen_server:call(?MODULE, {get, tx, Hash}).


save(Record) when ?is_record(Record) ->
	gen_server:call(?MODULE, {put, element(1, Record), Record}).


attach(Trunk, Branch, MinimumWeight, TryteList) ->
	gen_server:call(?MODULE, {attach, Trunk, Branch, MinimumWeight, TryteList}).


cancel_attach(Ref) ->
	gen_server:call(?MODULE, {cancel_attach, Ref}).


revalidate() ->
	gen_server:cast(?MODULE, revalidate).


init(_Args) ->
	{ok, DS} = iota_db:connect(),
	{ok, #{ds => DS, pearldiver => undefined}}.


handle_call(info, _From, State = #{ds := DS}) ->
	Reply = iota_db:info(DS),
	{reply, Reply, State};
handle_call({get, Bucket, Hash}, _From, State = #{ds := DS}) ->
	Reply = iota_db:get(DS, Bucket, Hash),
	{reply, Reply, State};
handle_call({put, Bucket, K, V}, _From, State = #{ds := DS}) ->
	Reply = iota_db:put(DS, Bucket, K, V),
	{reply, Reply, State};
handle_call({attach, Trunk, Branch, MinimumWeight, TryteList}, From, State = #{pearldiver := _Ref}) ->
	% TODO: spawn a pearl diver...
	Ref0 = erlang:make_ref(),
	{reply, {ok, Ref0}, State#{ref => Ref0}};
handle_call(Message, _From, State) ->
	{reply, {error, Message}, State}.


handle_cast(revalidate, State = #{ds := DS}) ->
	ok = ds:clear(milestone),
	ok = ds:clear(state_diff),
	ok = ds:clear(tx_metadata),
	{noreply, State};
handle_cast(cancel_attach, State = #{pearldiver := Ref}) ->
	% cancel pearl diver
	{noreply, State#{pearldiver => undefined}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Message, State) ->
    {noreply, State}.


handle_info(_Message, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


terminate(_Reason, #{ds := DS}) ->
	iota_db:release(DS).
