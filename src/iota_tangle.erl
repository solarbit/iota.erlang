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


load(transaction, Hash) ->
	gen_server:call(?MODULE, {load_transaction, Hash}).


attach(Trunk, Branch, MinimumWeight, TryteList) ->
	gen_server:call(?MODULE, {attach, Trunk, Branch, MinimumWeight, TryteList}).


cancel_attach(Ref) ->
	gen_server:call(?MODULE, {cancel_attach, Ref}).


init(_Args) ->
	{ok, DS} = iota_db:connect(),
	{ok, #{ds => DS, pearldiver => undefined}}.


handle_call(info, _From, State = #{ds := DS}) ->
	Reply = iota_db:info(DS),
	{reply, Reply, State};
handle_call({get, K}, _From, State = #{ds := DS}) ->
	Reply = iota_db:get(DS, K),
	{reply, Reply, State};
handle_call({put, K, V}, _From, State = #{ds := DS}) ->
	Reply = iota_db:put(DS, K, V),
	{reply, Reply, State};
handle_call({attach, Trunk, Branch, MinimumWeight, TryteList}, From, State = #{pearldiver := _Ref}) ->
	Ref0 = erlang:make_ref(), % TODO: spawn a pearl diver
	{reply, {ok, Ref0}, State#{ref => Ref0}};
handle_call(Message, _From, State) ->
	{reply, {error, Message}, State}.

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
