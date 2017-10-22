% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_node).


-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(IOTA_DEFAULT_PORT, 14600).

-export([info/0, get_peers/0, add_peers/1, remove_peers/1, broadcast/1]).


start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
	gen_server:cast(?MODULE, stop).


info() ->
	gen_server:call(?MODULE, info).


get_peers() ->
	gen_server:call(?MODULE, get_peers).


add_peers(URIList) ->
	gen_server:call(?MODULE, {add_peers, URIList}).


remove_peers(URIList) ->
	gen_server:call(?MODULE, {remove_peers, URIList}).


broadcast(Tx) when is_binary(Tx) ->
	gen_server:call(?MODULE, {broadcast, Tx}).



init(Args) ->
	Port = maps:get(port, Args, ?IOTA_DEFAULT_PORT),
	{ok, Pid} = net_peer:start_link(#{port => Port, protocol => iota_protocol}),
	LRUHashCache = maps:new(),
	LRUByteCache = maps:new(),
	State = #{net_peer => Pid, hash_cache => LRUHashCache, byte_cache => LRUByteCache},
	{ok, State}.


handle_call(get_peers, _From, State = #{net_peer := Pid}) ->
	{ok, Nodes} = net_peer:connections(Pid),
	{reply, {ok, Nodes}, State};
handle_call({add_peers, URIList}, _From, State = #{net_peer := Pid}) ->
	ok = net_peer:connect(Pid, [uri:decode(URI) || URI <- URIList]),
	{reply, {ok, length(URIList)}, State};
handle_call({remove_peers, URIList}, _From, State = #{net_peer := Pid}) ->
	ok = net_peer:disconnect(Pid, [uri:decode(URI) || URI <- URIList]),
	{reply, {ok, length(URIList)}, State};
handle_call({broadcast, TxTrytes}, _From, State = #{net_peer := Pid}) ->
	Reply =
		case iota_transaction:validate(TxTrytes) of % TODO: Should do this before here
		ok ->
			ok = net_peer:broadcast(Pid, TxTrytes);
		Error ->
			Error
		end,
	{reply, Reply, State};
handle_call(Message, _From, State) ->
	{reply, {error, Message}, State}.


handle_cast(stop, State = #{net_peer := Pid}) ->
	ok = net_peer:stop(Pid),
    {stop, normal, State};
handle_cast(_Message, State) ->
    {noreply, State}.


handle_info(_Message, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


terminate(_Reason, _State) ->
	ok.
