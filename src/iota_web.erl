% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_web).

-include("iota.hrl").

-export([request/1]).

-define(NO_PARAMS, #{command => binary}).
-define(URI_PARAMS, #{command => binary, uri => [binary]}).
-define(PARAMS_ATTACH, #{command => binary, trunkTransaction => binary,
		branchTransaction => binary, minWeightMagniture => integer, trytes => binary}).

request(#http_request{path = <<$/>>, content = Content}) ->
	Json = json:decode(Content),
	Command = maps:get(<<"command">>, Json, undefined),
	{Status, Json0} = response(Command, Json),
	#http_response{status = Status, content = json:encode(Json0)}.


response(<<"getNodeInfo">>, #{}) ->
	NodeInfoRecord = iota_node:info(),
	Reply = json:from_record(node_info, record_info(fields, node_info), NodeInfoRecord),
	{ok, Reply};

response(<<"getNeighbors">>, #{}) ->
	{ok, Peers} = iota_node:get_peers(),
	Neighbors = [#{address => uri:encode(URI)} || #node_info{uri = URI} <- Peers],
	{ok, Neighbors};

response(<<"addNeighbors">>, Map) ->
 	URIList = maps:get(<<"uris">>, Map),
	URIs = [uri:decode(URI) || URI <- URIList],
	{ok, Peers} = iota_node:add_peers(URIs),
	{ok, #{addedNeighbors => length(Peers)}};

response(<<"removeNeighbors">>, Map) ->
 	URIList = maps:get(<<"uris">>, Map),
	URIs = [uri:decode(URI) || URI <- URIList],
	{ok, Peers} = iota_node:remove_peers(URIs),
	{ok, #{removedNeighbors => length(Peers)}};

response(<<"attachToTangle">>, Map) ->
	Trunk = maps:get(<<"trunkTransaction">>, Map),
	Branch = maps:get(<<"branchTransaction">>, Map),
	MinimumWeight = maps:get(<<"minWeightMagnitude">>, Map),
	TryteList = maps:get(<<"trytes">>, Map),
	{ok, _Ref} = iota_tangle:attach(Trunk, Branch, MinimumWeight, TryteList),
	{ok, #{}};

response(<<"interruptAttachingToTangle">>, #{}) ->
	iota_tangle:cancel_attach(), % Ref?
	{ok, #{}};

response(<<"broadcastTransactions">>, Map) ->
	true = maps:has_key(<<"trytes">>, Map),
	{ok, #{}};

response(<<"findTransactions">>, Map) ->
	true = maps:has_key(<<"bundles">>, Map)
		orelse maps:has_key(<<"addresses">>, Map)
		orelse maps:has_key(<<"tags">>, Map)
		orelse maps:has_key(<<"approvees">>, Map),
	{ok, #{}};

response(<<"storeTransactions">>, Map) ->
	TxList = maps:get(<<"trytes">>, Map),
	Result = [iota_transaction:validate(Tx) || Tx <- TxList],
	{ok, #{}};

response(<<"getTransactionsToApprove">>, _Map) ->
	{ok, #{}};

response(<<"getMissingTransactions">>, _Map) ->
	{ok, #{}};

response(<<"getTips">>, _Map) ->
	{ok, #{}};

response(<<"getInclusionStates">>, Map) ->
	true = maps:has_key(<<"transactions">>, Map)
		andalso maps:has_key(<<"tips">>, Map),
	{ok, #{}};

response(<<"getBalances">>, Map) ->
	true = maps:has_key(<<"addresses">>, Map)
		andalso maps:has_key(<<"threshold">>, Map),
	{ok, #{}};

response(<<"getTrytes">>, Map) ->
	Hashes = maps:get(<<"hashes">>, Map),
	Transactions = [iota_tangle:load(transaction, Hash) || Hash <- Hashes],
	TryteList = [Trytes || #tx{address = Trytes} <- Transactions],
	{ok, #{trytes => TryteList}};

response(_, _) ->
	{bad_request, #{}}.
