% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_web).

-include("iota.hrl").

-export([request/1]).


request(#http_request{path = <<$/, Path/binary>>, content = Content}) ->
	Json = json:decode(Content),
	{Status, Json0} = request(Path, Json),
	#http_response{status = Status, content = json:encode(Json0)}.


request(<<"addNeighbors">>, Map) ->
	true = maps:has_key(<<"uris">>, Map),
	{ok, #{}};

request(<<"removeNeighbors">>, Map) ->
	true = maps:has_key(<<"uris">>, Map),
	{ok, #{}};


request(<<"attachToTangle">>, Map) ->
	true = maps:has_key(<<"trunkTransaction">>, Map)
		andalso maps:has_key(<<"branchTransaction">>, Map)
		andalso maps:has_key(<<"minWeightMagnitude">>, Map)
		andalso maps:has_key(<<"trytes">>, Map),
	{ok, #{}};

request(<<"broadcastTransactions">>, Map) ->
	true = maps:has_key(<<"trytes">>, Map),
	{ok, #{}};

request(<<"findTransactions">>, Map) ->
	true = maps:has_key(<<"bundles">>, Map)
		orelse maps:has_key(<<"addresses">>, Map)
		orelse maps:has_key(<<"tags">>, Map)
		orelse maps:has_key(<<"approvees">>, Map),
	{ok, #{}};

request(<<"getBalances">>, Map) ->
	true = maps:has_key(<<"addresses">>, Map)
		andalso maps:has_key(<<"threshold">>, Map),
	{ok, #{}};

request(<<"getInclusionStates">>, Map) ->
	true = maps:has_key(<<"transactions">>, Map)
		andalso maps:has_key(<<"tips">>, Map),
	{ok, #{}};

request(<<"getNeighbors">>, _Map) ->
	{ok, #{}};

request(<<"getNodeInfo">>, _Map) ->
	{ok, #{}};

request(<<"getTips">>, _Map) ->
	{ok, #{}};

request(<<"getTransactionsToApprove">>, _Map) ->
	{ok, #{}};

request(<<"getTrytes">>, Map) ->
	true = maps:has_key(<<"hashes">>, Map),
	{ok, #{}};

request(<<"interruptAttachingToTangle">>, _Map) ->
	{ok, #{}};

request(<<"storeTransactions">>, Map) ->
	true = maps:has_key(<<"trytes">>, Map),
	{ok, #{}};

request(<<"getMissingTransactions">>, _Map) ->
	{ok, #{}};

request(_, _) ->
	{bad_request, #{}}.
