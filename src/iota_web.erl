% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_web).

-include("iota.hrl").

-export([request/1]).


request(#http_request{path = <<$/>>, content = Content}) ->
	Json = json:decode(Content),
	Command = maps:get(<<"command">>, Json, undefined),
	{Status, Json0} = response(Command, Json),
	#http_response{status = Status, content = json:encode(Json0)}.


response(<<"getNodeInfo">>, _Map) ->
	{ok, #{}};

response(<<"getNeighbors">>, _Map) ->
	{ok, #{}};

response(<<"addNeighbors">>, Map) ->
	true = maps:has_key(<<"uris">>, Map),
	{ok, #{}};

response(<<"removeNeighbors">>, Map) ->
	true = maps:has_key(<<"uris">>, Map),
	{ok, #{}};

response(<<"attachToTangle">>, Map) ->
	true = maps:has_key(<<"trunkTransaction">>, Map)
		andalso maps:has_key(<<"branchTransaction">>, Map)
		andalso maps:has_key(<<"minWeightMagnitude">>, Map)
		andalso maps:has_key(<<"trytes">>, Map),
	{ok, #{}};

response(<<"interruptAttachingToTangle">>, _Map) ->
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
	true = maps:has_key(<<"trytes">>, Map),
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
	true = maps:has_key(<<"hashes">>, Map),
	{ok, #{}};

response(_, _) ->
	{bad_request, #{}}.
