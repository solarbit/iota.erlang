-module(iota_milestone).

-include("iota.hrl").

-export([validate/2]).

-define(MILESTONE_START_INDEX, 217000).
-define(MAX_MILESTONE_INDEX, 16#200000).

-define(KEYS_PER_MILESTONE, 20).

-define(RESCAN_INTERVAL, 5000).

-define(MAINNET, <<"mainnet">>).
-define(MAINNET_COORDINATOR,  <<"KPWCHICGJZXKE9GSUDXZYUAPLHAKAHYHDXNPHENTERYMMBQOPSQIDENXKLKCEYCPVTZQLEEJVYJZV9BWU">>).
-define(TESTNET, <<"testnet">>).
-define(TESTNET_COORDINATOR, <<"XNZBYAST9BETSDNOVQKKTBECYIPMF9IPOZRWUPFQGVH9HJW9NDSQVIPVBWU9YKECRYGDSJXYMZGHZDXCA">>).

-define(GROUP, 0). % transactions GROUP means that it's a non-leaf node (leaves store transaction value)
-define(PREFILLED_SLOT, 1). % means that we know only hash of the tx, the rest is unknown yet: only another tx references that hash
-define(FILLED_SLOT, -1). % knows the hash only coz another tx references that hash


validate(MilestoneIndex, TX = #tx{hash = TxHash, bundle = BundleHash})
		when MilestoneIndex >= 0 andalso MilestoneIndex < ?MAX_MILESTONE_INDEX ->
	case iota_tangle:get(milestone, MilestoneIndex) of
	{ok, #milestone{index = MilestoneIndex}} ->
		true; % NOTE: already validated
	undefined ->
		{ok, BundleTransactions} = iota_bundle:validate(BundleHash),
		FirstTxList = [X || [X|_] <- BundleTransactions],
		case validate_bundle_contains_tx(FirstTxList, TX) of
		true ->
			ok = iota_tangle:put(#milestone{index = MilestoneIndex, hash = TxHash}),
			true;
		false ->
			false
		end
	end;
validate(_ , _) ->
	false.


validate_bundle_contains_tx([#tx{hash = TxHash}|_], TX = #tx{hash = TxHash}) ->
	validate_trunk(TX);
validate_bundle_contains_tx([_|T], TX) ->
	validate_bundle_contains_tx(T, TX);
validate_bundle_contains_tx([], _) ->
	false.


validate_trunk(TX = #tx{bundle = BundleHash, trunk = TrunkHash, branch = BranchHash}) ->
	{ok, TrunkTx} = iota_tangle:get(tx, TrunkHash),
	case TrunkTx of
	#tx{type = ?FILLED_SLOT, bundle = BundleHash, trunk = BranchHash, signature = TrunkSignature} ->
		validate_merkle_root(TX, TrunkSignature);
	_ ->
		false
	end.


validate_merkle_root(TX = #tx{hash = TxHash}, TrunkSignature) ->
	{ok, RootHash} = iota_crypto:get_merkle_root(curlp27, TX, TrunkSignature), % Is this always curl?
	RootHash == ?MAINNET_COORDINATOR orelse ?TESTNET == iota:network().


updateLatestSolidSubtangleMilestone() ->
	Latest = iota_tangle:last(milestone),
	bored.
