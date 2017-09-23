-module(iota_bundle).

-include("iota.hrl").

-compile(export_all).


validate(Hash) ->
	{ok, #bundle{hashes = Hashes}} = iota_tangle:load(bundle, Hash),
	TxList = load_transactions(Hashes),
	ok = verify_transaction_validity(TxList),
	ok = verify_bundle_value(TxList),
	more.


load_transactions(TxList) ->
	load_transactions(TxList, []).

load_transactions([H|T], Acc) ->
	{ok, TX} = iota_tangle:load(tx, H),
	case TX of
	#tx{current_index = 0, validity = Validity} when Validity >= 0 ->
		load_transactions(T, [TX|Acc]);
	_ ->
		throw({invalid_tx, TX}) % this or ignore?
	end;
load_transactions([], Acc) ->
	lists:reverse(Acc).


verify_transaction_validity([#tx{current_index = 0, validity = V}|T]) when V >= 0 ->
	verify_transaction_validity(T);
verify_transaction_validity([H|_]) ->
	{error, H};
verify_transaction_validity([]) ->
	ok.


verify_bundle_value(TxList) ->
	case lists:sum([X || #tx{value = X} <- TxList]) of
	BundleValue when BundleValue =< ?MONEY_SUPPLY, BundleValue > -(?MONEY_SUPPLY) ->
		ok;
	BundleValue ->
		{error, {bundle_value, BundleValue}}
	end.
