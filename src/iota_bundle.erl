% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_bundle).

-include("iota.hrl").

-compile(export_all).


validate(Hash) ->
	{ok, #bundle{hashes = Hashes}} = iota_tangle:load(bundle, Hash),
	TxList = load_transactions(Hashes),
	ok = verify_transaction_validity(TxList),
	ok = verify_bundle_value(TxList),
	more_to_do.


load_transactions(TxList) ->
	load_transactions(TxList, []).

load_transactions([H|T], Acc) ->
	{ok, TX} = iota_tangle:load(tx, H),
	{ok, Meta} = iota_tangle:load(tx_metadata, H),
	load_transactions(T, [{TX, Meta}|Acc]);
load_transactions([], Acc) ->
	lists:reverse(Acc).


verify_transaction_validity([{#tx{current_index = 0}, #tx_metadata{validity = V}}|T]) when V >= 0 ->
	verify_transaction_validity(T);
verify_transaction_validity([H|_]) ->
	{error, H};
verify_transaction_validity([]) ->
	ok.


verify_bundle_value(TxList) ->
	case lists:sum([X || {#tx{value = X}, _} <- TxList]) of
	BundleValue when BundleValue =< ?MONEY_SUPPLY, BundleValue > -(?MONEY_SUPPLY) ->
		ok;
	BundleValue ->
		{error, {bundle_value, BundleValue}}
	end.
