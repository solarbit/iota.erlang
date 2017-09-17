% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_hash).
-include("iota.hrl").

-export([generate_seed/0, curl/1, curl/2, bcurlt/2, bcurlt/3, kerl/1, kerl/2]).

-define(TRYTE_HASHLENGTH, 81).

-define(TRIT_HASHLENGTH, (?TRYTE_HASHLENGTH * 3)).
-define(STATE_LENGTH, (?TRIT_HASHLENGTH * 3)).

-define(CURL_INITIAL_STATE, lists:duplicate(?STATE_LENGTH, 0)).
-define(CURL_ROUNDS, 27).
-define(CURL_TRUTH_TABLE, {1, 0, -1, 1, -1, 0, -1, 1, 0}).

-define(KERL_MASK, binary:copy(<<255>>, 48)).
-define(KECCAK_HASHSIZE, 384).


generate_seed() ->
	Bin = crypto:strong_rand_bytes(?TRYTE_HASHLENGTH),
	Bin0 = << <<(X rem 27)>> || <<X>> <= Bin>>,
	trinary:from_binary(Bin0).



curl(Trytes) ->
	curl(Trytes, 1).

curl(Trytes, SqueezeCount) ->
	State = curl_absorb(Trytes),
	curl_squeeze(State, SqueezeCount).


curl_absorb(Trytes) ->
	curl_absorb(Trytes, ?CURL_INITIAL_STATE).

curl_absorb(<<Trytes:?TRYTE_HASHLENGTH/binary, Bin/binary>>, State) ->
	Trits = lists:reverse(trinary:to_trits(Trytes)),
	{_, Rest} = lists:split(?TRIT_HASHLENGTH, State),
	State0 = curl_transform(Trits ++ Rest),
	curl_absorb(Bin, State0);
curl_absorb(<<>>, State) ->
	State.


curl_squeeze(State, Count) ->
	curl_squeeze(State, Count, <<>>).

curl_squeeze(State, Count, Acc) when Count > 0 ->
	{Hash, _} = lists:split(?TRIT_HASHLENGTH, State),
	Trytes = trinary:from_trits(lists:reverse(Hash)),
	State0 = curl_transform(State),
	curl_squeeze(State0, Count - 1, <<Acc/binary, Trytes/binary>>);
curl_squeeze(_, 0, Acc) ->
	Acc.


curl_transform(State) ->
	curl_transform(State, 1).

curl_transform(State, Round) when Round =< ?CURL_ROUNDS ->
	State0 = curl_transform(State, 1, 1, []),
	curl_transform(State0, Round + 1);
curl_transform(State, _) ->
	State.

curl_transform(State, Index, Count, Acc) when Count =< ?STATE_LENGTH ->
	Index0 = curl_next_index(Index),
	X = lists:nth(Index, State) + lists:nth(Index0, State) * 3 + 5,
	Value = element(X, ?CURL_TRUTH_TABLE),
	curl_transform(State, Index0, Count + 1, [Value|Acc]);
curl_transform(_, _, _, Acc) ->
	lists:reverse(Acc).


curl_next_index(X) when X =< 365 ->
	X + 364;
curl_next_index(X) ->
	X - 365.


bcurlt(HighTrytes, LowTrytes) ->
	bcurlt(HighTrytes, LowTrytes, 1).

bcurlt(HighTrytes, LowTrytes, SqueezeCount) ->
	{HighState, LowState} = bcurlt_absorb(HighTrytes, LowTrytes),
	bcurlt_squeeze(HighState, LowState, SqueezeCount).


bcurlt_absorb(High, Low) ->
	bcurlt_absorb(High, Low, ?CURL_INITIAL_STATE, ?CURL_INITIAL_STATE).

bcurlt_absorb(High, Low, HighState, LowState) ->
	% TODO: Implement
	{HighState, LowState}.


bcurlt_squeeze(HighState, LowState, Count) ->
	bcurlt_squeeze(HighState, LowState, Count, <<>>).

bcurlt_squeeze(High, Low, Count, Acc) when Count > 0 ->
	% TODO: Implement
	bcurlt_squeeze(High, Low, Count - 1, Acc);
bcurlt_squeeze(_, _, 0, Acc) ->
	Acc.


bcurlt_transform(HighState, LowState, Index, Count, Acc) when Count =< ?STATE_LENGTH ->
	% TODO: Implement
	bcurlt_transform(HighState, LowState, Index, Count + 1, Acc);
bcurlt_transform(_, _, _, _, Acc) ->
	lists:reverse(Acc).


kerl(Trytes) ->
	kerl(Trytes, 1).

kerl(Trytes, SqueezeCount) ->
	Hash = kerl_absorb(Trytes),
	kerl_squeeze(Hash, SqueezeCount).


kerl_absorb(Trytes) ->
	kerl_absorb(Trytes, sha3:hash_init(?KECCAK_HASHSIZE)).

kerl_absorb(<<Trytes:?TRYTE_HASHLENGTH/binary, Bin/binary>>, State) ->
	% NOTE: Seems odd to have to set the high trit to zero?
	[_|Trits] = trinary:to_trits(Trytes),
	X = ternary:to_integer([0|Trits]),
	State0 = sha3:hash_update(State, <<X:?KECCAK_HASHSIZE>>),
	kerl_absorb(Bin, State0);
kerl_absorb(<<>>, State) ->
	sha3:hash_final(State).


kerl_squeeze(Hash, Count) ->
	kerl_squeeze(Hash, Count, <<>>).

kerl_squeeze(Hash, Count, Acc) when Count > 0 ->
	<<X:?KECCAK_HASHSIZE/signed>> = Hash,
	Trytes = trinary:from_integer(X),
	Hash0 = sha3:hash(?KECCAK_HASHSIZE, crypto:exor(Hash, ?KERL_MASK)),
	kerl_squeeze(Hash0, Count - 1, <<Acc/binary, Trytes/binary>>);
kerl_squeeze(_, 0, Acc) ->
	Acc.
