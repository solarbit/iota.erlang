% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_transaction).

-include("iota.hrl").

-compile(export_all).

-define(TX_SIZE, 1604). % ???
-define(FULL_TX_SIZE, 2673).

-define(HASH_TRYTE_SIZE, 81).
-define(MINIMUM_WEIGHT_MAGNITUDE, 81).
-define(GENESIS_TIMESTAMP, 1502226000). % 2017-08-08T21:00:00Z
-define(NULL_HASH, binary:copy(<<$9>>, ?HASH_TRYTE_SIZE)).


encode(#tx{signature = Signature, address = Address, value = Value, tag = Tag,
		timestamp = Timestamp, current_index = CurrentIndex, last_index = LastIndex,
		bundle = Bundle, trunk = Trunk, branch = Branch, nonce = Nonce}) ->
	ValueTrytes = trinary:from_integer(Value, 27),
	TimestampTrytes = trinary:from_integer(Timestamp, 9),
	CurrentIndexTrytes = trinary:from_integer(CurrentIndex, 9),
	LastIndexTrytes = trinary:from_integer(LastIndex, 9),
	<<Signature:2187/binary, Address:81/binary, ValueTrytes:27/binary, Tag:27/binary,
		TimestampTrytes:9/binary, CurrentIndexTrytes:9/binary, LastIndexTrytes:9/binary,
		Bundle:81/binary, Trunk:81/binary, Branch:81/binary, Nonce:81/binary>>.


decode(<<Signature:2187/binary, Address:81/binary, ValueTrytes:27/binary, Tag:27/binary,
		TimestampTrytes:9/binary, CurrentIndexTrytes:9/binary, LastIndexTrytes:9/binary,
		Bundle:81/binary, Trunk:81/binary, Branch:81/binary, Nonce:81/binary>>) ->
	% TODO: Is the whole binary the correct value to hash over? (returns a wrong result)
	Hash = ?NULL_HASH, % iota_crypto:hash(curl, Bin),
	Value = trinary:to_integer(ValueTrytes),
	Timestamp = trinary:to_integer(TimestampTrytes),
	CurrentIndex = trinary:to_integer(CurrentIndexTrytes),
	LastIndex = trinary:to_integer(LastIndexTrytes),
	% NOTE: Sanity check/guard
	true = Value =< ?MONEY_SUPPLY andalso Timestamp > ?GENESIS_TIMESTAMP,
	#tx {
		hash = Hash, signature = Signature, address = Address, value = Value, tag = Tag,
		timestamp = Timestamp, current_index = CurrentIndex, last_index = LastIndex,
		bundle = Bundle, trunk = Trunk, branch = Branch, nonce = Nonce
	}.


validate(Trytes) ->
	% Check: POW, more?
	ok.
