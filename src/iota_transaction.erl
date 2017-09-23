% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_transaction).
-include("iota.hrl").
-compile(export_all).

-define(ZERO16, "9999999999999999").

-record(iota_transaction, {
	hash, signature, address, value, tag,
	timestamp, current_index, last_index,
	bundle, trunk, branch, nonce
}).


encode(TX = #iota_transaction{}) ->
	ok.


decode(TX) when byte_size(TX) =:= 2673 ->
	Hash = iota_hash:kerl(TX), % INCORRECT!
	<<Signature:2187/binary, Address:81/binary, Value:11/binary, ?ZERO16,
		Tag:27/binary, Timestamp:9/binary, CurrentIndex:9/binary, LastIndex:9/binary,
		Bundle:81/binary, Trunk:81/binary, Branch:81/binary, Nonce:81/binary>> = TX,
	#iota_transaction {
		hash = Hash,
		signature = Signature,
		address = Address,
		value = trinary:to_integer(Value),
		tag = Tag,
		timestamp = trinary:to_integer(Timestamp),
		current_index = trinary:to_integer(CurrentIndex),
		last_index = trinary:to_integer(LastIndex),
		bundle = Bundle,
		trunk = Trunk,
		branch = Branch,
		nonce = Nonce
	}.

validate(Trytes) ->
	% Check: POW, more?
	ok.
