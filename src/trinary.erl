% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(trinary).

-export([from_integer/1, from_trits/1, from_trytes/1, from_text/1, to_text/1]).

-define(CHARSET, <<"9ABCDEFGHIJKLMNOPQRSTUVWXYZ">>).


from_integer(X) ->
	Trits = ternary:from_integer(X),
	from_trits(Trits).


from_trits(Trits) ->
	Trits0 = pad(Trits),
	from_trits(Trits0, <<>>).

from_trits([T0, T1, T2|T], Acc) ->
	X = tryte(T0 * 9 + T1 * 3 + T2),
	from_trits(T, <<X, Acc/binary>>);
from_trits([], Acc) ->
	Acc.


from_trytes(Trytes) ->
	from_trytes(Trytes, <<>>).

from_trytes([H|T], Acc) ->
	X = tryte(H),
	from_trytes(T, <<X, Acc/binary>>);
from_trytes([], Acc) ->
	Acc.


from_text(Bin) ->
	from_text(Bin, <<>>).

from_text(<<X, Bin/binary>>, Acc) ->
	X0 = tryte(X rem 27),
	X1 = tryte(X div 27),
	from_text(Bin, <<Acc/binary, X0, X1>>);
from_text(<<>>, Acc) ->
	Acc.


to_text(Trytes) ->
	to_text(Trytes, <<>>).

to_text(<<X0, X1, Bin/binary>>, Acc) ->
	X = value(X1) * 27 + value(X0),
	to_text(Bin, <<Acc/binary, X>>);
to_text(<<>>, Acc) ->
	Acc.


tryte(X) when X < 0 ->
	tryte(X + 27);
tryte(X) ->
	binary:at(?CHARSET, X).


value($9) ->
	0;
value(Tryte) when Tryte >= $A andalso Tryte =< $Z ->
	Tryte - $A + 1.


pad(Trits) ->
	pad(length(Trits) rem 3, Trits).

pad(0, Trits) ->
	Trits;
pad(1, Trits) ->
	[0, 0|Trits];
pad(2, Trits) ->
	[0|Trits].
