% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(trinary).

-compile(export_all).

-define(CHARSET, <<"9ABCDEFGHIJKLMNOPQRSTUVWXYZ">>).


from_integer(X) ->
	Trits = ternary:from_integer(X),
	from_trits(Trits).


from_trits(Trits) ->
	Trits0 = pad(Trits),
	from_trits(Trits0, <<>>).

from_trits([T0, T1, T2|T], Acc) ->
	Index =
		case T0 * 9 + T1 * 3 + T2 of
		I when I < 0 ->
			27 - abs(I);
		I ->
			I
		end,
	X = binary:at(?CHARSET, Index),
	from_trits(T, <<X, Acc/binary>>);
from_trits([], Acc) ->
	Acc.


pad(Trits) ->
	case length(Trits) rem 3 of
	2 ->
		[0|Trits];
	1 ->
		[0, 0|Trits];
	0 ->
		Trits
	end.


from_trytes(Trytes) ->
	from_trytes(Trytes, <<>>).

from_trytes([H|T], Acc) when H >= 0 ->
	X = binary:at(?CHARSET, H),
	from_trytes(T, <<Acc/binary, X>>);
from_trytes([H|T], Acc) when H < 0 ->
	X = binary:at(?CHARSET, H + 27),
	from_trytes(T, <<Acc/binary, X>>);
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


tryte(X) ->
	binary:at(?CHARSET, X).


to_text(Trytes) ->
	to_text(Trytes, <<>>).

to_text(<<X0, X1, Bin/binary>>, Acc) ->
	X = value(X1) * 27 + value(X0),
	to_text(Bin, <<Acc/binary, X>>);
to_text(<<>>, Acc) ->
	Acc.


value($9) ->
	0;
value(Tryte) when Tryte >= $A andalso Tryte =< $Z ->
	Tryte - $A + 1.
