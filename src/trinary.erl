% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(trinary).

-export([from_integer/1, to_integer/1, from_trits/1, to_trits/1,
	from_binary/1, to_binary/1, from_text/1, to_text/1]).

-define(CHARSET, <<"9ABCDEFGHIJKLMNOPQRSTUVWXYZ">>).


from_integer(X) ->
	Trits = ternary:from_integer(X),
	from_trits(Trits).


to_integer(Trytes) ->
	Trits = to_trits(Trytes),
	ternary:to_integer(Trits).


from_trits(Trits) ->
	from_trits(Trits, <<>>).

from_trits([T0, T1, T2|T], Acc) ->
	X = char(T0 * 9 + T1 * 3 + T2),
	from_trits(T, <<X, Acc/binary>>);
from_trits([], Acc) ->
	Acc.


to_trits(Trytes) ->
	to_trits(Trytes, []).

to_trits(<<X, Trytes/binary>>, Acc) ->
	X0 = value(X),
	[T0, T1, T2] = ternary:from_integer(X0),
	to_trits(Trytes, [T0, T1, T2|Acc]);
to_trits(<<>>, Acc) ->
	Acc.


from_binary(Bin) ->
	from_binary(Bin, <<>>).

from_binary(<<0:3, X:5, Bin/binary>>, Acc) ->
	Tryte = binary:at(?CHARSET, X),
	from_binary(Bin, <<Acc/binary, Tryte>>);
from_binary(<<_, _/binary>>, _) ->
	error(invalid_encoding);
from_binary(<<>>, Acc) ->
	Acc.


to_binary(Trytes) ->
	to_binary(Trytes, <<>>).

to_binary(<<X, Trytes/binary>>, Acc) when X =:= $9 ->
	to_binary(Trytes, <<Acc/binary, 0>>);
to_binary(<<X, Trytes/binary>>, Acc) when X >= $A andalso X =< $Z ->
	to_binary(Trytes, <<Acc/binary, (1 + X - $A)>>);
to_binary(<<>>, Acc) ->
	Acc.


from_text(Bin) ->
	from_text(Bin, <<>>).

from_text(<<X, Bin/binary>>, Acc) ->
	X0 = char(X rem 27),
	X1 = char(X div 27),
	from_text(Bin, <<Acc/binary, X0, X1>>);
from_text(<<>>, Acc) ->
	Acc.


to_text(Trytes) ->
	to_text(Trytes, <<>>).

to_text(<<X0, X1, Bin/binary>>, Acc) ->
	X = unbalanced(X1) * 27 + unbalanced(X0),
	to_text(Bin, <<Acc/binary, X>>);
to_text(<<>>, Acc) ->
	Acc.


char(X) when X < 0 ->
	char(X + 27);
char(X) ->
	binary:at(?CHARSET, X).


value($9) ->
	0;
value(Tryte) when Tryte >= $A andalso Tryte =< $M ->
	Tryte - $A + 1;
value(Tryte) when Tryte >= $N andalso Tryte =< $Z ->
	Tryte - $Z - 1.


unbalanced($9) ->
	0;
unbalanced(Tryte) when Tryte >= $A andalso Tryte =< $Z ->
	Tryte - $A + 1.
