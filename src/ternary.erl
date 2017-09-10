% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(ternary).

% @ref https://en.wikipedia.org/wiki/Balanced_ternary

-export([negate/1, add/2, subtract/2]).
-export([from_integer/1, to_integer/1, from_binary/1, to_binary/1]).

-define(BASE, 3).
-define(is_trit(X), (X >= -1 andalso X =< 1)).


negate(Trits) ->
	[-X || X <- Trits].


add(Trits, Trits0) ->
	add(lists:reverse(Trits), lists:reverse(Trits0), 0, []).

add([H|T], [H0|T0], C, Acc) ->
	[C0, X] = add3(H, H0, C),
	add(T, T0, C0, [X|Acc]);
add([], [H0|T0], C, Acc) ->
	[C0, X] = add3(0, H0, C),
	add([], T0, C0, [X|Acc]);
add([H|T], [], C, Acc) ->
	[C0, X] = add3(H, 0, C),
	add(T, [], C0, [X|Acc]);
add([], [], C, Acc) when C =/= 0 ->
	pad3([C|Acc]);
add([], [], 0, Acc) ->
	pad3(Acc).


add3(X, Y, Z) ->
	case X + Y + Z of
	-3 -> [-1, 0];
	-2 -> [-1, 1];
	-1 -> [0, -1];
	 0 -> [0, 0];
	 1 -> [0, 1];
	 2 -> [1, -1];
	 3 -> [1, 0]
	end.


subtract(Trits, Trits0) ->
	add(Trits, negate(Trits0)).


from_integer(0) ->
	[0, 0, 0];
from_integer(N) ->
	from_integer(N, 0, []).

from_integer(0, 0, Acc) ->
	pad3(Acc);
from_integer(0, Carry, Acc) ->
	pad3([Carry|Acc]);
from_integer(N, Carry, Acc) ->
	[C, X] = add3(N rem ?BASE, 0, Carry),
	from_integer(N div ?BASE, C, [X|Acc]).


to_integer(Trits) ->
	to_integer(Trits, 0).

to_integer([H|T], Acc) when ?is_trit(H) ->
	to_integer(T, Acc * ?BASE + H);
to_integer([], Acc) ->
	Acc.


from_binary(Bin) ->
	Trits = from_binary(Bin, []),
	Trits0 = lists:dropwhile(fun(X) -> X =:= 0 end, Trits),
	pad3(Trits0).

from_binary(<<0:2, Bin/bits>>, Acc) ->
	from_binary(Bin, [0|Acc]);
from_binary(<<1:2, Bin/bits>>, Acc) ->
	from_binary(Bin, [1|Acc]);
from_binary(<<2:2, Bin/bits>>, Acc) ->
	from_binary(Bin, [-1|Acc]);
from_binary(<<3:2, _/bits>>, _) ->
	error(invalid_encoding);
from_binary(<<>>, Acc) ->
	lists:reverse(Acc).


to_binary(Trits) ->
	Trits0 = lists:dropwhile(fun(X) -> X =:= 0 end, Trits),
	Trits1 = pad4(Trits0),
	to_binary(Trits1, <<>>).

to_binary([0|Trits], Acc) ->
	to_binary(Trits, <<Acc/bits, 0:2>>);
to_binary([1|Trits], Acc) ->
	to_binary(Trits, <<Acc/bits, 1:2>>);
to_binary([-1|Trits], Acc) ->
	to_binary(Trits, <<Acc/bits, 2:2>>);
to_binary([], Acc) ->
	Acc.


pad3(Trits) ->
	case length(Trits) rem 3 of
	0 -> Trits;
	1 -> [0, 0|Trits];
	2 -> [0|Trits]
	end.


pad4(Trits) ->
	case length(Trits) rem 4 of
	0 -> Trits;
	1 -> [0, 0, 0|Trits];
	2 -> [0, 0|Trits];
	3 -> [0|Trits]
	end.
