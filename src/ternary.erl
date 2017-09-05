% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(ternary).

% @ref https://en.wikipedia.org/wiki/Balanced_ternary

-export([from_integer/1, to_integer/1, negate/1, add/2, subtract/2]).

-define(BASE, 3).
-define(is_trit(X), (X >= -1 andalso X =< 1)).


from_integer(N) ->
	from_integer(N, 0, []).

from_integer(0, 0, Acc) ->
	Acc;
from_integer(0, Carry, Acc) ->
	[Carry|Acc];
from_integer(N, Carry, Acc) ->
	[C, X] = add3(N rem ?BASE, 0, Carry),
	from_integer(N div ?BASE, C, [X|Acc]).


to_integer(Trits) ->
	to_integer(Trits, 0).

to_integer([H|T], Acc) when ?is_trit(H) ->
	to_integer(T, Acc * ?BASE + H);
to_integer([], Acc) ->
	Acc.


negate(Trits) ->
	[X * -1 || X <- Trits].


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
	[C|Acc];
add([], [], 0, Acc) ->
	Acc.


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
