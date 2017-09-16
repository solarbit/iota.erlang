% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota).
-include("iota.hrl").

-export([seed/0, curl/1, kerl/1, bch/1]).

-compile(export_all).


seed() ->
	iota_hash:generate_seed().


curl(Bin) when byte_size(Bin) > 0 andalso byte_size(Bin) rem 81 == 0 ->
	iota_hash:curl(Bin).


kerl(Bin) when byte_size(Bin) > 0 andalso byte_size(Bin) rem 81 == 0 ->
	iota_hash:kerl(Bin).


bch(N) ->
	Bin = crypto:strong_rand_bytes(N),
	<< <<(X rem 3):2>> || <<X:2>> <= Bin >>.



-define(TEST, 0).
-ifdef(TEST).

test() ->
	test_ternary(),
	test_trinary(),
	ok.


test_ternary() ->
	?TTY(ternary),
	<<X:256>> = crypto:strong_rand_bytes(32),
	<<Y:256>> = crypto:strong_rand_bytes(32),
	?TTY({X, Y}),
	XT = ternary:from_integer(X),
	YT = ternary:from_integer(Y),
	?TTY({XT, YT}),
	X0 = ternary:to_integer(XT),
	?TTY({X == X0, X0}),
	Y0 = ternary:to_integer(YT),
	?TTY({Y == Y0, Y0}),
	NT = ternary:negate(XT),
	?TTY({negate, NT}),
	N = ternary:to_integer(NT),
	?TTY({N == -X, N, -X}),
	AT = ternary:add(XT, YT),
	?TTY({add, AT}),
	A = ternary:to_integer(AT),
	?TTY({A == X + Y, A, X + Y}),
	ST = ternary:subtract(XT, YT),
	?TTY({subtract, ST}),
	S = ternary:to_integer(ST),
	?TTY({S == X - Y, S, X - Y}),
	XBCH = ternary:to_binary(XT),
	XT0 = ternary:from_binary(XBCH),
	?TTY({XT == XT0, XBCH}),
	YBCH = ternary:to_binary(YT),
	YT0 = ternary:from_binary(YBCH),
	?TTY({YT == YT0, YBCH}),
	ok.

test_trinary() ->
	?TTY(trinary),
	<<X:256>> = crypto:strong_rand_bytes(32),
	<<Y:256>> = crypto:strong_rand_bytes(32),
	?TTY({X, Y}),
	XT = trinary:from_integer(X),
	YT = trinary:from_integer(Y),
	?TTY({XT, YT}),
	X0 = trinary:to_integer(XT),
	?TTY({integer, X == X0, X0}),
	XB = trinary:to_binary(XT),
	XT0 = trinary:from_binary(XB),
	?TTY({binary, XT == XT0, XB}),
	M = <<"IOTA is a distributed ledger for the Internet of Things.\nhttp://iota.org">>,
	MT = trinary:from_text(M),
	M0 = trinary:to_text(MT),
	?TTY({text, M == M0, M0, MT}),
	MTT = trinary:to_trits(MT),
	MT0 = trinary:from_trits(MTT),
	?TTY({trits, MT0 == MT, MTT, MT0}),
	ok.

-endif.
