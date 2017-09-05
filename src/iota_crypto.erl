% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_crypto).

-export([generate_seed/0]).

-define(SEED_LENGTH, 81).


generate_seed() ->
	Bin = crypto:strong_rand_bytes(?SEED_LENGTH),
	Trytes = generate_seed(Bin, []),
	trinary:from_trytes(Trytes).

generate_seed(<<X, Bin/binary>>, Acc) ->
	generate_seed(Bin, [X rem 27 - 13|Acc]);
generate_seed(<<>>, Acc) ->
	lists:reverse(Acc).
