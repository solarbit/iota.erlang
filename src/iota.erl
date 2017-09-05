% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota).

-export([seed/0]).


seed() ->
	iota_crypto:generate_seed().
