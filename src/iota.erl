% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota).

-include("iota.hrl").

-export([start/0, stop/0]).
-export([seed/0, curl/1, kerl/1]).


start() ->
	application:start(?APPLICATION).


stop() ->
	application:stop(?APPLICATION).


seed() ->
	iota_hash:generate_seed().


curl(Trytes) when byte_size(Trytes) > 0 andalso byte_size(Trytes) rem 81 == 0 ->
	iota_hash:curl(Trytes).


kerl(Trytes) when byte_size(Trytes) > 0 andalso byte_size(Trytes) rem 81 == 0 ->
	iota_hash:kerl(Trytes).
