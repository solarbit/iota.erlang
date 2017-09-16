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


curl(Bin) when byte_size(Bin) > 0 andalso byte_size(Bin) rem 81 == 0 ->
	iota_hash:curl(Bin).


kerl(Bin) when byte_size(Bin) > 0 andalso byte_size(Bin) rem 81 == 0 ->
	iota_hash:kerl(Bin).
