% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_app).

-behaviour(application).
-export([start/2, stop/1]).


start(normal, []) ->
    iota_sup:start_link([{iota_srv, []}, {iota_data_srv, []}]).


stop(_State) ->
    ok.
