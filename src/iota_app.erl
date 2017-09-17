% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_app).

-include("iota.hrl").

-behaviour(application).
-export([start/2, stop/1]).


start(normal, []) ->
	Env = application:get_all_env(),
    iota_sup:start_link([{iota_srv, Env}, {iota_data_srv, Env}]).


stop(_State) ->
    ok.
