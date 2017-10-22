% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_app).

-include("iota.hrl").

-behaviour(application).
-export([start/2, stop/1]).


start(normal, []) ->
	Env = application:get_all_env(),
	Config = maps:from_list(Env),
	?TTY(Config),
    iota_sup:start_link([
		{iota_srv, Config},
		{iota_tangle, Config},
		{iota_ledger, Config},
		{iota_node, maps:with([port], Config)}
	]).


stop(_State) ->
    ok.
