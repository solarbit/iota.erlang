% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_sup).

-export([start_link/1]).

-behaviour(supervisor).
-export([init/1]).

-define(STRATEGY, #{strategy => one_for_one, intensity => 1, period => 5}).
-define(WORKER(M, A), #{id => M, start => {M, start_link, [A]}, restart => permanent, shutdown => 5000, type => worker}).


start_link(Services) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Services).


init(Services) ->
	ChildSpecs = [?WORKER(Module, StartArgs) || {Module, StartArgs} <- Services],
	{ok, {?STRATEGY, ChildSpecs}}.
