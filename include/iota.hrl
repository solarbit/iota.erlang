% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-define(APPLICATION, 'iota.erlang').

-define(TTY(X), io:format(user, "[~p:~p] ~p~n", [?MODULE, ?LINE, X])).
