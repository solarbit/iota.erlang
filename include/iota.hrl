% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-define(APPLICATION, 'iota.erlang').

-define(TTY(X), io:format(user, "[~p:~p] ~p~n", [?MODULE, ?LINE, X])).

% Currently mirrors the IRI model
-record(address, {hashes = []}).
-record(approvee, {hashes = []}).
-record(bundle, {hashes = []}).
-record(milestone, {index, hash}).
-record(statediff, {map = #{}}).
-record(tag, {hashes = []}).

-define(TX_SIZE, 1604).
-record(tx, {address, bundle, trunk, branch, tag,
	value, current_index, last_index, timestamp,
	validity = 0, type = 1, arrival_time = 0,
	solid = false, height = 0, sender = "", snapshot
}).


% NOTE: Placeholders until a decision is made on the HTTP library to use
-record(http_request, {version, method, path, headers, params, content}).
-record(http_response, {version, status, message, headers, content}).
