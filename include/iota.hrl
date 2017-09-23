% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-define(APPLICATION, 'iota.erlang').

-define(TTY(X), io:format(user, "[~p:~p] ~p~n", [?MODULE, ?LINE, X])).

% Currently mirrors the IRI model
-record(address, {hashes = []}).
-record(approvee, {hashes = []}).
-record(bundle, {hashes = []}).
-record(milestone, {index, hash}).
-record(state_diff, {map = #{}}).
-record(tag, {hashes = []}).

-define(TX_SIZE, 1604).
-record(tx, {address, bundle, trunk, branch, tag,
	value, current_index, last_index, timestamp,
	validity = 0, type = 1, arrival_time = 0,
	solid = false, height = 0, sender = "", snapshot
}).

-define(MONEY_SUPPLY, 2779530283277761). % (3^33 - 1) / 2

% NOTE: Placeholders until a decision is made on the HTTP library to use
-record(http_request, {version, method, path, headers, params, content}).
-record(http_response, {version, status, message, headers, content}).



% hmm
-record(neighbor, {uri}).

% From GetNodeInfoResponse - very java oriented
-record(node_info, {
	uri,
	app_name, % string
	app_version, % string
	jre_available_processors,
	jre_version, % string
	jre_free_memory, % long
	jre_max_memory, % long
	jre_total_memory, % long
	latest_milestone, % string
	latest_milestone_index, % int
	latest_solid_subtangle_milestone, % string
	latest_solid_subtangle_milestone_index, % int
	neighbors, % int
	packets_queue_size, % int
	time, % long
	tips, % int
	transactions_to_request % int
}).
