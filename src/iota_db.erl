% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_db).

-include("iota.hrl").

-export([connect/0, release/1, info/1, get/2, put/3, add_bucket/2, remove_bucket/2]).


connect() ->
	Path = code:priv_dir(?APPLICATION) ++ "/iota.db",
	erocksdb:open(Path, [{create_if_missing, true}], []).


release(Connection) ->
	erocksdb:close(Connection).


info(Connection) ->
	{ok, Status} = erocksdb:status(Connection),
	io:format(Status, []).


get(Connection, Key) ->
	erocksdb:get(Connection, Key, []).


put(Connection, Key, Value) ->
	erocksdb:put(Connection, Key, Value, []).


% TODO: Use RocksDB Column Families
add_bucket(_Connection, _Bucket) ->
	not_implemented.


% TODO: Use RocksDB Column Families
remove_bucket(_Connection, _Bucket) ->
	not_implemented.
