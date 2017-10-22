% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_db).

-include("iota.hrl").

-export([connect/0, release/1, info/1, get/2, get/3, put/3, put/4, delete/2, delete/3]).

-define(BUCKETS, [default, tx, tx_metadata, milestone, state_diff, address, approvee, bundle, tag]).

-define(MEGABYTE, 1024 * 1024).

% NOTE: For now, just mirror IRI RocksDBPersistenceProvider option settings

-define(DEFAULT_DB_OPTIONS, [
	{create_if_missing, true},
	{create_missing_column_families, true},
	{db_log_dir, code:priv_dir(?APPLICATION) ++ "/iota.log"},
	{max_log_file_size, ?MEGABYTE},
	{max_manifest_file_size, ?MEGABYTE},
	{max_open_files, 10000},
	{max_background_jobs, erlang:system_info(logical_processors_available)}
	% {bytes_per_sync, 4 * ?MEGABYTE},
	% {max_total_wal_size, 16 * ?MEGABYTE}
]).

-define(DEFAULT_CF_OPTIONS, [
	{write_buffer_size, 2 * ?MEGABYTE},
	{max_write_buffer_number, 2}
	% {compaction_style, universal},
	% {compression, snappy}
]).

-define(DEFAULT_READ_OPTIONS, []).
-define(DEFAULT_WRITE_OPTIONS, []).


connect() ->
	Path = code:priv_dir(?APPLICATION) ++ "/iota.db",
	CFOptions = [{atom_to_list(Bucket), ?DEFAULT_CF_OPTIONS} || Bucket <- ?BUCKETS],
	{ok, Ref, CF} = rocksdb:open_with_cf(Path, ?DEFAULT_DB_OPTIONS, CFOptions),
	Buckets = maps:from_list(lists:zip(?BUCKETS, CF)),
	{ok, #{db => Ref, buckets => Buckets}}.


release(#{db := DB}) ->
	rocksdb:close(DB).


info(#{db := DB, buckets := Buckets}) ->
	Info = [{Bucket, rocksdb:count(DB, maps:get(Bucket, Buckets))} || Bucket <- ?BUCKETS],
	{ok, Info}.


get(DS, Key) ->
	get(DS, default, Key).

get(#{db := DB, buckets := Buckets}, Bucket, Key) ->
	Ref = maps:get(Bucket, Buckets),
	rocksdb:get(DB, Ref, Key, ?DEFAULT_READ_OPTIONS).


put(DS, Key, Value) ->
	put(DS, default, Key, Value).

put(#{db := DB, buckets := Buckets}, Bucket, Key, Value) ->
	Ref = maps:get(Bucket, Buckets),
	rocksdb:put(DB, Ref, Key, Value, ?DEFAULT_WRITE_OPTIONS).


delete(DS, Key) ->
	delete(DS, default, Key).

delete(#{db := DB, buckets := Buckets}, Bucket, Key) ->
	Ref = maps:get(Bucket, Buckets),
	rocksdb:delete(DB, Ref, Key, ?DEFAULT_WRITE_OPTIONS).
