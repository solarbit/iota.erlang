% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_snapshot).

-include("iota.hrl").

-compile(export_all).

-define(SNAPSHOT_PUBLIC_KEY, <<"ETSYRXPKSCTJAZIJZDVJTQOILVEPHGV9PHPFLJVUFQRPXGNWPDBAKHCWPPEXPCZDIGPJDQGHVIQHQYQDW">>).
-define(SNAPSHOT_PUBLIC_KEY_DEPTH, 6).
-define(SNAPSHOT_INDEX, 0).

init() ->
	Bin = path:load("snapshot.txt"),
	Lines = text:split(Bin, <<"[\r\n]+">>),
	Trytes = iolist_to_binary([trinary:from_text(Line) || Line <- Lines]),
	Map = maps:from_list([list_to_tuple(text:split(Line, <<":">>)) || Line <- Lines]),
	SnapshotHash = iota_crypto:hash(kerl, Trytes),
	Bundle = iota_crypto:normalized_bundle(SnapshotHash).

	SignatureDigests = text:split(path:load("snapshot.sig"), <<"[\r\n]+">>),
