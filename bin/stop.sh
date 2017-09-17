#!/usr/bin/env escript
%% -*- erlang -*-

main([]) ->
	io:fwrite("==> Stopping IOTA..."),
    {ok, _} = net_kernel:start([iota_ctrl, shortnames]),
	[_|T] = re:split(atom_to_binary(node(), utf8), <<"(@)">>),
	Node = binary_to_atom(iolist_to_binary(["iota"|T]), utf8),
	rpc:call(Node, iota, stop, []),
    Res = rpc:call(Node, init, stop, []),
	case Res of
	ok ->
    	io:fwrite("OK~n");
	_ ->
		io:fwrite("ERROR:~p~n", [Res])
	end.
