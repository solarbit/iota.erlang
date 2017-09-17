#!/usr/bin/env escript
%% -*- erlang -*-

main([]) ->
	File = "iota.config",
	case file:consult(File) of
	{ok, _} ->
		Result = ok;
	Error ->
		Result = Error
	end,
	io:fwrite("==> ~p ~p~n", [File, Result]).
