% -*- mode:erlang -*-
{application, 'iota.erlang', [
	{description, "Erlang implementation of IOTA"},
	{vsn, "0.1.0"},
	{mod, {iota_app, []}},
	{env, []},
	{modules, [iota, iota_app, iota_sup, iota_srv, iota_hash, trinary, ternary]},
	{applications, [kernel, stdlib]}
]}.
