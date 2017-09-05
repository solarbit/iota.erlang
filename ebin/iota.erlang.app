% -*- mode:erlang -*-
{application, 'iota.erlang', [
	{description, "Erlang implementation of IOTA"},
	{vsn, "0.1.0"},
	{mod, {iota_app, []}},
	{env, []},
	{modules, [iota, iota_crypto, iota_srv, keccak, ternary]},
	{applications, [kernel, stdlib]}
]}.
