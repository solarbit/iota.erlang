#! /bin/bash

if [ "$(dirname "$0")" != "." ]; then
	echo This script must be run from the iota.erlang/bin directory
	exit 1
fi

export ERL_LIBS=$ERL_LIBS:../..

./verify.sh

echo -n "==> Starting IOTA..."
erl -detached -sname iota -config iota -s iota

sleep 1
if [ -f erl_crash.dump ]; then
	echo "FAILED"
	echo "See erl_crash.dump"
else
	echo "OK"
fi
