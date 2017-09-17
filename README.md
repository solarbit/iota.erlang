# IOTA Tangle for Erlang/OTP

> WARNING: This is a work in progress

## Dependencies

Keccak/SHA3:  https://github.com/szktty/erlang-sha3.git

RocksDB:  https://github.com/leo-project/erocksdb.git

## Issues

1. RocksDB expects the LZO (`lzo2`) library to be installed on your machine.
1. RocksDB uses `CXXFLAGS` as well as `CPPFLAGS`. Set these correctly before building.

e.g. If using MacPorts on OSX
```bash
$ sudo port install lzo2
$ cd erocksdb
$ export CXXFLAGS="-I /opt/local/include"
$ export CPPFLAGS="-I /usr/local/include $CXXFLAGS"
$ make
```
