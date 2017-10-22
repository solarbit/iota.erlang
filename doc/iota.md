# IOTA Reference

> This is my attempt at documenting what is actually going on in IRI without the implementation detail distractions resulting from the use of an imperative language (i.e. Java).

## The Token

IOTA has a fixed __money supply__ equal to (3<sup>33</sup> - 1)/2, or 2,779,530,283,277,761 iota tokens. [why?]

An iota token is formally denoted by the italicized greek character iota: <em>&iota;</em>

Less formally it may be represented by the latin character: i

Expression of large amounts of the token uses SI notation.

|Notation|SI|Value|Integer|Verbal|
|-|-|-|-|
|Ki|Kilo|10<sup>3</sup>|1,000|Thousand|
|Mi|Mega|10<sup>6</sup>|1,000,000|Million|
|Gi|Giga|10<sup>9</sup>|1,000,000,000|Billion|
|Ti|Tera|10<sup>12</sup>|1,000,000,000,000|Trillion|
|Pi|Peta|10<sup>15</sup>|1,000,000,000,000,000|Thousand Trillion|

i.e.: 1 Ki is exactly 1000i, and not 1024i, etc.

## Trits and Trytes

Internally, iota uses ternary representation (cf. binary) for many values.

A trinary digit, or __trit__, has the integer value 1, 0, or -1. [cf. +5V, 0V, -5V]

Like decimal, trit-encoded numbers have positional notation, but using base 3 not base 10, for example:

[1, 0, 1]<sub>3</sub> => (1 &times; 3<sup>2</sup>) + (0 &times; 3<sup>1</sup>) + (1 &times; 3<sup>0</sup>) => 10<sub>10</sub>

[-1, 0, -1]<sub>3</sub> => (-1 &times; 3<sup>2</sup>) + (0 &times; 3<sup>1</sup>) + (-1 &times; 3<sup>0</sup>) => -10<sub>10</sub>

An ordered 3-tuple of trits is called a __tryte__.

So a single tryte can represent 27 integer values between -13 and 13 inclusive.


In balanced ternary these values are:

|DECIMAL|TRITS|TRYTE|DECIMAL|TRITS|TRYTE|
|-|-|-|-|-|-|
|0|0, 0, 0|9||||
|1|0, 0, 1|A|-1|0, 0, -1|Z|
|2|0, 1, -1|B|-2|0, -1, 1|Y|
|3|0, 1, 0|C|-3|0, -1, 0|X|
|4|0, 1, 1|D|-4|0, -1, -1|W|
|5|1, -1, -1|E|-5|-1, 1, 1|V|
|6|1, -1, 0|F|-6|-1, 1, 0|U|
|7|1, -1, 1|G|-7|-1, 1, -1|T|
|8|1, 0, -1|H|-8|-1, 0, 1|S|
|9|1, 0, 0|I|-9|-1, 0, 0|R|
|10|1, 0, 1|J|-10|-1, 0, -1|Q|
|11|1, 1, -1|K|-11|-1, -1, 1|P|
|12|1, 1, 0|L|-12|-1, -1, 0|O|
|13|1, 1, 1|M|-13|-1, -1, -1|N|







## Transactions

A __bundle__ is a collection of __transactions__.

## Hashes
1. Curl/BCurlT
1. Kerl/Keccak

## Signatures
1. WOTS


## Replicator Protocol

This is a text-based protocol. It maintains two TCP sockets per peer. These maintain Source and Sink queues between the peers.


```
 SINK                          SOURCE
  |                              |
  |-----------PORT(10)---------->|
  |                              |
  |---TX(1650) + CHECKSUM(16)--->| // n times
  |                              |
```

#### Message formats:
```
  PORT(10) => zero-padded listen port string
  TX(1650) => tryte-encoded TX value
  CHECKSUM(16) => zero-padded hex-encoded CRC32 of TX
 ```


## Node

```
IRI
 ├── Iota
 │    ├── Tangle
 │    ├── Milestone
 │    ├── TransactionValidator
 │    ├── LedgerValidator *
 │    ├── TipsManager
 │    ├── TransactionRequester
 │    ├── UDPReceiver
 │    ├── Replicator
 │    └── Node
 │
 ├── IXI
 │    └── ModuleFileWatcher
 │
 └── API
      └── HTTPListener

```

---
## Ternary Encodings

|TRYTE|EXPANSION| |DECIMAL|
|-|-|-|-|
|-1, -1, -1|(-1 &times; 3<sup>2</sup>) + (-1 &times; 3<sup>1</sup>) + (-1 &times; 3<sup>0</sup>)|-9 - 3 - 1|-13|
|-1, -1, 0|(-1 &times; 3<sup>2</sup>) + (-1 &times; 3<sup>1</sup>) + (0 &times; 3<sup>0</sup>)|-9 - 3 + 0|-12|
|-1, -1, 1|(-1 &times; 3<sup>2</sup>) + (-1 &times; 3<sup>1</sup>) + (1 &times; 3<sup>0</sup>)|-9 - 3  + 1|-11|
|-1, 0, -1|(-1 &times; 3<sup>2</sup>) + (0 &times; 3<sup>1</sup>) + (-1 &times; 3<sup>0</sup>)|-9 + 0 - 1|-10|
|-1, 0, 0|(-1 &times; 3<sup>2</sup>) + (0 &times; 3<sup>1</sup>) + (0 &times; 3<sup>0</sup>)|-9 + 0 + 0|-9|

```
....
-3: [0, -1, 0]
-2: [0, -1, 1]
-1: [0, 0, -1]
 0: [0, 0, 0]
 1: [0, 0, 1]
 2: [0, 1, -1]
 3: [0, 1, 0]
 ...
  9: [1, 0, 0]
 10: [1, 0, 1]
 11: [1, 1, -1]
 12: [1, 1, 0]
 13: [1, 1, 1]
```


|DECIMAL|TRITS|BCT BITS|BCT HEX|TCT BITS|IOTA|
|-|-|-|
|13|[1, 1, 1]|00 01 01 01|15|000 01101|4D|M

The binary encoding for Trits uses two-bits per trit:

	 1 => {0, 1}
	 0 => {0, 0}
	-1 => {1, 0}
	NOTE THAT {1, 1} IS INVALID
A single byte can therefore hold 4 trits.



Binary-encoded Trytes

Tryte-encoded Text
