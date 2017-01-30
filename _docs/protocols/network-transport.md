---
layout: default
title: Network Transport Layer
permalink: /protocols/network-transport/
group: protocols
---

# Network Transport Layer

This guide is for developers who want to build their own client for Cardano SL. Please read [Cardano SL Implementation Overview](/for-contributors/implementation) for more info. This guide covers network transport layer used in Cardano SL nodes.

## Principles

From the highest point of view we're talking about 4 steps:

1. Connecting to other node.
2. Sending message(s) to other node.
3. Receiving message(s) from other node.
4. Disconnecting from other node.

Fundamental properties of the implementation:

1. **Single connection**. Once a connection with other node is established, use it for sending/receiving messages until connection is _explicitly_ closed or some unrecoverable error occurred.
2. **Resistance to network lags/failures**. If a connection dropped, try to reconnect (not simply failing with some exception or error code).

## Overview

Basic network concepts are:

- Transport
- EndPoint
- Connection
- Event
- Errors

**Transport** represents real "TCP-point". Actually transport associated with concrete host and port.

**EndPoint** represents node from the network point of view. The main property of an endpoint is the endpoint's address. If we want to connect to the node, we should use the endpoint's address. Address is a binary string with the structure `"HOST:PORT:NODE_ID"`, for example, `"127.0.0.1:3010:0"`. It's theoretically possible to have more than one node within one transport, for example, `"127.0.0.1:3010:0"` and `"127.0.0.1:3010:1"`, but in most cases there's one node on one transport.

**Connection** is a _lightweight_ bidirectional connection between nodes. In fact, two connected nodes use one and _only one_ real TCP-connection, so lightweight connections are just a _logical_ concepts. You can think about them as about tiny bidirectional channels inside a TCP-connection. Every connection has an integer id. It's theoretically possible to have thousands of lightweight connections inside a single TCP-connection. When the first lightweight connection is created, the real TCP-connection is established. When the last lightweight connection is closed, the real TCP-connection is dropped. In these terms, you can think about real TCP-connection as about a _heavyweight_ connection.

**Event** represents some concrete network event. For example:

- Connection opened (_when new lightweight connection was opened_),
- Received (_when new message was received_),
- Connection closed (_when lightweight connection was closed_),
- EndPoint closed (_when endpoint closed and TCP-connection dropped_),
- Error (_when some error occurs_).

**Errors** describe different network errors, like errors during creation of the endpoint, during sending of messages or some common network error.

## Nodes interconnection notice

After your node was started, it looks around and tries to find other nodes (neighbors). Please read [P2P Network section](/for-contributors/implementation#p2p-network) for more info about peer discovery and neighbors. Further it's assumed that the list of neighbors' endpoints addresses is already obtained.

Every node should work asynchronously: the node should run thread(s) for sending messages and thread(s) for receiving messages.

Messages can be divided into two groups: command messages and data messages. We use command messages for the needs of network transport layer itself, like connection requests. Data messages are used for transmitting of some data between the nodes.

### Low-level notice

All messages must be encoded with [network byte order](https://en.wikipedia.org/wiki/Endianness#Networking) before sending.

`Word32` type represents 32-bit unsigned integer value.

## Understanding lightweight connection

As said above, lightweight connection is just a _logical_ concept over heavyweight connection (real TCP-connection). When node opens new (lightweight) connection, there's just an index of new lightweight connection created. And after that we use this index as a "mark" of this (lightweight) connection.

It can be described by this schema:

~~~
    Node A                                 Node B
 +----------+  one real TCP-connection  +----------+
 |          |===========================|          |
 |  Code 1 <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> Code 3  |
 |          |     lightweight conn 0    |          |
 +----------+                           +----------+
 |          |                           |          |
 |  Code 2  |                           |  Code 4  |
 |          |===========================|          |
 +----------+                           +----------+
~~~

When `Code 1` do `connect`, there's no real TCP-connection yet, so this real TCP-connection is establishing and after that lightweight connection `0` is created (it's just an index `0`, nothing more).

And when `Code 2` do `connect`, lightweight connection `1` is created, so _conceptually_ we're creating second TCP-connection, but _in reality_ we're still using the same TCP-connection:

~~~
    Node A                                 Node B
 +----------+  one real TCP-connection  +----------+
 |          |===========================|          |
 |  Code 1 <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> Code 3  |
 |          |     lightweight conn 0    |          |
 +----------+                           +----------+
 |          |     lightweight conn 1    |          |
 |  Code 2 <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> Code 4  |
 |          |===========================|          |
 +----------+                           +----------+
~~~

So, when `Code 1` sends a message to `Code 3`, index of lightweight connection `0` stores as `Word32`-value in this message. And when `Code 2` sends a message to `Code 4`, index of lightweight connection `1` stores as `Word32`-value in this message. So, _in reality_ we have two messages sent via single real TCP-connection. But _conceptually_ we have two messages sent via two different lightweight connections. In this case we'll never get any collisions: messages sent by `Code 1` will always be received by `Code 3` only, as well as messages sent by `Code 4` will always be received by `Code 2` only.

And when `Code 1` disconnected _explicitly_ from `Code 3` (or vice versa), lightweight connection `0` disappeared, there's no more messages between `Code 1` and `Code 3`. But our real TCP-connection is still here, and lightweight connection `1` is still here too, so `Code 2` and `Code 4` can continue sent messages to each other:

~~~
    Node A                                 Node B
 +----------+  one real TCP-connection  +----------+
 |          |===========================|          |
 |  Code 1  |                           |  Code 3  |
 |          |                           |          |
 +----------+                           +----------+
 |          |     lightweight conn 1    |          |
 |  Code 2 <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> Code 4  |
 |          |===========================|          |
 +----------+                           +----------+
~~~

And only when `Code 2` _or_ `Code 4` disconnected _explicitly_, lightweight connection `1` is gone, and due it's the last lightweight connection, our real TCP-connection dropped too.

## How to connect

When node `B` wants to connect to node `A`, it sends message called **connection request**. Message structure is:

~~~
+-----------+-----------+--------------------+
|   A-EPI   |   B-EPAl  |       B-EPA        |
+-----------+-----------+--------------------+

|   Word32  |   Word32  |       B-EPAl       |
~~~

where:

- `A-EPI` - node `A` endpoint's id,
- `B-EPAl` - length of the other node `B` endpoint's address,
- `B-EPA` - other node `B` endpoint's address.

If node `A` accepts connection request, it replies to node `B` with message called **connection request accepted**. Message structure is:

~~~
+-----------+
|   CRAF    |
+-----------+

|   Word32  |
~~~

where `CRAF` - connection request accepted flag, value `0`.

When node `B` receives **connection request accepted** message, it replies with message called **created new connection**. Message structure is:

~~~
+-----------+-----------+
|   CNCF    |   LWCId   |
+-----------+-----------|

|   Word32  |   Word32  | 
~~~

where:

- `CNCF` - created new connection flag, value `0`,
- `LWCId` - new lightweight connection's id.

After that nodes `B` and `A` will use lightweight connection with `LWCId` id to send messages to each other.

### Invalid connection request

If node `B` sent **connection request** with invalid endpoint (for example, with `A-EPI` that doesn't exist), node `A` replies with a message called **connection request invalid**. Message structure is:

~~~
+-----------+
|   CRIF    |
+-----------+

|   Word32  |
~~~

where `CRIF` - connection request invalid flag, value `1`.

### Crossed connection request

The tricky case arises when nodes `A` and `B` send connection request to each other _at the same time_. In this case node `B` receives connection request from the node `A` but finds that it had _already_ sent a connection request to the node `A`. So node `B` will accept connection request from the node `A` if endpoint's address of the node `A` is smaller (lexicographically) than endpoint's address of the node `B`, and reject it otherwise. If node `B` rejects it, it sends to the node `A` a message called **connection request crossed**. Message structure is:

~~~
+-----------+
|   CRCF    |
+-----------+

|   Word32  |
~~~

where `CRCF` - connection request crossed flag, value `2`.

If a connection exists between nodes `A` and `B` when node `B` rejects the request, node `B` will probe the connection to make sure it's healthy. If node `A` doesn't answer timely to the probe, node `B` will discard the connection.

When it receives a **connection request crossed** message the node `A`that initiated the request just needs to wait until the node `A` that is dealing with `B`'s connection request completes, unless there is a network failure. If there is a network failure, the initiator node would timeout and return an error.

## How to send/receive data messages

As said above, data messages contain some binary data. Of course, at this level we know nothing about such a data and treat it as a raw bytes.

When node `A` sends data message to the node `B`, message structure is:

~~~
+-----------+-----------+-------------------+
|   LWCId   |   DataL   |       Data        |
+-----------+-----------+-------------------+

|   Word32  |   Word32  |       DataL       |
~~~

where:

- `LWCId` - using lightweight connection id,
- `DataL` - length of the binary data,
- `Data` - binary data itself.

## How to disconnect

It's possible to request closing lightweight connection as well as heavyweight one.

If node `A` wants to disconnect from the node `B`, it sends a message called **close connection**. Message structure is:

~~~
+-----------+-----------+
|     CC    |   LWCId   |
+-----------+-----------|

|   Word32  |   Word32  | 
~~~

where:

- `CC` - close connection flag, value `1`,
- `LWCId` - using lightweight connection id.

After node `B` receives **close connection** message, it just replies with the same message. After that corresponding lightweight connection is gone.

Moreover, node `A` can request to close a socket (our real TCP-connection). In this case it sends to other node `B` a message called **close socket**. Message structure is:

~~~
+-----------+-----------+
|     CS    |   LWCId   |
+-----------+-----------|

|   Word32  |   Word32  | 
~~~

where:

- `CS` - close socket flag, value `2`,
- `LWCId` - using lightweight connection id.

After node `B` receives **close socket** message, it just replies with the same message. After that our real TCP-connection dropped.
