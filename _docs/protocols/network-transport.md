---
layout: default
title: Network Transport Layer
permalink: /protocols/network-transport/
group: protocols
---

# Network Transport Layer

This guide will be useful for developers who want to build their own client for Cardano SL. Please read [Cardano SL Implementation Overview](/for-contributors/implementation) for more info. This guide covers network transport layer used in Cardano SL nodes.

## Principles

From the highest point of view we're talking about four steps:

1. Connecting to other node.
2. Sending message(s) to other node.
3. Receiving message(s) from other node.
4. Disconnecting from other node.

Fundamental properties of the implementation:

1. Single connection. Once a connection with other node is established, use it for sending/receiving messages until connection is _explicitly_ closed or some unrecoverable error occurred.
2. Resistance to network lags/failures. If a connection dropped, try to reconnect (not simply failing with some exception or error code).

## Overview

Basic network concepts are:

- Transport
- EndPoint
- Connection
- Event
- Errors

**Transport** represents real "TCP-point". Actually transport associated with concrete host and port.

**EndPoint** represents node from the network point of view. The main property of endpoint is endpoint's address. If we want to connect to the node, we should use endpoint's address. Address is a binary string with the structure `"HOST:PORT:NODE_ID"`, for example, `"127.0.0.1:3010:0"`. It's theoretically possible to have more than one nodes within one transport, for example, `"127.0.0.1:3010:0"` and `"127.0.0.1:3010:1"`, but in most cases there's one node on one transport.

**Connection** is a _lightweight_ bidirectional connection between nodes. In fact two connected nodes use one and _only one_ real TCP-connection, so lightweight connections are just a _logical_ concepts. You can think about them as about tiny bidirectional channels inside TCP-connection. Every connection has an integer id. It's theoretically possible to have thousands of lightweight connections inside single TCP-connection. When the first lightweight connection created, real TCP-connection is established. When the last lightweight connection is closed, real TCP-connection is dropped. In these terms you can think about real TCP-connection as about _heavyweight_ connection.

**Event** represents some concrete network event. For example:

- Connection opened (_when new lightweight connection was opened_),
- Received (_when new message was received_),
- Connection closed (_when lightweight connection was closed_),
- EndPoint closed (_when endpoint closed and TCP-connection dropped_),
- Error (_when some error occurs_).

**Errors** describe different network errors, like errors during creation of the endpoint, during sending messages or some common network error.

## Nodes interconnection notice

After your node was started, it looks around and tries to find other nodes (neighbors). Please read [P2P Network section](/for-contributors/implementation#p2p-network) for more info about peer discovery and neighbors. Further it's assumed that list of neighbors' endpoints addresses already obtained.

Every node should work asynchronously: there're should be thread(s) for sending messages and thread(s) for receiving messages.

### Low-level notice

All messages must be encoded with [network byte order](https://en.wikipedia.org/wiki/Endianness#Networking) before sending.

`Word32` is 32-bit unsigned integer value.

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

So, when `Code 1` sends a message to `Code 3`, index of lightweight connection `0` stores as 4 bytes in this message (`Word32`-value). And when `Code 2` sends a message to `Code 4`, index of lightweight connection `1` stores as 4 bytes in this message. So, _in reality_ we have two messages sent via single real TCP-connection. But _conceptually_ we have two messages sent via two different lightweight connections. In this case we'll never get any collisions: messages sent by `Code 1` will always be received by `Code 3` only, as well as messages sent by `Code 4` will always be received by `Code 2` only.

And when `Code 1` disconnected _explicitly_ from `Code 3` (or vice versa), lightweight connection `0` disappeared, there're no more messages between `Code 1` and `Code 3`. But our real TCP-connection is still here, and lightweight connection `1` is still here too, so `Code 2` and `Code 4` can continue sent messages to each other:

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

There're two situations: some node wants to connect to your one, or your node wants to connect to the other one.

### Listen connection request

When node `B` wants to connect to your node `A`, it sends message called **connection request**. Message structure is:

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

_Pending_ about `Invalid` and `Crossed` replies.

### Send connection request

When your node `A` wants to connect to the other node `B`, it sends the same **connection request**. Message structure is:

~~~
+-----------+-----------+--------------------+
|   B-EPI   |   A-EPAl  |       A-EPA        |
+-----------+-----------+--------------------+

|   Word32  |   Word32  |       A-EPAl       |
~~~

where:

- `B-EPI` - other node `B` endpoint's id,
- `A-EPAl` - length of our node `A` endpoint's address,
- `A-EPA` - our node `A` endpoint's address.

The same situation

_Pending_

If node accepted your connection request, it replies with `ConnectionRequestAccepted` code with value `0` (`Word32`).

## How to send messages

When your node `A` wants to send message to other node `B`, 

## How to receive messages

_Pending_.
handleIncomingMessages

## How to disconnect

If your node `A` wants to disconnect from other node `B`, it sends a message called **close connection**. Message structure is:

~~~
+-----------+-----------+
|     CC    |   LWCId   |
+-----------+-----------|

|   Word32  |   Word32  | 
~~~

where:

- `CC` - close connection flag, value `1`.
- `LWCId` - using lightweight connection id.

After node `B` receives **close connection** message, it just replies with the same message. After that corresponding lightweight connection is gone.

Moreover, if there's no more lightweight connections, node `A` can request to close a socket (i.e. our real TCP-connection). In this case it sends to other node `B` a message called **close socket**. Message structure is:

~~~
+-----------+-----------+
|     CS    |   LWCId   |
+-----------+-----------|

|   Word32  |   Word32  | 
~~~

where:

- `CS` - close socket flag, value `2`.
- `LWCId` - using lightweight connection id.

After node `B` receives **close socket** message, it just replies with the same message. After that our real TCP-connection is gone.
