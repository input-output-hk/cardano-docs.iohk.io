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

## Overiview

Basic network concepts are:

- Transport
- EndPoint
- Connection
- Event
- Errors

**Transport** represents real "TCP-point". Actually transport associated with concrete host and port.

**EndPoint** represents node from the network point of view. The main property of endpoint is endpoint's address. If we want to connect to the node, we should use endpoint's address. Address is a binary string with the structure `"HOST:PORT:NODE_ID"`, for example, `"127.0.0.1:3010:0"`. Theoretically it's possible to have N nodes within one **Transport**, for example, `"127.0.0.1:3010:0"` and `"127.0.0.1:3010:1"`.

**Connection** is a lightweight bidirectional connection between nodes. In fact two connected nodes use one and _only one_ real TCP-connection, so lightweight connection is a _logical_ concept. You can think about it as about tiny bidirectional channel inside TCP-connection. It's theoretically possible to have thousands of lightweight connections inside single TCP-connection. When the first lightweight connection created, real TCP-connection is established. When the last lightweight connection is closed, real TCP-connection is dropped.

**Event** represents some concrete network event. For example:

- Connection opened (_when new lightweight connection was opened_),
- Received (_when new message was received_),
- Connection closed (_when lightweight connection was closed_),
- EndPoint closed (_when endpoint closed and TCP-connection dropped_),
- Error (_when some error occurs_).

**Errors** describe different network errors, like errors during creation of the endpoint, during sending messages or some common network error.

## Nodes interconnection notice

After your node was started, it looks around and tries to find other nodes (neighbors). Please read [P2P Network section](/for-contributors/implementation#p2p-network) for more info about peer discovery and neighbors. Further it's assumed that list of neighbors' endpoints addresses already obtained.

Your node should work asynchronously: there're should be thread(s) for sending messages and thread(s) for receiving messages.

All messages can be divided into two categories: commands and data. At current level we know nothing about data (it's just a raw bytes), but we know about different commands (for example, connection request). Data is explained at the higher level protocol.

### Low-level notice

All commands/codes must be encoded with [network byte order](https://en.wikipedia.org/wiki/Endianness#Networking) before sending.

## How to connect

There're two situations: some node wants to connect to your one, or your node wants to connect to the other one.

### Listen connection requests

When node `B` wants to connect to your node `A`, it sends **connection request**. Message structure is:

~~~
+-----------+-----------+--------------------+
|  yourEPI  | theirEPAl |      theirEPA      |
+-----------+-----------+--------------------+

|   Int32   |   Int32   |     theirEPAl      |
~~~

where:

- `yourEPI` - your node endpoint's id,
- `theirEPAl` - length of the other node endpoint's address,
- `theirEPA` - other node endpoint's address.

### Send connection requests

_Pending_


If node accepted your connection request, it replies with `ConnectionRequestAccepted` code with value `0` (`Int32`).

## How to send messages

_Pending_.

## How to receive messages

_Pending_.
handleIncomingMessages

## How to disconnect

If your node wants to disconnect from other node, it should send `CloseSocket` code with value `2` (`Int32`).
