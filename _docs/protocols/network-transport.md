---
layout: default
title: Network Transport Layer
permalink: /protocols/network-transport/
group: protocols
---

# Network Transport Layer

This guide will be useful for developers who want to build their own client for Cardano SL. Please read [Cardano SL Implementation Overview](/for-contributors/implementation) for more info. This guide covers network layer used in Cardano SL nodes.

## Neighbors 

After your node was started, it looks around and tries to find other nodes (neighbors). Please read [P2P Network section](/for-contributors/implementation#p2p-network) for more info about peer discovery and neighbors. Further it's assumed that list of neighbors' endpoints addresses already obtained.

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

- EndPoint
- Connection
- Event
- Errors

**EndPoint** represents node from the network point of view. The main property of endpoint is endpoint's address. If we want to connect to the node, we should use endpoint's address. Address is a binary string with the structure `HOST:PORT:NODE_ID`, for example, `"127.0.0.1:3010:0"`.

**Connection** is a lightweight bidirectional connection between nodes. In fact two connected nodes use one and _only one_ real TCP-connection, so lightweight connection is a _logical_ connection. You can think about it as about tiny bidirectional channel inside single TCP-connection. It's possible to have thousands of lightweight connections inside TCP-connection. When the first lightweight connection created, real TCP-connection is established. When the last lightweight connection is closed, real TCP-connection is dropped.

**Event** represents some concrete network event. For example:

- Connection opened (when new lightweight connection was opened),
- Received (when new message was received),
- Connection closed (when lightweight connection was closed),
- EndPoint closed (when endpoint closed and TCP-connection dropped),
- Error event (when some error occurs).

**Errors** represents different network errors, like errors during creation of the endpoint, during sending messages or some common network error.

## Nodes interconnection notice

After your node was started, it should work asynchronously: there're should be thread(s) for sending messages and thread(s) for receiving messages. Moreover, each node should listen connection requests, when another node wants to connect with it.

All messages can be divided into two categories: commands and data. At current level we know nothing about data (it's just raw bytes), but we know about different commands (for example, connection request).

## Low-level notice

All commands/codes must be encoded with [network byte order](https://en.wikipedia.org/wiki/Endianness#Networking) before sending.

## How to connect

There're two situations: some node wants to connect to our one, or our node wants to connect to the other one.

### Listen connection requests

_Pending_

handleConnectionRequest

-- | Read a length and then a payload of that length

-- | Receive a 32-bit integer

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
