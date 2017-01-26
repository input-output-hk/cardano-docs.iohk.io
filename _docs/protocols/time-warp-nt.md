---
layout: default
title: Time-warp-NT
permalink: /protocols/time-warp-nt/
group: protocols
---

# Introduction

Time-warp is developed to provide reliable networking layer with different levels of abstractions. Its second big objective is to provide an easy way to write and run tests for distributed systems using emulation mode, which should be flexible enough to support different scenarios (tunable network delays, disconnects, other real-time conditions).

Time-warp is split up into two parts:

1. `Mockable` interfaces,
2. Network functionality.


# Mockable

`Mockable` interfaces allow to abstract from language-specific details of implementation of the basic functions.

They are split to several categories. For instance, `Mockable Delay` contains `delay` operation, while `Mockable Fork` keeps elementary functions to manipulate with threads.

Such innovation allows to launch same code both in production and testing environment, where the latter allows to emulate time, threads, networking, etc.

`Production` implements all those interfaces with references to respective prototypes of the functions.


# Networking

This layer is written on top of _network-transport_ and provides network capabilities for application layer. It is split up to two sub-layers, **lower** and **upper**.


## Lower layer

This sub-layer is direct wrapper over _network-transport_ and provides convenient interface which allows to initiate lightweight connection and send/receive data on it. Please read [Network Transport Layer guide](/protocols/network-transport) for more info.

It supports two types of connections, **unidirectional** and **bidirectional**.

### Unidirectional connections

Unidirectional connections allows to send stream of bytes without waiting for peer's response.

Function `withOutChannel` executes given action with context to send data chunks to peer using one-shot lightweight connection.

Upon connection initialization, node sends `UNI`:

~~~
+------------------+
|       UNI        |
+------------------+

|   'U' :: Word8   |
~~~

`Word8` represents 8-bit unsigned integer value.

### Bidirectional connections

Bidirectional connections allow both nodes to send and receive bytes to each other.

Function `withInOutChannel` establishes connection, executes given action with context enabling sending and receiving bytes on connection, and automatically closes connection on action's end. Its usage requires handshake, which contains following steps.

First, initiator sends **connection request**, which has following structure:

~~~
+------------------+-----------------+
|     `BI_SYN`     |      Nonce      |
+------------------+-----------------+

|   'S' :: Word8   |   Word64 (BE)   |
~~~

where `Nonce` is randomly generated.

Then peer sends **acknowledgement**, with has following structure:

~~~
+------------------+-----------------+
|     `BI_ACK`     |      Nonce      |
+------------------+-----------------+

|   'A' :: Word8   |   Word64 (BE)   |
~~~

where `Nonce` is same nonce which came from request.

If initiator receives acknowledgement with correct nonce, conversation starts.

The opposite case could take place if node never sent request on that nonce (peer made a protocol error), but it could also be that node did send the `BI_SYN`, but its handler for that conversation has already finished. That's just normal, and node should ignore such acknowledgement.


## Messaging

Before talking about upper layer, we first describe messaging.

In order to specify different handlers for various message types, sent messages should implement `Message` interface, defining `messageName` function. It tells unique message identifier, which is sent along with the message itself and allows receiver to select correct handler to process this message.


## Upper layer

This sub-layer enables message exchange. It provides two styles of communication:

1. *One-message style* allows to send single message and uses unidirectional connection from layer below. Here node first sends message name, then message itself.
2. *Conversation style* uses capabilities of bidirectional connectionand allows to send / receive messages. In this case initiator node sends message name once, then both initiator and peer just send required messages.

Network events processing is initiated by `node` function, where ***worker*** and ***listeners*** arguments should be specified.

***Worker*** is some action which performs as initiator of all communication, being supplied with `SendAction`. `SendAction` provides two functions:

1. `sendTo` sends a message in *one-message style*.
2. `withConnectionTo` initiates *conversation*, executing given action with `ConversationActions` in its context and closing conversation once action completes. `ConversationActions` provides `send` and `recv` functions to communicate with peer.

***Listener*** is a handler for message. Each listener remembers type of related message, and several listeners with non-overlapping message types could be defined. Listeners could be of two types:

1. `ListenerActionOneMsg`, for *one-message style*.
2. `ListenerActionConversation`, for *conversation style*.


## Serialization

Time-warp doesn't rely on any predefined serialization strategy, but rather allows user to use its own.

To define custom serialization, user should create special data type, so-called *packing type*, and implement `Packable` and `Unpackable` interfaces for it.
