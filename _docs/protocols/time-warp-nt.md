---
layout: default
title: Time-warp-NT
permalink: /protocols/time-warp-nt/
group: protocols
---

# Time-warp layer

This layer is written on top of _network-transport_ and used as network library
at application layer.

Its interface design aims following points:

1. Convenience of *network-transport* functionality usage
2. Provide message exchange capabilities

**TODO** smth else?

This layer is split up to two sub-layers.

## Lower layer

This sub-layer is direct wrapper over _network-transport_ and provides
convenient interface which allows to initiate lightweight connection
and send/receive data on it.

It supports two types of connections:

* Unidirectional connection allows to send stream of bytes without waiting
for peer's response

 * `withOutChannel` executes given action with context to send
 data chunks to peer using one-shot lightweight connection.

 * Upon connection initialization, node sends `UNI`:

 ~~~
 +------------------+
 |       UNI        |
 +------------------+

 |   'U' :: Word8   |
 ~~~


* Bidirectional connections allow both to send and receive bytes.

 * `withInOutChannel` establishes connection, executes given action
with context enabling sending and receiving bytes on connection,
and automatically closes connection on action's end.

 * Its usage requires handshake, which contains following steps:

   * Initiator sends connection request, which has following structure:

   ~~~
   +------------------+-----------------+
   |     `BI_SYN`     |      Nonce      |
   +------------------+-----------------+

   |   'S' :: Word8   |   Word64 (BE)   |
   ~~~

   where `Nonce` is randomly generated.

   * Peer sends acknowledgement, with the following structure:

   ~~~
   +------------------+-----------------+
   |     `BI_ACK`     |      Nonce      |
   +------------------+-----------------+

   |   'A' :: Word8   |   Word64 (BE)   |
   ~~~

   where `Nonce` is same nonce which came from request.

   If initiator receives acknowledgement with correct nonce, conversation starts.

   The opposite case could take place if node never sent request on that nonce (peer made a protocol error), but it could also be that node did send the `BI_SYN`, but its handler for that conversation has already finished.
   That's just normal, and node should ignore such acknowledgement.


## Messaging

Before talking about upper layer, we first describe messaging.

**TODO** Change first section about messaging in *application-layer*?

In order to specify different handlers for various message types,
sent messages should implement `Message` interface, defining `messageName` function. It tells unique message identifier, which is sent
 along with the message itself and allows receiver to select correct handler
 to process this message.


## Upper layer

This sub-layer enables message exchange.

It provides two styles of communication:

* *One-message style* allows to send single message and uses unidirectional connection from layer below.

 Here node first sends message name, then message itself.

* *Conversation style* uses capabilities of bidirectional connection
and allows to send / receive messages.

 In this case initiator node sends message name once, then both initiator and
peer just send required messages.

Network events processing is initiated by `node` function, where
 ***worker*** and ***listeners*** arguments should be specified.

***Worker*** is some action which performs as initiator of all communication,
being supplied with `SendAction`. `SendAction` provides two functions:

* `sendTo` sends a message in *one-message style*.

* `withConnectionTo` initiates *conversation*, executing given action with
`ConversationActions` in its context and closing conversation once
action completes.

`ConversationActions` provides `send` and `recv` functions to communicate with peer.

***Listener*** is a handler for message. Each listener remembers type of
 related message, and several listeners with non-overlapping message types
 could be defined.

Listeners could be of two types: `ListenerActionOneMsg` and
`ListenerActionConversation`, which should be used for *one-message style*
and *conversation style* accordingly.


## Serialization

Time-warp doesn't rely on any predefined serialization strategy, but rather
allows user to use its own.

To define custom serialization, user should create special data type, so-called
*packing type*, and implement `Packable` and `Unpackable` interfaces for it.
