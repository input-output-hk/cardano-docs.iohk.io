---
layout: default
title: Time-warp-NT
permalink: /protocols/time-warp-nt/
group: protocols
---

# Introduction

Time-warp is developed to provide a reliable networking layer with different
levels of abstractions. Its second big objective is to provide an easy way to
write and run tests for distributed systems using emulation mode, which should
be flexible enough to support different scenarios (tunable network delays,
disconnects, other real-time conditions).

time-warp-nt provides:

1. `Mockable` interfaces, to facilitate simulation.
2. Network functionality, which is abstracted over something which conforms to
   the network-transport protocol and may therefore also be simulated.

This document gives an overview of time-warp-nt and its usage, as well as a
description of the communication protocol implemented by time-warp-nt.

# Networking protocol

This section describes a protocol which is implemented by time-warp-nt, but
does not describe that particular implementation.

This layer is written on top of network-transport and provides a
protocol for unidirectional and bidirectional peer-to-peer connections where
each peer has associated static data.

All network-transport connections must be reliable and ordered.

Note that unidirectional connections are provided by network-transport, but
bidirectional connections and static peer data are not.

### Static peer data

Every peer has a value, chosen by that peer, called the _static peer data_ or
simply _peer data_. The form of this data is application-specific, and the
protocol assumes only that the application is capable of parsing it from a
stream of bytes.

When peer `A` establishes one or more connections to peer `B` (according to the
network-transport protocol), one of these connections must send the complete
peer data before any connection begins handshake (next section).

Arbitrarily-many connections from `A` to `B` may be established, possibly
concurrently (according to the network-transport protocol), but the peer data
must be completely transmitted by one and only one connection before the others
may continue.

If one connection has begun to, but fails to send all of the peer data, because
the connection was closed prematurely but normally, then another connection may
send the peer data, but only after that connection has been closed
(network-transport protocol).

If the connection closes due to an exception on the socket, then all other
connections (also carried on that socket, by network-transport specification)
are lost, and so none may take over responsibility for sending the peer data.

This is to ensure that whenever `B` receives a message from `A` (message
exchange section), `B` knows the static peer data of `A`.

### Handshake

After the peer data has been transmitted by some connection from `A` to `B`,
every connection from `A` to `B` must perform a handshake to indicate whether
it is _unidirectional_ or _bidirectional_.

A unidirectional connection is indicated by transmitting an ASCII 'U'. After
this has been sent, message exchange can begin (see the 'Unidirectional'
subsection of the 'Message exchange' section).

~~~
+------------------+
|       UNI        |
+------------------+

|   'U' :: Word8   |
~~~

A bidirectional connection _request_ is indicated by transmitting an ASCII 'S'
(for SYN) followed by 64 bits (the nonce). This is to identify the bidirectional
connection, which is in fact composed of two independent network-transport
connections: one from `A` to `B`, and one from `B` to `A`. After this has been
transmitted, the peer may proceed to message exchange
(see the 'Bidirectional, initiating peer' subsection of the 'Message exchange'
section).

~~~
+------------------+-----------------+
|     `BI_SYN`     |      Nonce      |
+------------------+-----------------+

|   'S' :: Word8   |   Word64 (BE)   |
~~~

A bidirectional connection _response_ is indicated by transmitting an ASCII 'A'
(for ACK) followed by 64 bits (the nonce). This is to identify the bidirectional
connection request for which it is a response. After this response is
transmitted, the responding peer must wait for more data from the initiating
peer (see the 'Bidirectional, responding peer' subsection of the
'Message exchange' section).

~~~
+------------------+-----------------+
|     `BI_ACK`     |      Nonce      |
+------------------+-----------------+

|   'A' :: Word8   |   Word64 (BE)   |
~~~

A peer which receives a bidirectional connection _request_ must create a
connection to the initiating peer, carry out peer data exchange if necessary
(previous section), and then perform the bidirectional connection response
using the same nonce which it received from the initiating peer.

It is recommended, but not required, that the initiating peer implement a
timeout for the bidirectional connection response, for there is no guarantee
that the responding peer will indeed send the expected response (a protocol
error).

### Message exchange

All message exchanges begin with the transmission of an application-specific
message name. It is assumed that the application is capable of parsing message
names from a stream of bytes, and parsing a message body from a stream of
bytes, where this parser may depend upon the message name.

#### Unidirectional

If the connection is unidirectional, the peer must transmit the message name
followed by its body, where the expected form of the body is also
application-specific and may be determined by the message name. After
transmission, the peer must close the network-transport connection. Excess
data must be ignored by the receiving peer.

#### Bidirectional, initiating peer

If the connection is bidirectional, the initiating peer (the peer which sent
the SYN) must transmit the message name, and may transmit arbitrarily-many
message bodies. Message exchange may begin immediately after it has sent its
handshake requirements. That is to say, the initiating peer is not required to
wait for the ACK before commencing message exchange.

#### Bidirectional, responding peer

If the connection is bidirectional, the responding peer (the peer which
received the SYN and sent the ACK) must not transmit a message name (the
initiating peer will send this) but may transmit arbitrarily-many message
bodies immediately after it has received the message name from the initiating
peer. The form of the message bodies is fixed for both peers, as determined by
the message name chosen by the initiating peer.


# time-warp-nt user guide


## Spinning up a node

A node is brought up by:

```Haskell
-- Simplified type (constraints omitted).
-- Defined in module Node
node :: Transport m
     -> StdGen
     -> packingType
     -> peerData
     -> (Node m -> NodeAction packingType peerData m t)
     -> m t
```

There are four prerequisites for a node:

  - A `Transport` from `Network.Transport.Abstract`, which implements the
    lower-level network-transport protocol over which the node will operate.
    See the 'Networking' section.
  - A source of randomness. This is used to produce nonces for bidirectional
    connections. See the 'Networking' section.
  - A packing type. This is used to determine how serializable types are
    encoded. See the 'Serialization' subsection of this section).
  - Static peer data. This is sent to every peer to which this node
    connects. See the 'Networking' section.

The final argument determines the listeners for this node and the computation
to carry out once the node is up and running. The `NodeAction` value must be
computed purely, and be lazy in the components of the `Node m` argument. That's
because the listeners must be defined before the node is made available on the
network, else traffic could induce a listener before the listeners are known.

```Haskell
-- Defined in module Node
data NodeAction packingType peerData m t =
    NodeAction
        [Listener packingType peerData m]
        (SendActions packingType peerData m -> m t)
```

The second component of a `NodeAction` can be strict in the `Node m` and also
the `SendActions`, which allow the resulting computation to contact peers (see
the 'Contacting peers' subsection of this section). When the resulting
computation finishes, the node will stop accepting traffic from peers, and will
wait for all running listeners to finish.

## Message

Peers communicate by sending _message_s. Any type is a message if it
implements the `Message` typeclass, i.e. if it gives a `MessageName`.

```Haskell
newtype MessageName = MessageName ByteString

-- Simplified definition. See Node.Message for the real thing.
class Message thing where
    messageName :: Proxy thing -> MessageName
```

Conceptually, a message is a message name followed by some payload, as in

```Haskell
data Message thing = Message MessageName thing
```

but this is realized in time-warp-nt only indirectly, by way of the
`Message` typeclass and `messageName`.

## Defining listeners

The set of listeners for a node is static, and is specified by a list of
`ListenerAction` values (see the `NodeAction` data type). Each listener
determines a message type, which is an instance of `Message` and therefore
determines a message name. That name is used to identify the listener. Every
listener in the set must be for a message with a message name distinct from all
others.

Listeners come in two variants: those for unidirectional connections, and those
for bidirectional connections. Their differences can be seen in their types:

```Haskell
ListenerActionOneMsg
    :: ( Serializable packingType message, Message message )
    => (peerData -> NodeId -> SendActions packingType peerData m -> message -> m ())
    -> ListenerAction packingType peerData m
```

This is a unidirectional listener. It is run whenever a peer sends a value of
type `message` on a unidirectional connection. Made available to it is the
static peer data of the remote peer, the address of that peer, the content
of the message, and a `SendActions` (see the next subsection,
'Contacting peers').

Contrast this with a bidirectional listener:

```Haskell
ListenerActionConversation
    :: ( Packable packingType send, Unpackable packingType recv, Message recv )
    => (peerData -> NodeId -> ConversationActions peerData send recv m -> m ())
    -> ListenerAction packingType peerData m
```

Such a listener is run whenever a peer sends a value of type `message` on a
bidirectional connection. Like a unidirectional listener, it receives the
static peer data and address of the remote peer, but it does not receive a
message body. Instead, it is given a `ConversationActions` which allows for
sending and receiving message bodies.

```Haskell
send
    :: ConversationActions peerData sendBody recvBody m
    -> sendBody
    -> m ()

recv
    :: ConversationActions peerData sendBody recvBody m
    -> m (Maybe recvBody)
```

If `recv` gives `Nothing` then no more data will ever come from the peer;
subsequent `recv`s will block indefinitely.

## Contacting peers

There are two ways to communicate with a peer, both of which use a `SendActions`
value.

```Haskell
-- Unidirectional
sendTo
    :: ( Message message, Packable packingType message )
    => SendActions packingType peerData m
    -> NodeId
    -> message
    -> m ()

-- Bidirectional
withConnectionTo
    :: ( Message send
       , Packing packingType send
       , Unpackable packingType recv
       )
    => SendActions packingType peerData m
    -> NodeId
    -> (m peerData -> ConversationActions peerData send recv m -> m t)
    -> m t
```

The last argument of `withConnectionTo` is given the receiving peer's static
peer data, but within `m` because this data may not be available immediately.

## Serialization

### Packable

Instances of the `Packable` typeclass show how to serialize a thing according
to its packing type (see the 'Serialization strategy' section).

```Haskell
class Packable packing thing where
    packMsg :: packing -> thing -> Lazy.ByteString
```

### Unpackable

Instances of the `Unpackable` typeclass show how to deserialize a thing
according to its packing type (see the 'Serialization strategy' section).

```Haskell
class Unpackable packing thing where
    unpackMsg :: packing -> Decoder thing
```

Decoder is defined in `Data.Binary.Get` of the 'binary' package and facilitates
incremental decoding with the possibility of interleaved monadic effects.
`unpackMsg` therefore should be general enough to allow for deserialization
from anything which carries a stream of bytes, whether a socket, a channel,
stdin, etc.

Of course, every `Unpackable` instance must satisfy the following law:

```
  pushChunks (unpackMsg packing) (packMsg packing thing)
= Done "" (length (packMsg packing thing)) thing
```

The first and second fields of `Done` are the trailing bytes and the number
of bytes consumed, respectively (see `Data.Binary.Get`)
`pushChunks` is also defined in `Data.Binary.Get`.

### Serializable

As it's common to require both serialization and deserialization, the following
type synonym is defined:

```Haskell
type Serializable packing thing =
    ( Packable packing thing
    , Unpackable packing thing
    )
```

### Serialization strategy

Although serialization always targets a lazy `ByteString`, time-warp-nt includes
the notion of a serialization strategy, or _packing type_, to allow for more
than one encoding.

To define such a strategy, the user should create special data type, called a
_packing type_, and give `Packable` and `Unpackable` instances for it.

#### The `BinaryP` packing type

One packing type is included in time-warp-nt, and is defined in `Node.Message`.

```Haskell
data BinaryP = BinaryP
```

This packing type uses `Binary` instances (from the 'binary' package) to
fulfill the `Packable` and `Unpackable` typeclasses.

## Mockable

### Motivation

The goal is to be able to swap out impure pieces of a program, such as
concurrency primitives or foreign function calls, without changing the
program text. This allows for the very same program to be executed with
different interpretations for various impure components like databases
network infrastructure.

A _mockable component_ has a single _description_ and arbitrarily-many
_interpretations_.

As we seek only to simulate _impure_ pieces, their interpretations are always
peculiar to some monad.

### Description

The description of a mockable component is a GADT of kind

```Haskell
(* -> *) -> * -> *
```

A term of a mockable component type is a formal representation of some term
of its first type parameter `m :: * -> *` applied to its second type parameter
`t :: *`. The type of such a term therefore determines the type `m t` of a
monadic term.

### Interpretation

An interpretation is an instance of the `Mockable` typeclass.

```Haskell
-- Defined in Mockable.Class
class ( Monad m ) => Mockable d m where
    liftMockable :: d m t -> m t
```

In English, an instance of this class is pronounced "`d` is mocked in `m` by
way of `liftMockable`".

### Introduction and elimination

To appreciate how the description and interpretation work together, and why
they are defined as they are, a very simple example is in order.

```Haskell
-- A mockable type.
data StringIO (m :: * -> *) (t :: *) where
    Input :: StringIO m String
    Output :: String -> StringIO m ()

input :: ( Mockable StringIO m ) => m String
input = liftMockable Input

output :: ( Mockable StringIO m ) => String -> m ()
output str = liftMockable (Output str)

-- An impure interpretation which uses stdin and stdout.
instance Mockable Logger IO where
    liftMockable term = case term of
        Input -> getLine
        Output str -> putStrLn str

-- A pure interpretation which uses a predefined infinite stream of input
-- and produces a finite list of output.
instance Mockable Logger (State (Stream String, [String])) where
    liftMockable term = case term of
        Input -> do
            (inp, out) <- get
            let (next, rest) = streamUncons inp
            put (rest, out)
            return next
        Output str -> do
            (inp, out) <- get
            put (inp, str : out)

-- Shout can be run in IO or State (Stream String, [String]).
shout :: ( Mockable Logger m ) => m ()
shout = do
    x <- input
    output (fmap toUpper x)
```

Notice that the terms of a representation type, such as
`Output str :: StringIO m ()`, are used only in two ways:

  - Introduced by definitions of `input` and `output`, as arguments to
    `liftMockable`.
  - Eliminated by definitions of `liftMockable`, to inject them into the monad
    `m`.

This is always the pattern of use. Further examples can be found in the
`src/Mockable` subdirectory of time-warp-nt. The module `Mockable.Production`
gives many examples of interpretations of mockable components in `IO`.


