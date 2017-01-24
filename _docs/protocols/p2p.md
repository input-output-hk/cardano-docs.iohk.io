---
layout: default
title: P2P Layer
permalink: /protocols/p2p/
group: protocols
---

# P2P Layer

To start communicating with other nodes, a node has to join the network. To do this, the node has to know some other node that already participates in the protocol; this node is called a *bootstrap node*.

After connecting to the bootstrap node, we receive a list of peers whom we'll use for network communication. Those peers are called *neighbors*. The list of neighbors should be maintained in such a way that these nodes are online and any node from the network can receive our messages. Moreover, messages should be delivered efficiently.

To achieve this, Cardano SL uses the *Kademlia* DHT protocol. Even though Kademlia provides more features, we only use it as a method of peer discovery.

## An overview of the Kademlia protocol

*Also see: the [P2P Network section](/for-contributors/implementation#p2p-network) of the guide for contributors.*

In Kademlia, every node is associated with a 32-byte ID (see [Binary representation of messages](#binary-representation-of-messages) for more details). These IDs are used to identify nodes without having to refer to their network addresses. Keys used to store values in Kademlia are also 32-byte identifiers.

Kademlia uses the XOR metric to define distance between nodes. Key-value pairs are stored in nodes with IDs that are “close” to the keys. This distance is also used to efficiently locate a node with the given ID.

At start, a bootstrap node should be provided to Kademlia in order to join the network. Later, the node will attempt to find more peers by querying its neighbors (from the initial list of peers sent by the bootstap node). A node sends messages to its peers, which resend messages to their peers close to the needed ID/key.

Kademlia uses the UDP protocol for transmitting messages.

To learn more about how Kademlia is implemented, please refer to the paper [Kademlia: a Peer-to-peer Information System Based on the XOR Metric](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf).

## Messages used in Kademlia

**PING**: Check if a peer is still accessible. After sending this message the node would expect to receive a *PONG* message as the reply. Kademlia pings every peer periodically to maintain a correct peer list.

**PONG**: Used as a reply to *PING* messages.

**STORE ID value**: Store given value in Kademlia. This message is disabled and would be ignored by nodes.

**FIND\_NODE ID**: Request network address of node with given ID. After sending this message the node would expect to receive a *RETURN\_NODES* message with a list of nodes closest to the requested one (including the requested node itself).

**FIND\_VALUE key**: Behaves just like *FIND\_NODE*, except that it can also receive a *RETURN\_VALUE* response in case of a successful lookup. Currently it's only used in Cardano SL for finding peers. When the node starts working, it generates a random key and asks Kademlia to find it; this search always fails, but it lets the node discover some initial peer addresses.

**RETURN\_VALUE key value nodes**: A reply to a *STORE* request. This message is not used in Cardano SL because it does not store any values in Kademlia.

**RETURN\_NODES nodes**: Send network addresses of some nodes in reply to *FIND\_NODE* of *FIND\_VALUE*.

## Binary representation of messages

Every message is represented as a binary string with length of at most 1200 bytes (so that it wouldn't exceed IPv6 datagram size). A special case is *RETURN\_NODES*: if it exceeds 1200 bytes, the node list is split into several messages. The number of messages is represented with a single byte.

Each package is a concatenation of the following binary sequences for each peer:

    <Peer ID><Peer host><Peer port>

All IDs and keys are represented as 32-byte strings in the following format:

    <Hash><Nonce>

Here *Nonce* is a random binary string and *Hash* is a *PBKDF2* key generated from *Nonce*.

| Message           | Binary representation                                           |
|-------------------|-----------------------------------------------------------------|
| **PING**          | 0\<Our ID\>                                                     |
| **PONG**          | 1\<Our ID\>                                                     |
| **STORE**         | 2\<Our ID\>\<Key\>\<Value\>                                     |
| **FIND_NODE**     | 3\<Our ID\>\<Destination ID\>                                   |
| **FIND\_VALUE**   | 4\<Our ID\>\<Key\>                                              |
| **RETURN\_VALUE** | 5\<Our ID\>\<Destination ID\>\<Value\>                          |
| **RETURN\_NODES** | 6\<Our ID\>\<Number of packages\>\<Destination ID\>\<Packages\> |

## Security

Since Kademlia is a protocol for open P2P networks, it had to be modified in several other ways to become reasonably secure.

### Possible attacks

An **eclipse attack** is a situation when a node is surrounded by adversary nodes.

In Kademlia, eclipse attacks (targeted at a particular participant of the network) are possible but hard. First, launch a hundred nodes with node IDs close to target node ID. These nodes would fill the node’s lowest k-buckets (which are probably empty). Then, DDoS nodes from target’s k-buckets (it’s possible to determine those nodes if network's topology didn't change much after the node’s start). After a successful DDoS, node's remaining neighbors would be adversary agents.

Please note that Kademlia’s structure implies that merely launching nodes close to the target is not enough to eclipse it. Node lists are stored by node in k-buckets (the i-th bucket contains no more than k nodes with relative distance `2^i-1 < d < 2^i`), and new nodes are added to corresponding buckets only if those buckets are not full already. Kademlia prefers nodes that have been in lists for a long time and were recently seen alive. Without getting some nodes down it’s impossible to eclipse a node.

This attack is tricky and unlikely to happen in practice. Also, the [Addressing](#addressing) modification makes it even harder.

A **100500 attack** is an attack that launches significantly more nodes than the amount of nodes in the current P2P network, either in order to eclipse some nodes or to deny service by flooding the network. The attack wouldn't cause any problems for old nodes (with a possible exception of some network overhead), because old nodes preserve their routes. But when a new node joins the network, it would get eclipsed (isolated in an adversarial subnet), because old honest nodes won’t add it to their buckets (as these buckets are already filled by other nodes) and the new node would be known only to adversaries.

Defending against 100500 attacks remains an open problem. However, we’re going to make them practically infeasible with sophisticated ban system / adversary detection.

### Addressing

For Kademlia addresses we use so-called HashNodeId. This makes it impossible to assign yourself an arbitrary ID, which makes a 100500 attack the only way to perform an eclipse attack.

See:
[Network.Kademlia.HashNodeId](https://github.com/serokell/kademlia/blob/7f3f96d7bfdb80077ac27b0a424828fa88d85334/src/Network/Kademlia/HashNodeId.hs)

### Routing data anti-forging

In Kademlia, node requests a list of peers from its neighbors and accepts the first message it receives. An adversary may forge those replies, providing addresses of adversary nodes as closest nodes to given ID. To overcome this issue, we make nodes wait for some period to gather as many replies as possible, after which the replies get merged and the node selects K closest nodes from the resulting set. This way an adversary would have to eclipse a node in order to forge the list of peers it receives.

### Routing tables sharing

When a node has just joined the network, it requests a list of neighbors (set of nodes closest to it). We have modified Kademlia to include some extra nodes into this list – specifically, now we just pick some random nodes along with neighbors and return them. This gives the node additional knowledge to recover in the case it’s surrounded with adversary nodes.

See:
+ [Network.Kademlia.Tree.pickupRandom](https://github.com/serokell/kademlia/blob/7f3f96d7bfdb80077ac27b0a424828fa88d85334/src/Network/Kademlia/Tree.hs#L219)
+ [Network.Kademlia.Tree.findClosest](https://github.com/serokell/kademlia/blob/7f3f96d7bfdb80077ac27b0a424828fa88d85334/src/Network/Kademlia/Tree.hs#L234)
+ [Network.Kademlia.Instance.returnNodes](https://github.com/serokell/kademlia/blob/7f3f96d7bfdb80077ac27b0a424828fa88d85334/src/Network/Kademlia/Instance.hs#L360)

### Ban nodes

We introduced a feature to ban nodes to Kademlia. We will use this to ban nodes when we detect them to act maliciously.

See:
+ [Network.Kademlia.Instance](https://github.com/serokell/kademlia/blob/7f3f96d7bfdb80077ac27b0a424828fa88d85334/src/Network/Kademlia/Instance.hs)
+ [Network.Kademlia.Implementation](https://github.com/serokell/kademlia/blob/7f3f96d7bfdb80077ac27b0a424828fa88d85334/src/Network/Kademlia/Instance.hs)
