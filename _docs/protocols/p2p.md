---
layout: default
title: P2P Layer
permalink: /protocols/p2p/
group: protocols
---

# P2P Layer

To start communicating with other nodes of the Cardano SL node need to join the network. To do this node has to know one node already participating in the protocol.
We call this node *bootstrap node*. When connected to the bootstrap node we recieve list of peers used for network communication called *neighbors*.
This list should be maintained in such a way that these nodes are online and any node from the network can receive our messages. Moreover messages should be delivered efficiently.

To achieve this Cardano SL uses Kademlia DHT protocol. Kademlia DHT is used only for peer discovery whereas it provides more features.

## Briefly how Kademlia works

Every node is associated with some 32-byte ID (see [Messages binary representation](#messages-binary-representation)) used to identify the node not using its network address. Keys used to store values in Kademlia are also 32-byte identifiers.
[P2P Network section](/for-contributors/implementation#p2p-network)

Kademlia uses XOR-metric to define distance between nodes. Key-value pairs are store in nodes with ID close to the key. Also this distance is used to locate
a node with the given ID efficiently.

At start bootstrap node should be provided to kademlia to join the network, it will provide initial list of peers which will be extended with sending and receiving some Kademlia messages. Kademlia node sends messages to its peers which resend messages to their peers close to the needed ID/key.

Kademlia uses UDP protocol for transmitting packages.

To learn more about how Kademlia is implemented read the [paper](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf).

## Messages used in Kademlia

**PING**: Check if peer is still accessible. Node which sent *PING* message would expect to receive *PONG* message as answer. Kademlia pings every peer periodically to maintain correct peer list.

**PONG**: Used as answer to *PING* messages.

**STORE ID value**: Store given value in Kademlia. This message is disabled and would be ignored by nodes.

**FIND\_NODE ID**: Request network address of node with given ID. Node which sent this message would expect to receive *RETURN\_NODES* answer with some nodes closest to the requested one (the requested node would be returned as well).

**FIND\_VALUE key**: Behaves just like *FIND\_NODE* except we can receive also *RETURN\_VALUE* response. Now used in Cardano SL for only one purpose: at the node start we generate some random key and ask Kademlia to find it. This search always fails but we discover some initial peer addresses.

**RETURN\_VALUE key value nodes**: Answer to *STORE* request. This message is not used in Cardano SL because we do not store any values in Kademlia.

**RETURN\_NODES nodes**: Send network addresses of some nodes as answer to *FIND\_NODE* of *FIND\_VALUE*

## Messages binary representation

Every message is represented as binary string with size of at most 1200 bytes (to not exceed IPV6 datagram size).
Special case is *RETURN\_NODES*: in case this message exceed 1200 bytes, node list is splitted into several packages. Number of packages is one byte length.
Where each package is concatenation of the following binary sequences for each peer:

    <Peer ID><Peer host><Peer port>

All IDs and keys are represented as 32-byte string of following format:

    <Hash><Nonce>

Where *Nonce* is random binary string and *Hash* is *PBKDF2* key generated from *Nonce*

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

Being a protocol for open P2P network, Kademlia protocol need to be modified in few other ways to become reasonably secure.

### Possible attacks

**Eclipse attack** is a situation when node is surrounded by adversary nodes.

In Kademlia eclipse attacks (targeted at particular participant of network) are possible but hard: just launch 100 nodes with node ids close to target node id. This nodes would fill node’s lowest k-buckets (which are probably empty). Then adversary may DDOS nodes from target’s k-buckets (it’s possible to deduce such list if network topology didn’t change much from node’s start) and all nodes neighbors would be adversary agents.

And here should be highlighted, that Kademlia’s structure implies that just launching nodes close to target is not enough to eclipse it. Node contains node lists in k-buckets (i-th bucket contains no more than k nodes with relative distance `2^i-1 < d < 2^i`). And new nodes would be added to corresponding k-buckets only if these buckets are not full already. Kademlia biases nodes that stay in lists for long time and recently shown alive.
And without getting some nodes down it’s impossible to eclipse node.

This attack is tricky and unlikely to happen in practice. Also [Addressing](#addressing) modification makes it harder.

**100500 attack** is an attack that launches amount of nodes significantly larger than the amount of nodes in the current P2P network, either in order to eclipse some nodes or to deny service by flooding the network. No problem would arise for old nodes (maybe only some network overhead) because they preserve their routes. But if new node would want to join network, it would get eclipsed (isolated in adversary subnet), because old honest nodes won’t add it to their buckets (because these buckets are already filled by other nodes) and it would be known only to adversaries.

It’s an open question, how to fight 100500 attacks. We’re going to make them practically infeasible with sophisticated ban system / adversary detection.

### Addressing

For Kademlia addresses we use so called HashNodeId. This makes impossible to assign yourself an arbitrary ID. This makes eclipse attacks only possible by 100500 attack.

### Routing data anti-forging

In Kademlia node requests some neighbors for routing data, and accepts first message received.
This way adversary may forge these replies, providing addresses of adversary group as closest nodes to given id. To overcome this issue we made node to wait for some period to gather as many replies as possible. Then node merges them and select K closest nodes from merges set. This way adversary would need to eclipse node to forge its list.

### Routing tables sharing

When node just joins network, it requests list of neighbors (set of nodes closest to ours).
We modified Kademlia in such a way that now some other nodes are also included in this list.
Now we just pick up some random nodes along with neighbors and return them.
This gives node additional knowledge to recover from case, when it’s surrounded by adversary nodes.

### Ban nodes

We introduced feature to ban nodes to Kademlia. We will use this to ban nodes when detect them to act maliciously.
