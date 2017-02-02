---
layout: default
title: Update Mechanism
permalink: /cardano/update-mechanism/
group: cardano
visible: true
---

# CSL Update Mechanism

## Research Overview

### Problem Definition

### Terminology

### Soft Forks

### Hard Forks

## Implementation Overview

Implementation of the update system lives in the
[Pos.Update](https://github.com/input-output-hk/cardano-sl/tree/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update)
family of modules. General approach to implementation is the same as
with other subsystems of CSL, such as Txp, Ssc and Delegation. Update
system has a global state, stored in the database. Global state can be
unambiguously derived from the information that is in the blockchain.
Local state, sometimes referred to as “mempool”, is stored in the memory.
Mempool is used for data transfer and inclusion of transferred data into
blocks. The network protocol (built with standard [Inv/Req/Data
pattern](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Communication/Relay.hs))
is described in [Application-level
document](/protocols/csl-application-level/) with a binary protocol
described in [Binary protocols document](/protocols/binary-protocols/).

Currently, everything is done to add hard-fork functionality via
software update to then perform a hard-fork, as described in research
section, and soft-forks (or software updates) are fully implemented.

### Fields Updatable with a Soft-Fork

# писать сюда

### Proposal Accumulation and Application

Proposals are stored in mempool or gathered from the blockchain in
order to figure out which proposal is adopted, and whether or not the current
node has to participate in voting. No matter whether a change in
proposal state comes from the network / mempool, or from loading
blockchain, it is stored in the `PollModifier` data structure and applied
appropriately.

### Core Types

Core types are mentioned in the Binary protocols document. Those types
reflect concepts from the research section in a straight-forward way.
Please refer to [core types
module](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Core/Types.hs)
for more information.

### Update Proposal Adoption

A very important part of implementation of the update mechanism is
the part that works with genesis blocks for epochs and applies updates.
This logic resides
[in this well-documented function](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Poll/Logic/Softfork.hs#L67).
Below we explain terminology realted to this process.

#### softforkResolutionThreshold

`softforkResolutionThreshold` is a predicate (seen as “threshold” in the
code) which, for an update proposal with version `v`, says that it is a
version of software ran by the network.

### Acceptable Proposal

A proposal is called “acceptable” if it is:

 + Correctly formed, including necessary cryptographic signatures.
 + Its version is greater or equal to the currently adopted proposal
   (see below).

### Confirmed Proposal

An acceptable proposal is called “confirmed” if it was voted for by the
majority of stake, but `softforkResolutionThreshold` predicate isn't yet
true for it.

### Adopted Proposal

A confirmed proposal is said to be “adopted” if its
`softforkResolutionThreshold` predicate is true for it. For each
blockchain state, there is exactly one adopted version. Blocks are
checked honoring the currently adopted version.
