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
Local state, sometimes referred to as “mempool”, is stored in memory.
Mempool is used for data transfer and inclusion of transferred data into
blocks. The network protocol (built with standard [Inv/Req/Data
pattern](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Communication/Relay.hs))
is described in [Application-level
document](/protocols/csl-application-level/) with binary protocol
described in [Binary protocols document](/protocols/binary-protocols/).

Currently, everything is done to add hard-fork functionality via
software update to then perform a hard-fork, as described in research
section and soft-forks (or software updates) are fully implemented.

### Terminology

### Global State

### Local State

### Messages
