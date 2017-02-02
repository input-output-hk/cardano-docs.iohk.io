---
layout: default
title: Update Mechanism
permalink: /cardano/update-mechanism/
group: cardano
visible: true
---

# CSL Update Mechanism

## Research Overview

To update the application securely, the majority of the stake must put
their signatures on it. To put an update proposal forward if there are
T% signatures of the stake. Then anyone can post their signature of
the update to the blockchain; if one epoch after the update collects
over 50% of the stake's signatures, the update is considered approved
and is to be applied by everyone.

There would, of course, be some notification for stakeholders to sign
an update and a button to sign and publish the signature on the
blockchain.

Soft fork proposes modification of blockchain consensus rules in such
a way that blocks with a new version are still compatible with old
version clients.

Hard Fork, however, doesn’t maintain backward compatibility with the previous version.

### Problem Definition
In the case of a soft fork update, 

### Terminology

### Soft Forks

### Hard Forks

## Implementation Overview

Implementation of the update system can be found in the
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

An
[`UpdateProposal`](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Core/Types.hs#L97-L108) contains
fields for changing some parameters used by Cardano SL (for instance, slot
duration). Specifically, `upBlockVersion` is used to signify that a proposal
performs such changes; if `upBlockVersion` is greater than the last used
block version, the changes from `upBlockVersionData` will be applied.

`upBlockVersionData` has the
type
[`BlockVersionData`](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Core/Types.hs#L131-L142). Its fields are described below:

  * `bvdScriptVersion` – script language version used to validate script
    transactions. If the proposal increases `upBlockVersion`, it must also
    increase `bvdScriptVersion` by 1 (and cannot leave it unchanged).

  * `bvdSlotDuration` – slot duration (in milliseconds).

  * `bvdMaxBlockSize` – block size limit (in bytes). A proposal can't
    increase the block size limit more than twofold compared to the previous
    limit.

The checks described above are done
in
[`verifyNextBVData`](https://github.com/input-output-hk/cardano-sl/blob/f77917a6a6a393bb3ef158500c147181fe21ed39/src/Pos/Update/Poll/Logic/Base.hs#L194-L221).

In addition, there are some fields that are unused right now but will be used
in the future. Their meaning is briefly described below:

  * `bvdMaxTxSize` – transaction size limit (in bytes).

  * `bvdMpcThd` – eligibility threshold for MPC.

  * `bvdHeavyDelThd` – threshold for heavyweight delegation.

  * `bvdUpdateVoteThd` – portion of total stake necessary to vote for or
    against an update.

  * `bvdUpdateProposalThd` – number of slots after which an update is
    implicitly approved (unless it has more negative votes than positive).

  * `bvdUpdateImplicit` – number of slots after which an update is implicitly
    approved (unless it has more negative votes than positive).

  * `bvdUpdateSoftforkThd` – portion of total stake such that if total stake
    of issuers of blocks with some block version is bigger than this portion,
    this block version is adopted.

### Proposal Accumulation

Proposals are stored in mempool or gathered from the blockchain in
order to figure out which proposal is adopted, and whether or not the current
node has to participate in voting. No matter whether a change in
proposal state comes from the network / mempool, or from loading
blockchain, it is stored in the `PollModifier` data structure and applied
appropriately.

### Updating Mempool

As nodes deserialize [payloads of update system
messages](/protocols/binary-protocols/#update-system), they modify
mempool as implemented
[here](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/MemState/Functions.hs#L40).

### Interaction With the Database

In order to verify update system data, we have to get this data from the
global state (database). To provide such interface, a [well-documented
set of typeclasses are
presented](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Poll/Class.hs).
It is important that implementation of those relies on functions found
in
[Pos.DB.GState.Update](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/DB/GState/Update.hs).

### Core Types

Core types are mentioned in the Binary protocols document. Those types
reflect concepts from the research section in a straight-forward way.
Please refer to the [core types
module](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Core/Types.hs)
for more information.

### Update Proposal Adoption

A very important part of implementation of the update mechanism is
the part that works with genesis blocks for epochs and applies updates.
This logic resides
[in this well-documented function](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Poll/Logic/Softfork.hs#L67).
Below we explain terminology related to this process.

#### `softforkResolutionThreshold` Predicate

`softforkResolutionThreshold` is a predicate (referred to as “threshold”
in the code) which, for an update proposal with version `v`, says that
it is a version of software ran by the network.

#### Acceptable Proposal

A proposal is called “acceptable” if it is:

 + Correctly formed, including necessary cryptographic signatures.
 + Its version is greater or equal to the currently adopted proposal
   (see below).

#### Confirmed Proposal

An acceptable proposal is called “confirmed” if it was voted for by the
majority of stake, but `softforkResolutionThreshold` predicate isn't yet
true for it.

#### Adopted Proposal

A confirmed proposal is said to be “adopted” if its
`softforkResolutionThreshold` predicate is true for it. For each
blockchain state, there is exactly one adopted version. Blocks are
checked honoring the currently adopted version.

### Download New Version

In the
[Pos.Update.Download](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Download.hs)
module, the following algorithms are implemeted. Downloaded updates are
applied using a tool called
[launcher](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/launcher/Main.hs)

#### Download Confirmed Update

To download confirmed update, we extract update hash from
`ConfirmedProposalState`. We extreact it depending on whether or not we're
using an installer on given platform. If update hash is extracted successfully,
the “Download Update by Hash” algorithm to download and save
confirmed update is invoked.

#### Download Update by Hash

To download update by hash, we loop through known update servers trying
to download update with given hash using `httpLBS` from HTTP. Simple. In
the end, we will either have the update completely downloaded or server list
exhausted and an error reported.
